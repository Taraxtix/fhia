use std::{
    cmp::Ordering,
    collections::{HashMap, VecDeque},
    fs,
    mem::MaybeUninit,
    path::Path,
    process::Command,
};

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    targets::{FileType, TargetData},
    types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum, FloatType, IntType},
    values::{BasicValue, BasicValueEnum, PointerValue},
};

use crate::{
    Spanned,
    parser::expr::{Expr, Ty},
    program::Program,
    topo_order::topo_order,
    typer::Typer,
};

struct Codegen<'ctx, 'src> {
    context:     &'ctx Context,
    module:      Module<'ctx>,
    builder:     Builder<'ctx>,
    target_data: TargetData,
    vars:        HashMap<&'src str, (PointerValue<'ctx>, AnyTypeEnum<'ctx>)>,
    program:     Program<'src, Typer<'src>>,
}

impl<'src> Program<'src, Typer<'src>> {
    pub fn compile(self) {
        let context = Context::create();
        Codegen::new(&context, self).compile();
    }
}

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx,
{
    pub fn new(context: &'ctx Context, program: Program<'src, Typer<'src>>) -> Self {
        let module = context.create_module("main"); //TODO: Have a module system
        let builder = context.create_builder();
        let target_data = program.target_machine.get_target_data();

        Codegen {
            context,
            module,
            builder,
            target_data,
            vars: HashMap::new(),
            program,
        }
    }

    pub fn compile(mut self) {
        let main_entry = self.alloc_top_level_decls();
        self.builder.position_at_end(main_entry);
        // self.alloc_decls(&exprs);

        let exprs = &self.program.state.exprs;
        let (order, mut decl_map) =
            topo_order(exprs).expect("Cycle should have been caught by typer");

        // Generate the declarations in topological order
        let mut main_body = None;
        for name in order
        {
            let Spanned(Expr::Declaration { expr: body, .. }, _) = decl_map.remove(name).unwrap()
            else
            {
                unreachable!()
            };
            if name == "main"
            {
                main_body = Some(*body);
            }
            else
            {
                self.gen_decl(name, *body);
            }
        }

        let ret_val = self.gen_expr(main_body.expect("no main declaration"));
        self.builder
            .build_return(Some(&ret_val))
            .expect("build_return failed");

        self.module.verify().expect("Module was not correct");

        let args = &self.program.args;
        if args.llvm_ir
        {
            println!(
                "-------------------------------------------------\n{}",
                self.module.print_to_string().to_string()
            );
        }

        let obj_path = Path::new(&args.output).with_extension("o");
        self.emit_object(&obj_path);
        link(&obj_path, &args.output);
        fs::remove_file(&obj_path).expect("Failed to remove temporary object file");
    }

    fn emit_object(&self, path: &Path) {
        self.program
            .target_machine
            .write_to_file(&self.module, FileType::Object, path)
            .expect("Failed to emit object file");
    }

    fn alloc_top_level_decls(&mut self) -> BasicBlock<'ctx> {
        let exprs = &self.program.state.exprs;
        let mut main_entry: MaybeUninit<BasicBlock> = MaybeUninit::uninit();

        for Spanned(expr, _) in exprs
        {
            if let Expr::Declaration { name, ty, .. } = expr
            {
                let (alloca, llvm_ty) = if *name == "main"
                {
                    let fn_type = self.ty_to_llvm(*ty).fn_type(&[], false);
                    // TODO: Replace by `if let Arrow(..) = *ty {use straight ty_to_llvm} else {use ty_to_llvm.fn_type(&[], false)}`
                    let main_fn = self.module.add_function("main", fn_type, None);
                    main_entry =
                        MaybeUninit::new(self.context.append_basic_block(main_fn, "entry"));
                    (main_fn.as_global_value(), fn_type.as_any_type_enum())
                }
                else
                {
                    let llvm_ty = self.ty_to_llvm(*ty);

                    let global = self.module.add_global(llvm_ty, None, name);
                    let initializer = self.program.state.env.lookup_const(name).map_or_else(
                        || match llvm_ty
                        {
                            BasicTypeEnum::IntType(ty) => ty.get_poison().into(),
                            BasicTypeEnum::FloatType(ty) => ty.get_poison().into(),
                            BasicTypeEnum::ArrayType(ty) => ty.get_poison().into(),
                            BasicTypeEnum::PointerType(ty) => ty.get_poison().into(),
                            BasicTypeEnum::StructType(ty) => ty.get_poison().into(),
                            BasicTypeEnum::VectorType(ty) => ty.get_poison().into(),
                            BasicTypeEnum::ScalableVectorType(ty) => ty.get_poison().into(),
                        },
                        |const_value| {
                            global.set_constant(true);
                            const_value.to_basic_value(llvm_ty)
                        },
                    );
                    global.set_initializer(&initializer);
                    (global, llvm_ty.as_any_type_enum())
                };

                self.vars.insert(name, (alloca.as_pointer_value(), llvm_ty));
            }
        }
        unsafe { main_entry.assume_init() }
    }

    fn _alloc_decls(&mut self, exprs: &VecDeque<Spanned<Expr<'src>>>) {
        for Spanned(expr, _) in exprs
        {
            if let Expr::Declaration { name, ty, .. } = expr
            {
                let llvm_ty = self.ty_to_llvm(*ty);
                let alloca = self
                    .builder
                    .build_alloca(llvm_ty, name)
                    .expect("build_alloca failed");
                self.vars.insert(name, (alloca, llvm_ty.as_any_type_enum()));
            }
        }
    }

    fn gen_decl(&mut self, name: &'src str, body: Spanned<Expr<'src>>) {
        if self.program.state.env.lookup_const(name).is_some()
        {
            // Const declarations are inlined, no codegen needed
            return;
        }
        let (alloca, ty) = self.vars[name];
        let value = self.gen_expr(body);
        let align = self.target_data.get_abi_alignment(&ty);
        self.builder
            .build_store(alloca, value)
            .expect("build_store failed")
            .set_alignment(align)
            .expect("set_alignment failed");
    }

    fn gen_expr(&mut self, expr: Spanned<Expr<'src>>) -> inkwell::values::BasicValueEnum<'ctx> {
        let Spanned(expr, _span) = expr;
        match expr
        {
            Expr::Declaration { .. } => unreachable!("declarations are handled by gen_decl"),
            #[allow(clippy::cast_possible_truncation)]
            Expr::IntLit { ty, value, .. } => self
                .int_type(ty)
                .const_int_arbitrary_precision(&[value as u64, (value >> 64) as u64])
                .into(),
            Expr::F64(value) => self.context.f64_type().const_float(value).into(),
            Expr::Cast(ty, expr) => self.gen_cast(ty, *expr),
            Expr::Ident { name, .. } =>
            {
                let (ptr, ty) = *self.vars.get(name).expect("undefined var");
                let align = self.target_data.get_abi_alignment(&ty);
                let basic_ty = BasicTypeEnum::try_from(ty).expect("Variable type must be basic");
                let load = self
                    .builder
                    .build_load(basic_ty, ptr, name)
                    .expect("build_load failed");
                match load
                {
                    BasicValueEnum::IntValue(v) => v
                        .as_instruction_value()
                        .unwrap()
                        .set_alignment(align)
                        .unwrap(),
                    BasicValueEnum::FloatValue(v) => v
                        .as_instruction_value()
                        .unwrap()
                        .set_alignment(align)
                        .unwrap(),
                    _ => unreachable!(),
                }
                load
            },
            Expr::Unary { kind, .. } => todo!("Codegen unary operator '{kind}'"),
        }
    }

    fn ty_to_llvm(&self, ty: Ty) -> BasicTypeEnum<'ctx> {
        match ty
        {
            Ty::Int { width, .. } => self
                .context
                .custom_width_int_type(width)
                .expect("failed to create Int type")
                .into(),
            Ty::F32 => self.context.f32_type().into(),
            Ty::F64 => self.context.f64_type().into(),
            Ty::Isize | Ty::Usize => self
                .context
                .ptr_sized_int_type(&self.target_data, None)
                .into(),
            Ty::Unit => unreachable!("Don't call ty_to_llvm on `()`"),
            Ty::Unknown => unreachable!("`?` Should not exist anymore at this stage"),
            Ty::IntLit => unreachable!("`{{int}}` should not exist anymore at this stage"),
        }
    }

    fn gen_cast(&mut self, dst: Ty, expr: Spanned<Expr<'src>>) -> BasicValueEnum<'ctx> {
        let src = expr.ty();

        let value = self.gen_expr(expr);

        match (value, dst)
        {
            (..) if src == dst => value,
            (BasicValueEnum::IntValue(v), dst) if dst.is_llvm_int() =>
            {
                let dst = self.int_type(dst);
                let src_bits = v.get_type().get_bit_width();
                let dst_bits = dst.get_bit_width();
                match src_bits.cmp(&dst_bits)
                {
                    Ordering::Less if src.is_signed() => self
                        .builder
                        .build_int_s_extend(v, dst, "sext")
                        .unwrap()
                        .into(),
                    Ordering::Less => self
                        .builder
                        .build_int_z_extend(v, dst, "zext")
                        .unwrap()
                        .into(),
                    Ordering::Greater => self
                        .builder
                        .build_int_truncate(v, dst, "trunc")
                        .unwrap()
                        .into(),
                    // same width, different signedness: no instruction needed
                    Ordering::Equal => BasicValueEnum::IntValue(v),
                }
            },
            (BasicValueEnum::FloatValue(v), Ty::F32) => self
                .builder
                .build_float_trunc(v, self.context.f32_type(), "ftrunc")
                .unwrap()
                .into(),
            (BasicValueEnum::FloatValue(v), Ty::F64) => self
                .builder
                .build_float_ext(v, self.context.f64_type(), "fext")
                .unwrap()
                .into(),
            (BasicValueEnum::IntValue(v), Ty::F32 | Ty::F64) =>
            {
                let dst = self.float_type(dst);
                if src.is_signed()
                {
                    self.builder
                        .build_signed_int_to_float(v, dst, "sitofp")
                        .unwrap()
                        .into()
                }
                else
                {
                    self.builder
                        .build_unsigned_int_to_float(v, dst, "uitofp")
                        .unwrap()
                        .into()
                }
            },
            (BasicValueEnum::FloatValue(v), dst_ty) =>
            {
                let dst = self.int_type(dst_ty);
                if dst_ty.is_signed()
                {
                    self.builder
                        .build_float_to_signed_int(v, dst, "fptosi")
                        .unwrap()
                        .into()
                }
                else
                {
                    self.builder
                        .build_float_to_unsigned_int(v, dst, "fptoui")
                        .unwrap()
                        .into()
                }
            },
            _ => unreachable!("unhandled cast {src:?} → {dst:?}"),
        }
    }

    fn int_type(&self, ty: Ty) -> IntType<'ctx> { self.ty_to_llvm(ty).into_int_type() }

    fn float_type(&self, ty: Ty) -> FloatType<'ctx> { self.ty_to_llvm(ty).into_float_type() }
}

fn link(obj: &Path, output: &str) {
    let status = Command::new("cc")
        .arg(obj)
        .arg("-o")
        .arg(output)
        .status()
        .expect("Failed to invoke linker (is `cc` installed?)");
    if !status.success()
    {
        eprintln!("error: linker exited with non-zero status");
        std::process::exit(1);
    }
}
