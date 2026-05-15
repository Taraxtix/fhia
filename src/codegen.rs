use std::{
    cmp::Ordering,
    collections::{HashMap, VecDeque},
    fs,
    mem::MaybeUninit,
    path::Path,
    process::Command,
};

use inkwell::{
    OptimizationLevel,
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    targets::{
        CodeModel,
        FileType,
        InitializationConfig,
        RelocMode,
        Target,
        TargetData,
        TargetMachine,
    },
    types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum, FloatType, IntType},
    values::{BasicValue, BasicValueEnum, PointerValue},
};

use crate::{
    Args,
    Spanned,
    parser::expr::{Expr, Ty},
    typer::TypedOutput,
    util::topo_order,
};

struct Codegen<'ctx, 'src> {
    context:     &'ctx Context,
    module:      Module<'ctx>,
    builder:     Builder<'ctx>,
    machine:     TargetMachine,
    target_data: TargetData,
    vars:        HashMap<&'src str, (PointerValue<'ctx>, AnyTypeEnum<'ctx>)>,
}

impl TypedOutput<'_> {
    pub fn compile(self, args: &Args, source: &str) {
        let context = Context::create();
        Codegen::new(&context).compile(self.exprs, args, source);
    }
}

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx,
{
    pub fn new(context: &'ctx Context) -> Self {
        Target::initialize_all(&InitializationConfig::default());

        let module = context.create_module("main"); //TODO: Have a module system
        let builder = context.create_builder();

        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).expect("Host target not found");
        let cpu = TargetMachine::get_host_cpu_name();
        let features = TargetMachine::get_host_cpu_features();
        let machine = target
            .create_target_machine(
                &triple,
                cpu.to_str().expect("Failed to transform LLVMString to str"),
                features
                    .to_str()
                    .expect("Failed to transform LLVMString to str"),
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .expect("Failed to create target machine");
        let target_data = machine.get_target_data();

        Codegen {
            context,
            module,
            builder,
            machine,
            target_data,
            vars: HashMap::new(),
        }
    }

    pub fn compile(mut self, exprs: VecDeque<Spanned<Expr<'src>>>, args: &Args, source: &str) {
        let main_entry = self.alloc_top_level_decls(&exprs);
        self.builder.position_at_end(main_entry);
        // self.alloc_decls(&exprs);

        let mut dep_map: HashMap<&'src str, Vec<&'src str>> = HashMap::new();
        let mut decls: HashMap<&'src str, Spanned<Expr<'src>>> = HashMap::new();
        // Pass 1: Populate the HashMaps
        for expr in exprs
        {
            let (name, deps) = match &expr
            {
                Spanned(
                    Expr::Declaration {
                        name, expr: body, ..
                    },
                    _,
                ) =>
                {
                    let name = *name;
                    let mut deps = body.0.deps();
                    deps.retain(|&d| self.vars.contains_key(d) && d != name);
                    (name, deps)
                },
                _ => unreachable!(),
            };
            dep_map.insert(name, deps);
            decls.insert(name, expr);
        }

        // Pass 2: Compute topological order
        let order = topo_order(&dep_map).unwrap_or_else(|cycle| {
            let mut diag = crate::diagnostics::Diagnostic::error(
                crate::diagnostics::ErrorCode::CyclicDeclaration,
            );
            for name in &cycle
            {
                if let Some(Spanned(_, span)) = decls.get(name)
                {
                    diag =
                        diag.with_main_label(span.clone(), format!("'{name}' is part of a cycle"));
                }
            }
            crate::diagnostics::parser::render(&diag, source, &args.input);
            std::process::exit(1);
        });

        // Pass 3: Generate the declarations
        let mut main_body = None;
        for name in order
        {
            let Spanned(Expr::Declaration { expr: body, .. }, _) = decls.remove(name).unwrap()
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
        self.machine
            .write_to_file(&self.module, FileType::Object, path)
            .expect("Failed to emit object file");
    }

    fn alloc_top_level_decls(&mut self, exprs: &VecDeque<Spanned<Expr<'src>>>) -> BasicBlock<'ctx> {
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
                    global.set_initializer(&llvm_ty.const_zero());
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
            #[allow(clippy::cast_sign_loss)]
            Expr::I64(value) => self.context.i64_type().const_int(value as u64, true).into(),
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
        }
    }

    fn ty_to_llvm(&self, ty: Ty) -> BasicTypeEnum<'ctx> {
        match ty
        {
            Ty::I8 | Ty::U8 => self.context.i8_type().into(),
            Ty::I16 | Ty::U16 => self.context.i16_type().into(),
            Ty::I32 | Ty::U32 => self.context.i32_type().into(),
            Ty::I64 | Ty::U64 => self.context.i64_type().into(),
            Ty::I128 | Ty::U128 => self.context.i128_type().into(),
            Ty::F32 => self.context.f32_type().into(),
            Ty::F64 => self.context.f64_type().into(),
            Ty::Isize | Ty::Usize => self
                .context
                .ptr_sized_int_type(&self.target_data, None)
                .into(),
            Ty::Unit => unreachable!("Don't call ty_to_llvm on `()`"),
            Ty::Unknown => unreachable!("`?` Should not exist anymore at this stage"),
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
