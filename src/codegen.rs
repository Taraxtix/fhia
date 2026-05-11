use std::{
    cmp::Ordering,
    collections::{HashMap, VecDeque},
    fs,
    path::Path,
    process::Command,
};

use inkwell::{
    OptimizationLevel,
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
    types::{BasicTypeEnum, FloatType, IntType},
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
    vars:        HashMap<&'src str, (PointerValue<'ctx>, BasicTypeEnum<'ctx>)>,
}

impl TypedOutput<'_> {
    pub fn compile(self, args: &Args) {
        let context = Context::create();
        Codegen::new(&context).compile(self.exprs, args);
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

    pub fn compile(mut self, exprs: VecDeque<Spanned<Expr<'src>>>, args: &Args) {
        let fn_type = self.context.i32_type().fn_type(&[], false);
        let main_fn = self.module.add_function("main", fn_type, None);
        let main_entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(main_entry);

        self.alloc_decls(&exprs);

        let mut dep_map: HashMap<&'src str, Vec<&'src str>> = HashMap::new();
        let mut bodies: HashMap<&'src str, Box<Spanned<Expr<'src>>>> = HashMap::new();
        for Spanned(expr, _) in exprs
        {
            if let Expr::Declaration {
                name, expr: body, ..
            } = expr
            {
                let mut deps = body.0.deps();
                deps.sort_unstable();
                deps.dedup();
                deps.retain(|&d| self.vars.contains_key(d) && d != name);
                dep_map.insert(name, deps);
                bodies.insert(name, body);
            }
        }

        for name in topo_order(&dep_map)
        {
            self.gen_decl(name, *bodies.remove(name).unwrap());
        }

        self.builder
            .build_return(Some(&self.context.i32_type().const_int(0, false)))
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

    fn alloc_decls(&mut self, exprs: &VecDeque<Spanned<Expr<'src>>>) {
        for Spanned(expr, _) in exprs
        {
            if let Expr::Declaration { name, ty, .. } = expr
            {
                let llvm_ty = self.ty_to_llvm(*ty);
                let alloca = self
                    .builder
                    .build_alloca(llvm_ty, name)
                    .expect("build_alloca failed");
                self.vars.insert(name, (alloca, llvm_ty));
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
                let load = self
                    .builder
                    .build_load(ty, ptr, name)
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
