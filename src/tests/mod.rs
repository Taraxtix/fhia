use std::num::NonZero;
use std::sync::OnceLock;

use inkwell::targets::{InitializationConfig, Target, TargetMachine, TargetTriple};

use crate::parser::expr::Ty;

mod const_eval;
mod lexer;
mod parser;
mod typer;

fn int_ty(signed: bool, width: u32) -> Ty {
    Ty::Int {
        signed,
        width: unsafe { NonZero::new_unchecked(width) },
    }
}

static LLVM_INIT: OnceLock<()> = OnceLock::new();

fn init_llvm() {
    LLVM_INIT.get_or_init(|| {
        Target::initialize_all(&InitializationConfig::default());
    });
}

fn get_default_target_machine() -> TargetMachine {
    init_llvm();
    let target_triple = TargetTriple::create("x86_64-unknown-linux-gnu");
    let target = Target::from_triple(&target_triple).expect("Failed to get target from triple");
    target
        .create_target_machine(
            &target_triple,
            "generic",
            "",
            inkwell::OptimizationLevel::Default,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
        .expect("Failed to create default target machine")
}
