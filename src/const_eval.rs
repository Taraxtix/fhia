use std::{num::NonZero, range::Range};

use inkwell::values::BasicValueEnum;

use crate::{
    Spanned,
    parser::expr::{DeclKind, Expr, Ty, UnaryOpKind},
    program::{
        Program,
        diagnostics::{Diagnostic, ErrorCode, Reportable},
        env::Env,
    },
    topo_order::topo_order,
    typer::Typer,
};

impl<'src> Program<'src, Typer<'src>> {
    pub fn const_eval(mut self) -> Self {
        let mut diagnostics = Vec::new();

        let exprs = self.state.exprs;
        let env = &mut self.state.env;

        let Ok((order, decl_map)) = topo_order(&exprs).inspect_err(|diag| {
            diagnostics.push(diag.clone());
            diagnostics.report(self.source, &self.args.input);
        })
        else
        {
            unreachable!()
        };

        for name in order
        {
            let Spanned(expr @ Expr::Declaration { kind, .. }, span) = decl_map
                .get(name)
                .expect("topo_order should not provide non-existant name")
            else
            {
                unreachable!()
            };

            let ptr_size = (self
                .target_machine
                .get_target_data()
                .get_pointer_byte_size(None)
                * 8) as usize;

            expr.const_value(span, env, *kind, &mut diagnostics, ptr_size);
        }

        diagnostics.report(self.source, &self.args.input);

        Self {
            args:           self.args,
            source:         self.source,
            target_machine: self.target_machine,
            state:          Typer {
                exprs,
                env: self.state.env,
            },
        }
    }
}

impl<'src> Expr<'src> {
    fn handle_error(
        span: Range<usize>,
        kind: DeclKind,
        diagnostics: &mut Vec<Diagnostic>,
    ) -> Option<ConstValue> {
        if kind == DeclKind::Const
        {
            diagnostics.push(
                Diagnostic::error(ErrorCode::NotConstInConst)
                    .with_context_label(span, "expression was expected to be const"),
            );
        }
        None
    }

    pub fn const_value<'a>(
        &'a self,
        span: &Range<usize>,
        env: &'a mut Env<'src>,
        decl_kind: DeclKind,
        diagnostics: &mut Vec<Diagnostic>,
        ptr_size: usize,
    ) -> Option<ConstValue> {
        match self
        {
            Self::Declaration { name, expr, .. } =>
            {
                let Spanned(expr, span) = expr.as_ref();
                let value = expr.const_value(span, env, decl_kind, diagnostics, ptr_size)?;
                env.declare_const(name, value);
                Some(value)
            },
            Self::IntLit { ty, value, .. } => match ty
            {
                Ty::Isize | Ty::Int { signed: true, .. } =>
                {
                    Some(ConstValue::Int((*value).cast_signed()))
                },
                Ty::Usize | Ty::Int { signed: false, .. } => Some(ConstValue::Uint(*value)),
                Ty::F32 | Ty::F64 | Ty::Unit | Ty::Unknown | Ty::IntLit =>
                {
                    unreachable!("Should not be able to pass the typer")
                },
            },
            Self::F64(val) => Some(ConstValue::Float(*val)),
            Self::Cast(ty, s_expr) =>
            {
                let Spanned(expr, span) = s_expr.as_ref();
                Some(
                    expr.const_value(span, env, decl_kind, diagnostics, ptr_size)
                        .or_else(|| Self::handle_error(*span, decl_kind, diagnostics))?
                        .cast_to(s_expr.ty(), *ty, 64),
                )
            },
            Self::Ident { name, .. } => env
                .lookup_const(name)
                .copied()
                .or_else(|| Self::handle_error(*span, decl_kind, diagnostics)),
            Self::Unary { .. } =>
            {
                self.const_eval_unary(span, env, decl_kind, diagnostics, ptr_size)
            },
        }
    }

    fn const_eval_unary(
        &self,
        span: &Range<usize>,
        env: &mut Env<'src>,
        decl_kind: DeclKind,
        diagnostics: &mut Vec<Diagnostic>,
        ptr_size: usize,
    ) -> Option<ConstValue> {
        let Expr::Unary { kind, operand } = self
        else
        {
            unreachable!("Ensured by caller")
        };
        let Spanned(operand, expr_span) = operand.as_ref();
        let const_operand = operand
            .const_value(span, env, DeclKind::Const, diagnostics, ptr_size)
            .or_else(|| Self::handle_error(*expr_span, decl_kind, diagnostics))?;
        Some(match kind
        {
            UnaryOpKind::Neg => match const_operand
            {
                ConstValue::Uint(val) => ConstValue::Int(val.wrapping_neg().cast_signed()),
                ConstValue::Int(val) => ConstValue::Int(val.wrapping_neg()),
                ConstValue::Float(val) => ConstValue::Float(-val),
            },
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ConstValue {
    Uint(u128),
    Int(i128),
    Float(f64),
}

impl ConstValue {
    #[expect(
        clippy::similar_names,
        reason = "Yes i128 is similar to u128. What else would you name them?"
    )]
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        clippy::cast_sign_loss,
        clippy::cast_precision_loss,
        reason = "This is intentional. We want to allow all possible casts, even if they may \
                  cause truncation, wrapping, sign loss, or precision loss. The behavior of these \
                  casts should be the same as in Rust."
    )]
    pub(crate) fn cast_to(self, from: Ty, to: Ty, ptr_size: usize) -> Self {
        match from
        {
            Ty::Unit | Ty::Unknown | Ty::IntLit =>
            {
                unreachable!("Should not pass type_check")
            },
            _ =>
            {},
        }
        let (as_i128, as_u128, as_f64) = match self
        {
            Self::Int(v) => (v, v as u128, v as f64),
            Self::Uint(v) => (v as i128, v, v as f64),
            Self::Float(v) => (v as i128, v as i128 as u128, v),
        };
        match to
        {
            Ty::Int {
                signed: true,
                width,
            } =>
            {
                let shift = 128 - u32::from(width);
                Self::Int((as_i128 << shift) >> shift)
            },
            Ty::Int {
                signed: false,
                width,
            } =>
            {
                let shift = 128 - u32::from(width);
                Self::Uint((as_u128 << shift) >> shift)
            },
            Ty::F32 | Ty::F64 => Self::Float(as_f64),
            Ty::Usize => self.cast_to(
                from,
                Ty::Int {
                    signed: false,
                    width:  unsafe { NonZero::new(ptr_size as u32).unwrap_unchecked() },
                },
                ptr_size,
            ),
            Ty::Isize => self.cast_to(
                from,
                Ty::Int {
                    signed: true,
                    width:  unsafe { NonZero::new(ptr_size as u32).unwrap_unchecked() },
                },
                ptr_size,
            ),
            Ty::Unit | Ty::Unknown | Ty::IntLit =>
            {
                unreachable!("Should not pass type_check")
            },
        }
    }

    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_sign_loss,
        reason = "We are cutting the 128bits in half discarding sign to form llvm's integers."
    )]
    pub fn to_basic_value(self, llvm_ty: inkwell::types::BasicTypeEnum<'_>) -> BasicValueEnum<'_> {
        match (self, llvm_ty)
        {
            (Self::Int(v), inkwell::types::BasicTypeEnum::IntType(int_ty)) =>
            {
                BasicValueEnum::IntValue(
                    int_ty.const_int_arbitrary_precision(&[v as u64, (v >> 64) as u64]),
                )
            },
            (Self::Uint(v), inkwell::types::BasicTypeEnum::IntType(int_ty)) =>
            {
                BasicValueEnum::IntValue(
                    int_ty.const_int_arbitrary_precision(&[v as u64, (v >> 64) as u64]),
                )
            },
            (Self::Float(v), inkwell::types::BasicTypeEnum::FloatType(float_ty)) =>
            {
                BasicValueEnum::FloatValue(float_ty.const_float(v))
            },
            _ => unreachable!("Type mismatch when converting const value to basic value"),
        }
    }
}
