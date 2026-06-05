use std::{collections::VecDeque, num::NonZero};

use crate::{
    Spanned,
    parser::{
        Parser,
        expr::{Expr, Ty},
    },
    program::{
        Program,
        diagnostics::{Diagnostic, ErrorCode, Reportable},
        env::Env,
    },
};

pub struct Typer<'src> {
    pub exprs: VecDeque<Spanned<Expr<'src>>>,
    pub env:   Env<'src>,
}

impl<'src> Program<'src, Parser<'src>> {
    pub fn type_check(self) -> Program<'src, Typer<'src>> {
        let typer = Program {
            args:           self.args,
            target_machine: self.target_machine,
            source:         self.source,
            state:          Typer {
                exprs: VecDeque::from(self.state),
                env:   Env::new(),
            },
        }
        .prepopulate_decls()
        .type_check();
        if typer.args.typer
        {
            println!("-------------------------------------------------");
            for expr in &typer.state.exprs
            {
                println!("{}", expr.0);
            }
        }
        typer
    }
}

impl<'src> Program<'src, Typer<'src>> {
    fn prepopulate_decls(mut self) -> Self {
        let mut diagnostics = Vec::new();

        let exprs = &self.state.exprs;
        let env = &mut self.state.env;
        for expr in exprs
        {
            if let Spanned(Expr::Declaration { name, ty, .. }, span) = expr
            {
                if env.is_in_current_scope(name)
                {
                    diagnostics.push(
                        Diagnostic::error(ErrorCode::DuplicateDeclaration)
                            .with_main_label(*span, format!("'{name}' redeclared here")),
                    );
                }
                else
                {
                    if *name == "main"
                    {
                        match ty
                        {
                            Ty::Int { .. } | Ty::Usize | Ty::Isize => (),
                            ty => diagnostics.push(
                                Diagnostic::error(ErrorCode::IncorrectMainType).with_main_label(
                                    *span,
                                    format!(
                                        "expected main type to return an integer type, but \
                                         returns {ty} instead"
                                    ),
                                ),
                            ),
                        }
                    }
                    env.declare(name, *ty);
                }
            }
        }

        if env.lookup("main").is_none()
        {
            diagnostics.push(Diagnostic::error(ErrorCode::MissingMain));
        }

        diagnostics.report(self.source, &self.args.input);
        self
    }

    fn type_check(mut self) -> Self {
        let mut diagnostics = Vec::new();

        let exprs = self.state.exprs;
        let env = &mut self.state.env;
        let exprs = exprs
            .into_iter()
            .map(|s_expr| {
                let (expr, diags) = s_expr.type_check(env);
                diagnostics.extend(diags);
                expr
            })
            .collect::<VecDeque<_>>();

        diagnostics.report(self.source, &self.args.input);
        Self {
            args:           self.args,
            target_machine: self.target_machine,
            source:         self.source,
            state:          Typer {
                exprs,
                env: self.state.env,
            },
        }
    }
}

const fn int_lit_fits(value: u128, ty: Ty) -> bool {
    match ty
    {
        Ty::Int {
            signed: false,
            width,
        } => width.get() == 128 || value < (1u128 << width.get()),
        Ty::Int {
            signed: true,
            width,
        } => value < (1u128 << (width.get() - 1)),
        Ty::Isize | Ty::Usize => true,
        _ => false,
    }
}

impl<'src> Spanned<Expr<'src>> {
    #[must_use]
    pub const fn ty(&self) -> Ty {
        match self
        {
            Spanned(Expr::F64(_), _) => Ty::F64,
            Spanned(
                Expr::IntLit { ty, .. }
                | Expr::Ident { ty, .. }
                | Expr::Declaration { ty, .. }
                | Expr::Cast(ty, _),
                _,
            ) => *ty,
            Spanned(Expr::Unary { operand, .. }, _) => operand.ty(),
        }
    }

    fn type_check(self, env: &mut Env<'src>) -> (Self, Vec<Diagnostic>) {
        let mut diagnostics = Vec::new();
        let expr = match self
        {
            Spanned(Expr::IntLit { .. } | Expr::F64(_), _) => self,
            Spanned(Expr::Ident { .. }, ..) => self.type_check_ident(env, &mut diagnostics),
            Spanned(Expr::Cast(..), ..) => self.type_check_cast(env, &mut diagnostics),
            Spanned(Expr::Declaration { .. }, ..) => self.type_check_decla(env, &mut diagnostics),
            Spanned(Expr::Unary { kind, .. }, _) =>
            {
                todo!("Type check unary operator '{kind}'")
            },
        };
        (expr, diagnostics)
    }

    fn type_check_ident(self, env: &Env<'src>, diagnostics: &mut Vec<Diagnostic>) -> Self {
        let Spanned(Expr::Ident { name, ty }, span) = self
        else
        {
            unreachable!()
        };
        match env.lookup(name)
        {
            None =>
            {
                diagnostics.push(
                    Diagnostic::error(ErrorCode::UndefinedVariable)
                        .with_main_label(span, format!("'{name}' not defined")),
                );
                Spanned(Expr::Ident { name, ty }, span)
            },
            Some(env_ty) =>
            {
                if ty != Ty::Unknown && ty != env_ty
                {
                    diagnostics.push(
                        Diagnostic::error(ErrorCode::TypeAscriptionMismatch).with_main_label(
                            span,
                            format!("ascribed as '{ty}' here but '{name}' has type '{env_ty}'"),
                        ),
                    );
                }
                Spanned(Expr::Ident { name, ty: env_ty }, span)
            },
        }
    }

    fn type_check_cast(self, env: &mut Env<'src>, diagnostics: &mut Vec<Diagnostic>) -> Self {
        let Spanned(Expr::Cast(ty, expr), span) = self
        else
        {
            unreachable!()
        };
        let (typed_expr, expr_diagnostics) = expr.type_check(env);
        diagnostics.extend(expr_diagnostics);
        let typed_expr = match typed_expr
        {
            Spanned(
                Expr::IntLit {
                    value,
                    ty: Ty::IntLit,
                },
                expr_span,
            ) => Spanned(
                Expr::IntLit {
                    ty: Ty::Int {
                        signed: false,
                        width:  unsafe { NonZero::new_unchecked(128) },
                    },
                    value,
                },
                expr_span,
            ),
            other => other,
        };
        if typed_expr.ty() == Ty::Unit
        {
            diagnostics.push(
                Diagnostic::error(ErrorCode::InvalidCastOperand)
                    .with_main_label(typed_expr.1, "cannot cast a `()` value"),
            );
        }
        Spanned(Expr::Cast(ty, Box::new(typed_expr)), span)
    }

    fn type_check_decla(self, env: &mut Env<'src>, diagnostics: &mut Vec<Diagnostic>) -> Self {
        let Spanned(
            Expr::Declaration {
                ty,
                expr,
                name,
                kind,
            },
            span,
        ) = self
        else
        {
            unreachable!()
        };
        env.push_scope();
        let (typed_expr, expr_diagnostics) = expr.type_check(env);
        env.pop_scope();
        diagnostics.extend(expr_diagnostics);
        let typed_expr = match typed_expr
        {
            Spanned(
                Expr::IntLit {
                    value,
                    ty: Ty::IntLit,
                },
                expr_span,
            ) if ty.is_llvm_int() =>
            {
                if !int_lit_fits(value, ty)
                {
                    diagnostics.push(
                        Diagnostic::error(ErrorCode::IntLiteralOutOfRange)
                            .with_main_label(expr_span, format!("{value} does not fit in '{ty}'")),
                    );
                }
                Spanned(Expr::IntLit { ty, value }, expr_span)
            },
            typed_expr =>
            {
                if ty != typed_expr.ty()
                {
                    let expr_ty = typed_expr.ty();
                    diagnostics.push(
                        Diagnostic::error(ErrorCode::TypeMismatch)
                            .with_main_label(typed_expr.1, format!("found '{expr_ty}' here"))
                            .with_context_label(span, format!("expected '{ty}'")),
                    );
                }
                typed_expr
            },
        };
        Spanned(
            Expr::Declaration {
                ty,
                expr: Box::new(typed_expr),
                name,
                kind,
            },
            span,
        )
    }
}
