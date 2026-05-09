use std::collections::HashMap;

use crate::{
    Spanned,
    diagnostics::{Diagnostic, ErrorCode},
    parser::expr::{Expr, Ty},
};

pub struct Env<'src> {
    scopes: Vec<HashMap<&'src str, Ty>>,
}

impl<'src> Env<'src> {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    fn lookup(&self, name: &str) -> Option<Ty> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).copied())
    }

    fn declare(&mut self, name: &'src str, ty: Ty) {
        self.scopes.last_mut().unwrap().insert(name, ty);
    }

    fn is_in_current_scope(&self, name: &str) -> bool {
        self.scopes
            .last()
            .is_some_and(|scope| scope.contains_key(name))
    }

    pub fn push_scope(&mut self) { self.scopes.push(HashMap::new()); }

    /// # Panics
    /// - Panics if you attempt to pop the root scope
    pub fn pop_scope(&mut self) {
        assert!(self.scopes.len() > 1, "attempted to pop the root scope");
        self.scopes.pop();
    }
}

pub struct TypedOutput<'a> {
    pub exprs:       Vec<Spanned<Expr<'a>>>,
    pub diagnostics: Vec<Diagnostic>,
}

impl<'a> TypedOutput<'a> {
    #[must_use]
    pub fn type_check(exprs: Vec<Spanned<Expr<'a>>>) -> Self {
        let mut diagnostics: Vec<Diagnostic> = Vec::new();
        let mut env = Env::new();

        // Pass 1: pre-populate all declarations so every RHS sees the full scope
        for expr in &exprs
        {
            if let Spanned(Expr::Declaration { name, ty, .. }, span) = expr
            {
                if env.is_in_current_scope(name)
                {
                    diagnostics.push(
                        Diagnostic::error(ErrorCode::DuplicateDeclaration)
                            .with_main_label(span.clone(), format!("'{name}' redeclared here")),
                    );
                }
                else
                {
                    env.declare(name, *ty);
                }
            }
        }

        // Pass 2: type-check each RHS with the fully-populated scope
        let exprs = exprs
            .into_iter()
            .map(|s_expr| {
                let (expr, diags) = s_expr.type_check(&mut env);
                diagnostics.extend(diags);
                expr
            })
            .collect::<Vec<_>>();
        Self { exprs, diagnostics }
    }
}

impl<'src> Spanned<Expr<'src>> {
    const fn ty(&self) -> Ty {
        match self
        {
            Spanned(Expr::I64(_), _) => Ty::I64,
            Spanned(Expr::F64(_), _) => Ty::F64,
            Spanned(
                Expr::Ident { ty, .. } | Expr::Declaration { ty, .. } | Expr::Cast(ty, _),
                _,
            ) => *ty,
        }
    }

    fn type_check(self, env: &mut Env<'src>) -> (Self, Vec<Diagnostic>) {
        let mut diagnostics = Vec::new();
        let expr = match self
        {
            Spanned(Expr::I64(_) | Expr::F64(_), _) => self,
            Spanned(Expr::Ident { name, ty }, span) => match env.lookup(name)
            {
                None =>
                {
                    diagnostics.push(
                        Diagnostic::error(ErrorCode::UndefinedVariable)
                            .with_main_label(span.clone(), format!("'{name}' not defined")),
                    );
                    Spanned(Expr::Ident { name, ty }, span)
                },
                Some(env_ty) =>
                {
                    if ty != Ty::Unknown && ty != env_ty
                    {
                        diagnostics.push(
                            Diagnostic::error(ErrorCode::TypeAscriptionMismatch).with_main_label(
                                span.clone(),
                                format!("ascribed as '{ty}' here but '{name}' has type '{env_ty}'"),
                            ),
                        );
                    }
                    Spanned(Expr::Ident { name, ty: env_ty }, span)
                },
            },
            Spanned(Expr::Cast(ty, expr), span) =>
            {
                let (typed_expr, expr_diagnostics) = expr.type_check(env);
                diagnostics.extend(expr_diagnostics);
                Spanned(Expr::Cast(ty, Box::new(typed_expr)), span)
            },
            Spanned(Expr::Declaration { ty, expr, name }, span) =>
            {
                env.push_scope();
                let (typed_expr, expr_diagnostics) = expr.type_check(env);
                env.pop_scope();
                diagnostics.extend(expr_diagnostics);
                if ty != typed_expr.ty()
                {
                    let expr_ty = typed_expr.ty();
                    diagnostics.push(
                        Diagnostic::error(ErrorCode::TypeMismatch)
                            .with_main_label(
                                typed_expr.1.clone(),
                                format!("found '{expr_ty}' here"),
                            )
                            .with_context_label(span.clone(), format!("expected '{ty}'")),
                    );
                }
                Spanned(
                    Expr::Declaration {
                        ty,
                        expr: Box::new(typed_expr),
                        name,
                    },
                    span,
                )
            },
        };
        (expr, diagnostics)
    }
}
