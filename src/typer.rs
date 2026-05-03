use std::collections::HashMap;

use chumsky::span::Spanned;

use crate::{
    diagnostics::Diagnostic,
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

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// # Panics
    /// - Panics if you attempt to pop the root scope
    pub fn pop_scope(&mut self) {
        assert!(self.scopes.len() > 1, "attempted to pop the root scope");
        self.scopes.pop();
    }
}

pub struct TypedOutput<'a> {
    pub exprs: Vec<Expr<'a>>,
    pub diagnostics: Vec<Diagnostic>,
}

impl<'a> TypedOutput<'a> {
    #[must_use]
    pub fn type_check(exprs: Vec<Expr<'a>>) -> Self {
        let mut diagnostics: Vec<Diagnostic> = Vec::new();
        let mut env = Env::new();

        // Pass 1: pre-populate all declarations so every RHS sees the full scope
        for expr in &exprs {
            if let Expr::Declaration { name, ty, .. } = expr {
                if env.is_in_current_scope(name) {
                    diagnostics.push(Diagnostic::error(format!(
                        "'{name}' is already declared in this scope"
                    )));
                } else {
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

impl<'src> Expr<'src> {
    const fn ty(&self) -> Ty {
        match self {
            Self::I64(_) => Ty::I64,
            Self::F64(_) => Ty::F64,
            Self::Ident { ty, .. } | Self::Declaration { ty, .. } | Self::Cast(ty, _) => *ty,
        }
    }

    fn type_check(self, env: &mut Env<'src>) -> (Self, Vec<Diagnostic>) {
        let mut diagnostics = Vec::new();
        let expr = match self {
            Self::I64(_) | Self::F64(_) => self,
            Self::Ident { name, ty } => match env.lookup(name) {
                None => {
                    diagnostics.push(Diagnostic::error(format!("Undefined variable '{name}'")));
                    Self::Ident {
                        name,
                        ty: Ty::Unknown,
                    }
                }
                Some(env_ty) => {
                    if ty != Ty::Unknown && ty != env_ty {
                        diagnostics.push(Diagnostic::error(format!(
                            "Type ascription mismatch: '{name}' has type '{env_ty}' but is ascribed '{ty}'"
                        )));
                    }
                    Self::Ident { name, ty: env_ty }
                }
            },
            Self::Cast(ty, expr) => {
                let (typed_expr, expr_diagnostics) = expr.type_check(env);
                diagnostics.extend(expr_diagnostics);
                Self::Cast(
                    ty,
                    Box::new(Spanned {
                        inner: typed_expr,
                        span: expr.span,
                    }),
                )
            }
            Self::Declaration { ty, expr, name } => {
                env.push_scope();
                let (typed_expr, expr_diagnostics) = expr.type_check(env);
                env.pop_scope();
                diagnostics.extend(expr_diagnostics);
                if ty != typed_expr.ty() {
                    diagnostics.push(Diagnostic::error(format!(
                        "Type mismatch: expected '{ty}', found '{}'",
                        typed_expr.ty()
                    )));
                }
                Self::Declaration {
                    ty,
                    expr: Box::new(Spanned {
                        inner: typed_expr,
                        span: expr.span,
                    }),
                    name,
                }
            }
        };
        (expr, diagnostics)
    }
}
