use std::{collections::HashMap, fmt::Display};

use crate::{
    lexer::Span,
    parser::{BinOp, Expr, ExprKind, Parser, Pattern, Ty},
};

#[derive(Debug, Clone)]
struct Symbol {
    name: String,
    ty: Ty,
    initialized: bool,
}

pub type Scope = HashMap<String, (usize, Symbol)>;

#[derive(Debug, Clone)]
pub struct ScopedExpr {
    scope: Vec<Scope>,
    expr: Expr,
}

#[derive(Debug, Clone)]
pub enum ProgExpr {
    Lit(Expr),
    Simple(ScopedExpr),
    Unit(Option<Box<ProgExpr>>),
    Decla {
        name: String,
        ty: Ty,
        expr: Box<ProgExpr>,
    },
    Sequence {
        curr: Box<ProgExpr>,
        next: Box<ProgExpr>,
    },
    BinOp {
        op: BinOp,
        lhs: Box<ProgExpr>,
        rhs: Box<ProgExpr>,
    },
    Paren(Box<ProgExpr>),
}

impl Display for ProgExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProgExpr::Lit(expr) => write!(f, "{expr}"),
            ProgExpr::Simple(scoped_expr) => write!(f, "{}", scoped_expr.expr),
            ProgExpr::Unit(Some(expr)) => write!(f, "{expr};"),
            ProgExpr::Unit(None) => write!(f, "();"),
            ProgExpr::Decla { name, ty, expr } => write!(f, "decla {name}: {ty} =\n{expr}"),
            ProgExpr::Sequence { curr, next } => write!(f, "{curr};\n{next}"),
            ProgExpr::BinOp { op, lhs, rhs } => write!(f, "{lhs} {} {rhs}", op),
            ProgExpr::Paren(expr) => write!(f, "({expr})"),
        }
    }
}

impl ProgExpr {
    pub fn span(&self) -> Span {
        match self {
            ProgExpr::Lit(expr) => expr.span,
            ProgExpr::Simple(scoped_expr) => scoped_expr.expr.span,
            ProgExpr::Unit(Some(expr)) => expr.span(),
            ProgExpr::Unit(None) => unreachable!(),
            ProgExpr::Decla { expr, .. } => expr.span(),
            ProgExpr::Sequence { curr, next } => Span {
                start: curr.span().start,
                end: next.span().end,
            },
            ProgExpr::BinOp { lhs, rhs, .. } => Span {
                start: lhs.span().start,
                end: rhs.span().end,
            },
            ProgExpr::Paren(expr) => expr.span(),
        }
    }

    pub fn path(&self) -> &str {
        match self {
            ProgExpr::Lit(expr) => &expr.filename,
            ProgExpr::Simple(scoped_expr) => &scoped_expr.expr.filename,
            ProgExpr::Unit(Some(expr)) => expr.path(),
            ProgExpr::Unit(None) => unreachable!(),
            ProgExpr::Decla { expr, .. } => expr.path(),
            ProgExpr::Sequence { curr, .. } => curr.path(),
            ProgExpr::BinOp { lhs, .. } => lhs.path(),
            ProgExpr::Paren(expr) => expr.path(),
        }
    }
}

pub struct Program {
    //TODO: Store undefined symbols yet to see if they will appears in the global scope later on
    pub prog_exprs: Vec<ProgExpr>,
}

impl Program {
    pub fn new(parser: Parser) -> Self {
        Self {
            prog_exprs: parser.map(|e| to_prog_expr(e, vec![])).collect(),
        }
    }
}

fn to_prog_expr(expr: Expr, mut scope: Vec<Scope>) -> ProgExpr {
    match expr.kind {
        ExprKind::U32(_)
        | ExprKind::U64(_)
        | ExprKind::U128(_)
        | ExprKind::F64(_)
        | ExprKind::Bool(_)
        | ExprKind::Str(_)
        | ExprKind::Char(_) => ProgExpr::Lit(expr),
        ExprKind::Ident(_) => ProgExpr::Simple(ScopedExpr { scope, expr }),
        ExprKind::Decla { name, ty, expr } => ProgExpr::Decla {
            name,
            expr: {
                scope.push(scope_from_ty(&ty));
                Box::new(to_prog_expr(*expr, scope))
            },
            ty,
        },
        ExprKind::Unit(expr) => {
            ProgExpr::Unit(expr.map(|expr| Box::new(to_prog_expr(*expr, scope.clone()))))
        }
        ExprKind::Sequence { curr, next } => ProgExpr::Sequence {
            curr: Box::new(to_prog_expr(*curr, scope.clone())),
            next: Box::new(to_prog_expr(*next, scope.clone())),
        },
        ExprKind::BinOp { op, lhs, rhs } => ProgExpr::BinOp {
            op,
            lhs: Box::new(to_prog_expr(*lhs, scope.clone())),
            rhs: Box::new(to_prog_expr(*rhs, scope)),
        },
        ExprKind::Paren(expr) => ProgExpr::Paren(Box::new(to_prog_expr(*expr, scope))),
    }
}

fn scope_from_ty(ty: &Ty) -> Scope {
    let mut scope = HashMap::new();
    let mut idx = 0;
    let mut ty = ty.clone();
    while let Ty::Arrow { param, ret } = ty {
        match *param {
            Pattern::Wildcard => None, // Ignore wildcard patterns
            Pattern::NamedWildcard(name) => scope.insert(
                name.clone(),
                (
                    idx,
                    Symbol {
                        name,
                        ty: Ty::Unknown,
                        initialized: false,
                    },
                ),
            ),
            Pattern::Typed { ty, name } => scope.insert(
                name.clone(),
                (
                    idx,
                    Symbol {
                        name,
                        ty,
                        initialized: false,
                    },
                ),
            ),
        };
        ty = *ret;
        idx += 1;
    }
    scope
}
