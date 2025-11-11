use std::collections::HashMap;

use crate::program::ScopedExpr;
use crate::{
    lexer::Span,
    parser::{Expr, ExprKind, Pattern, Ty},
    program::{to_prog_expr, ProgExpr, Program, Scope},
};

fn aggregate_first_order_declarations(
    program: Program,
) -> Result<HashMap<String, Vec<(Ty, ProgExpr)>>, String> {
    let mut declarations: HashMap<String, Vec<(Ty, ProgExpr)>> = HashMap::new();
    for curr_expr in &program.prog_exprs {
        if let ProgExpr::Decla { name, ty, expr } = curr_expr {
            if !validate_decla(&declarations, name, ty, &curr_expr)? {
                Err(format!(
                    "Typing Error: {}:{}: Declaration type '{ty}' is not correct for '{name}'",
                    curr_expr.path(),
                    curr_expr.span()
                ))?
            }
            if declarations.contains_key(name) {
                declarations
                    .get_mut(name)
                    .unwrap()
                    .push((ty.clone(), *expr.clone()));
            } else {
                declarations.insert(name.clone(), vec![(ty.clone(), *expr.clone())]);
            }
        } else {
            Err(format!(
                "ERROR: {}:{}: Only declarations are allowed at the top level",
                curr_expr.path(),
                curr_expr.span()
            ))?;
        }
    }
    Ok(declarations)
}

fn validate_decla(
    declarations: &HashMap<String, Vec<(Ty, ProgExpr)>>,
    name: &str,
    ty: &Ty,
    curr_expr: &ProgExpr,
) -> Result<bool, String> {
    let variants = declarations.get(name);
    if name == "main" {
        if variants.is_some() {
            Err(format!(
                "ERROR: {}:{}: 'main' function cannot be overloaded",
                curr_expr.path(),
                curr_expr.span()
            ))
        } else {
            match ty {
                Ty::Arrow { param, ret }
                    if param.is_compatible(&Pattern::Typed {
                        ty: Ty::Usize,
                        name: "argc".to_string(),
                    }) && ret.is_compatible(&Ty::Arrow {
                        param: Box::new(Pattern::Typed {
                            ty: Ty::ConstRef(Box::new(Ty::Slice(Box::new(Ty::Str)))),
                            name: "argv".to_string(),
                        }),
                        ret: Box::new(Ty::I32),
                    }) =>
                {
                    Ok(true)
                }
                _ => Err(format!(
                    "ERROR {}:{}:'main' function must have a type compatible with `usize -> &[str] -> i32`",
                    curr_expr.path(),
                    curr_expr.span()
                )),
            }
        }
    } else if variants.is_none() {
        Ok(ty.is_explicit())
    } else {
        let (original, _) = &variants.unwrap()[0];
        Ok(original.is_compatible(ty))
    }
}

pub struct TypedProgram {
    pub path: String,
    pub declarations: HashMap<String, Vec<(Ty, TypedProgExpr)>>,
}

impl Program {
    pub fn type_check(self) -> Result<TypedProgram, String> {
        let path = self.path.clone();
        let mut declarations = aggregate_first_order_declarations(self)?;
        let mut typed_declarations: HashMap<String, Vec<(Ty, TypedProgExpr)>> = HashMap::new();

        let mut names = declarations.keys().cloned().collect::<Vec<_>>();

        loop {
            if names.is_empty() {
                break;
            }
            let curr_name = names.remove(0);
            let variants = declarations.remove(&curr_name).unwrap();
            let typed_variants: Vec<(Ty, TypedProgExpr)> = variants
                .into_iter()
                .map(|(ty, expr)| {
                    let typed_expr = expr.type_check(&mut names, &mut declarations, &mut typed_declarations)?;
                    if typed_expr.ty.is_compatible(&ty) {
                        Ok((typed_expr.ty.clone(), typed_expr))
                    } else {
                        Err(format!(
                            "Typing Error: {}:{}: Declaration type '{ty}' is not correct for '{curr_name}'",
                            typed_expr.path(),
                            typed_expr.span()
                        ))
                    }
                })
                .collect::<Result<Vec<_>, String>>()?;
            typed_declarations.insert(curr_name, typed_variants);
        }

        Ok(TypedProgram {
            path,
            declarations: typed_declarations,
        })
    }
}

impl ProgExpr {
    pub fn type_check(self, scope: &mut Vec<Scope>) -> Result<TypedProgExpr, String> {
        match &self {
            ProgExpr::Lit(expr) => {
                let ty = expr.type_check(scope)?;
                Ok(TypedProgExpr { ty, expr: self })
            }
            ProgExpr::Simple(scoped_expr) => {
                let ty = scoped_expr.type_check(scope)?;
                Ok(TypedProgExpr { ty, expr: self })
            }
            ProgExpr::Unit(prog_expr) => todo!(),
            ProgExpr::Decla { name, ty, expr } => todo!(),
            ProgExpr::Sequence { curr, next } => todo!(),
            ProgExpr::BinOp { op, lhs, rhs } => todo!(),
            ProgExpr::Paren(prog_expr) => todo!(),
        }
    }
}

impl Expr {
    fn type_check(
        &self,
        scope: impl Into<Option<Vec<Scope>>> + Clone,
        typed_scope: impl Into<Option<Vec<TypedScope>>> + Clone,
    ) -> Result<Ty, String> {
        let scope = scope.into();
        match &self.kind {
            ExprKind::U32(_) => Ok(Ty::U32),
            ExprKind::U64(_) => Ok(Ty::U64),
            ExprKind::U128(_) => Ok(Ty::U128),
            ExprKind::F64(_) => Ok(Ty::F64),
            ExprKind::Bool(_) => Ok(Ty::Bool),
            ExprKind::Str(_) => Ok(Ty::Str),
            ExprKind::Char(_) => Ok(Ty::Char),
            ExprKind::Unit(None) => Ok(Ty::Unit),
            ExprKind::Unit(Some(expr)) => {
                expr.type_check(scope, typed_scope)?;
                Ok(Ty::Unit)
            }
            ExprKind::Paren(expr) => expr.type_check(scope, typed_scope),
            ExprKind::Sequence { curr, next } => {
                curr.type_check(scope.clone(), typed_scope.clone())?;
                next.type_check(scope, typed_scope)
            }
            ExprKind::Ident(name) => todo!(),
            ExprKind::Decla { name, ty, expr } => {
                let prog_expr = to_prog_expr(*expr.clone(), scope.unwrap_or(Vec::new()));
                todo!()
            }
            ExprKind::BinOp { op, lhs, rhs } => todo!(),
            ExprKind::Application(func, param) => {
                let func_ty =
                    func.type_check(names, declarations, typed_declarations, scope.clone())?;
                let param_ty = param.type_check(names, declarations, typed_declarations, scope)?;
                match (func_ty, param_ty) {
                    (Ty::Arrow { param, ret }, ty)
                        if param.is_compatible(&Pattern::Typed {
                            ty: ty.clone(),
                            name: "".to_string(),
                        }) =>
                    {
                        todo!("Application with Matching param type")
                    }
                    (Ty::Arrow { param, ret }, ty) => {
                        todo!("Application with non matching param type")
                    }
                    (func_ty, param_ty) => todo!("Application on non applicable type"),
                }
            }
        }
    }
}
pub struct TypedProgExpr {
    pub ty: Ty,
    pub expr: ProgExpr,
}

impl TypedProgExpr {
    pub fn path(&self) -> &str {
        self.expr.path()
    }

    pub fn span(&self) -> Span {
        self.expr.span()
    }
}

impl ScopedExpr {
    pub fn type_check(&self, scope: &mut Vec<Scope>) -> Result<Ty, String> {
        scope.append();
        self.sc
        self.expr.type_check(scope)
    }
}
