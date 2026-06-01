use std::collections::{HashMap, VecDeque};

use crate::{
    Spanned,
    parser::expr::Expr,
    program::diagnostics::{Diagnostic, ErrorCode},
};

type DepMap<'src> = HashMap<&'src str, Vec<&'src str>>;
type DeclMap<'src> = HashMap<&'src str, Spanned<Expr<'src>>>;

fn maps(exprs: VecDeque<Spanned<Expr<'_>>>) -> (DepMap<'_>, DeclMap<'_>) {
    let mut dep_map: DepMap = HashMap::new();
    let mut decl_map: DeclMap = HashMap::new();
    for Spanned(expr, span) in exprs
    {
        if let Expr::Declaration {
            name,
            expr: ref rhs,
            ..
        } = expr
        {
            dep_map.insert(name, rhs.as_ref().0.deps());
            decl_map.insert(name, Spanned(expr, span));
        }
    }
    for deps in dep_map.values_mut()
    {
        deps.retain(|d| decl_map.contains_key(d));
    }
    (dep_map, decl_map)
}

pub fn topo_order(
    exprs: VecDeque<Spanned<Expr<'_>>>,
) -> Result<(Vec<&str>, DeclMap<'_>), Diagnostic> {
    let (dep_map, decl_map) = maps(exprs);

    let mut in_deg: HashMap<&'_ str, usize> = dep_map.iter().map(|(&n, d)| (n, d.len())).collect();

    let mut rdeps: DepMap = HashMap::new();
    for (&name, deps) in &dep_map
    {
        for dep in deps
        {
            rdeps.entry(dep).or_default().push(name);
        }
    }

    let mut queue: VecDeque<&'_ str> = {
        let mut v: Vec<&'_ str> = in_deg
            .iter()
            .filter_map(|(&n, &d)| (d == 0).then_some(n))
            .collect();
        v.sort_unstable();
        v.into()
    };

    let mut order = Vec::with_capacity(dep_map.len());
    while let Some(name) = queue.pop_front()
    {
        order.push(name);
        let mut newly_freed: Vec<&'_ str> = rdeps
            .get(name)
            .map_or([].as_slice(), Vec::as_slice)
            .iter()
            .filter_map(|&dep| {
                let d = in_deg.get_mut(dep).unwrap();
                *d -= 1;
                (*d == 0).then_some(dep)
            })
            .collect();
        newly_freed.sort_unstable();
        queue.extend(newly_freed);
    }

    if order.len() == dep_map.len()
    {
        Ok((order, decl_map))
    }
    else
    {
        let cycle: Vec<&str> = in_deg
            .into_iter()
            .filter_map(|(name, deg)| (deg > 0).then_some(name))
            .collect();
        let mut diag = Diagnostic::error(ErrorCode::CyclicDeclaration);
        for name in &cycle
        {
            if let Some(Spanned(_, span)) = decl_map.get(name)
            {
                diag = diag.with_main_label(*span, format!("'{name}' is part of a cycle"));
            }
        }
        Err(diag)
    }
}
