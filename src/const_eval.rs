use crate::{
    Spanned,
    parser::expr::{DeclKind, Expr},
    program::{Program, diagnostics::Reportable},
    topo_order::topo_order,
    typer::Typer,
};

impl<'src> Program<'src, Typer<'src>> {
    pub fn const_eval(mut self) -> Self {
        let mut diagnostics = Vec::new();

        let exprs = self.state.exprs;
        let env = &mut self.state.env;

        let Ok((order, decl_map)) = topo_order(exprs.clone()).inspect_err(|diag| {
            diagnostics.push(diag.clone());
            diagnostics.report(self.source, &self.args.input);
        })
        else
        {
            unreachable!()
        };

        for name in order
        {
            let Spanned(expr, _) = decl_map
                .get(name)
                .expect("topo_order should not provide non-existant name");

            match expr.const_value(env)
            {
                Some(_) =>
                {
                    println!("Declaration of {name} is const (Should be inlined)");
                },
                None if matches!(
                    expr,
                    Expr::Declaration {
                        kind: DeclKind::Const,
                        ..
                    }
                ) =>
                {
                    println!("Declaration of {name} is not const (ERROR)");
                    // TODO: emit NotConstExpr error
                },
                None =>
                {
                    println!("Declaration of {name} is not const (OK)");
                },
            }
        }

        Self {
            args:   self.args,
            source: self.source,
            state:  Typer {
                exprs,
                env: self.state.env,
            },
        }
    }
}
