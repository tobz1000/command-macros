use crate::{
    Arg,
    Expr,
    Spanned,
    Splice,
    Stmt,
    Tree,
};
use crate::command_generator::CommandGenerator;
use crate::syntax::{from_source, new_ident};

use proc_macro::{Span, TokenTree};

type Result<T> = ::std::result::Result<T, ()>;

pub struct CommandArgsGenerator {
    vec_var: TokenTree
}

impl CommandArgsGenerator {
    pub fn new() -> CommandArgsGenerator {
        CommandArgsGenerator {
            vec_var: new_ident("cmd_args")
        }
    }
}

impl CommandGenerator for CommandArgsGenerator {
    const TYPE_HINT_PLACEHOLDER: &'static str = "::std::vec::Vec::<::std::ffi::OsString>::new()";

    fn generate(self, trees: Vec<Tree>) -> Result<Expr> {
        let cmd_expr = Expr::from_source(Self::TYPE_HINT_PLACEHOLDER, Span::call_site());

        let init_stmt = Stmt::new_let(&self.vec_var, cmd_expr);
        let mut stmts: Vec<Stmt> = vec![init_stmt];
        stmts.extend(self.generate_stmts(trees)?);

        let block = Expr::block(stmts, Expr::from_tt(self.vec_var), Span::call_site());

        Ok(block)
    }

    fn generate_single_arg(&self, arg: Arg) -> Result<Stmt> {
        let span = arg.span();
        let os_str = Self::generate_os_str(arg)?;
        let call_expr = Expr::call_method_on(&self.vec_var, "push", os_str, span);
        Ok(call_expr.into_stmt())
    }

    // TODO - this only works on `Iterator<Item = OsString>`. Ideally, the accepted form would be identical to
    // `command!`, meaning it should accept Iterator<Item = impl AsRef<OsString>>.
    fn generate_args_iter(&self, args: Spanned<Expr>) -> Result<Stmt> {
        let Spanned { elem: expr, span } = args;
        let call_expr = Expr::call_method_on(&self.vec_var, "extend", expr, span);
        Ok(call_expr.into_stmt())
    }

    fn generate_splice(Spanned { elem: splice, span }: Spanned<Splice>) -> Result<Expr> {
        let expr = match splice {
            Splice::Word(word) => {
                let str_expr = Expr::string_literal(&word);

                os_string_from_str(str_expr, span)
            },
            Splice::Literal(lit) => {
                let str_expr = Self::generate_literal(lit)?;

                os_string_from_str(str_expr, span)
            },
            Splice::AsOsStr(expr) => expr,
            Splice::ToStr(expr) => {
                let to_string = Expr::call(
                    from_source("ToString::to_string", span),
                    Expr::reference(expr, span),
                    span
                );

                Expr::call(from_source("::std::ffi::OsString::from", span), to_string, span)
            },
        };
        Ok(expr)
    }
}

fn os_string_from_str(str_expr: Expr, span: Span) -> Expr {
    let os_string_var = new_ident("os_string");

    let os_string_init_stmt = Stmt::new_let(
        &os_string_var,
        Expr::from_source("::std::ffi::OsString::new()", span)
    );

    let call_stmt = Expr::call_method_on(&os_string_var, "push", str_expr, span)
        .into_stmt();
    let return_expr = Expr::from_tt(os_string_var);

    let block = Expr::block(vec![os_string_init_stmt, call_stmt], return_expr, span);

    block
}