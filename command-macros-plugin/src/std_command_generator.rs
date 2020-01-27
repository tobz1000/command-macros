use crate::{
    Arg,
    Expr,
    Spanned,
    Splice,
    Stmt,
    Tree,
};
use crate::command_generator::{CommandGenerator, IntoCommandExpression};
use crate::syntax::{from_source, new_ident};

use proc_macro::{Span, TokenTree};

type Result<T> = ::std::result::Result<T, ()>;

pub struct StdCommandGenerator {
    command_var: TokenTree
}

impl StdCommandGenerator {
    pub fn new() -> StdCommandGenerator {
        StdCommandGenerator {
            command_var: new_ident("cmd")
        }
    }
}

impl IntoCommandExpression for StdCommandGenerator {
    const TYPE_HINT_PLACEHOLDER: &'static str = "::std::process::Command::new(\"dummy\")";

    fn generate(self, mut trees: Vec<Tree>) -> Result<Expr> {
        if trees.is_empty() {
            Span::call_site().error("This macro needs at least the command name").emit();
            return Err(());
        }

        let cmd_tree = trees.remove(0);
        let cmd_expr: Expr = match cmd_tree {
            Tree::Arg(arg) => {
                let span = arg.span();
                let str_expr = Self::generate_os_str(arg)?;
                Expr::call(from_source("::std::process::Command::new", span), str_expr, span)
            }
            Tree::Cmd(cmd) => cmd,
            other => {
                other.span().error("Command name should be `cmd` `(cmd_name_expr)` or `{Command_expr}`").emit();
                return Err(())
            }
        };

        let init_stmt = Stmt::new_let(&self.command_var, cmd_expr);
        let mut stmts: Vec<Stmt> = vec![init_stmt];
        stmts.extend(self.generate_stmts(trees)?);

        let block = Expr::block(stmts, Expr::from_tt(self.command_var), Span::call_site());

        Ok(block)
    }
}

impl CommandGenerator for StdCommandGenerator {
    fn generate_single_arg(&self, arg: Arg) -> Result<Stmt> {
        let span = arg.span();
        let os_str = Self::generate_os_str(arg)?;
        let call_expr = Expr::call_method_on(&self.command_var, "arg", os_str, span);
        Ok(call_expr.into_stmt())
    }

    fn generate_args_iter(&self, Spanned { elem: expr, span }: Spanned<Expr>) -> Result<Stmt> {
        let call_expr = Expr::call_method_on(&self.command_var, "args", expr, span);
        Ok(call_expr.into_stmt())
    }

    fn generate_splice(Spanned { elem: splice, span }: Spanned<Splice>) -> Result<Expr> {
        let expr = match splice {
            Splice::Word(word) => Expr::string_literal(&word),
            Splice::Literal(lit) => Self::generate_literal(lit)?,
            Splice::AsOsStr(expr) => Expr::reference(expr, span),
            Splice::ToStr(expr) => Expr::call(
                from_source("ToString::to_string", span),
                Expr::reference(expr, span),
                span
            ),
        };
        Ok(expr)
    }
}