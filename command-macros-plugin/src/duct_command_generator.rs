use crate::{
    Arg,
    Expr,
    Spanned,
    Splice,
    Stmt,
    Tree,
};
use crate::command_args_generator::CommandArgsGenerator;
use crate::command_generator::{CommandGenerator, IntoCommandExpression};
use crate::syntax::{from_source, new_ident};

use proc_macro::{Span, TokenTree};

type Result<T> = ::std::result::Result<T, ()>;

pub struct DuctCommandGenerator {
    command_var: TokenTree,
    args_gen: CommandArgsGenerator
}

impl DuctCommandGenerator {
    pub fn new() -> DuctCommandGenerator {
        DuctCommandGenerator {
            command_var: new_ident("cmd"),
            args_gen: CommandArgsGenerator::new()
        }
    }
}

impl IntoCommandExpression for DuctCommandGenerator {
    const TYPE_HINT_PLACEHOLDER: &'static str = "::std::vec::Vec::<::std::ffi::OsString>::new()";

    fn generate(self, mut trees: Vec<Tree>) -> Result<Expr> {
        // let DuctCommandGenerator { command_var, args_gen } = self;

        if trees.is_empty() {
            Span::call_site().error("This macro needs at least the command name").emit();
            return Err(());
        }

        let cmd_tree = trees.remove(0);
        let args_expr = self.args_gen.generate(trees)?;

        match cmd_tree {
            Tree::Arg(arg) => {
                let span = arg.span();
                let cmd_fn = from_source("::duct::cmd", span);
                let str_expr = CommandArgsGenerator::generate_os_str(arg)?;
                Ok(Expr::call_multiple_args(cmd_fn, vec![str_expr, args_expr].into_iter(), span))
            }
            Tree::Cmd(cmd) => {
                // TODO - interpret `{x}` as a `duct::Expression`, and allow appending args
                // via `duct::Expression::before_spawn`
                cmd.span().error("`{Command_expr}` cannot be used for `duct` commands").emit();
                Err(())
            },
            other => {
                other.span().error("`duct` command name should be `cmd` or `(cmd_name_expr)``").emit();
                Err(())
            }
        }
    }
}
