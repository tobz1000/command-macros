use crate::{Expr, Tree};
use crate::command_args_generator::CommandArgsGenerator;
use crate::command_generator::{CommandGenerator, IntoCommandExpression};
use crate::std_command_generator::StdCommandGenerator;
use crate::syntax::{from_source, Pat};

use proc_macro::Span;
use std::iter::once;

type Result<T> = ::std::result::Result<T, ()>;

pub struct DuctCommandGenerator;

impl IntoCommandExpression for DuctCommandGenerator {
    const TYPE_HINT_PLACEHOLDER: &'static str = "::duct::cmd(\"dummy\", &[\"dummy\"])";

    fn generate(self, mut trees: Vec<Tree>) -> Result<Expr> {
        if trees.is_empty() {
            Span::call_site().error("This macro needs at least the command name").emit();
            return Err(());
        }

        let cmd_tree = trees.remove(0);

        match cmd_tree {
            Tree::Arg(arg) => {
                let span = Span::call_site();
                let cmd_fn = from_source("::duct::cmd", span);
                let str_expr = CommandArgsGenerator::generate_os_str(arg)?;
                let args_gen = CommandArgsGenerator::new();
                let args_expr = args_gen.generate(trees)?;

                Ok(Expr::call_multiple_args(cmd_fn, vec![str_expr, args_expr].into_iter(), span))
            }
            Tree::Cmd(duct_cmd) => {
                let span = Span::call_site();
                let std_gen = StdCommandGenerator::new();
                let cmd_binding = Pat(once(std_gen.command_var.clone()).collect());
                let cmd_return = Expr::from_source("Ok(())", span);
                let cmd_appends = std_gen.generate_stmts(trees)?;

                let append_args_closure = Expr::closure(cmd_binding, cmd_appends, cmd_return, span);

                Ok(Expr::call_method(duct_cmd, "before_spawn", append_args_closure, span))
            },
            other => {
                other.span().error("`duct` command name should be `cmd` or `(cmd_name_expr)``").emit();
                Err(())
            }
        }
    }
}
