use crate::{
    Arg,
    Arm,
    Block,
    Condition,
    Expr,
    For,
    If,
    Literal,
    Match,
    Pat,
    Spanned,
    Splice,
    Tree,
};
use crate::syntax::{
    new_block,
    new_ident,
    new_spanned_ident,
    Stmt,
    surround
};

use proc_macro::{Delimiter, Span, TokenTree};
use std::iter::once;

type Result<T> = ::std::result::Result<T, ()>;

pub(crate) trait CommandGenerator {
    /// Inserted on syntax error to generate type hint
    const TYPE_HINT_PLACEHOLDER: &'static str;

    fn generate(self, tree: Vec<Tree>) -> Result<Expr>;

    fn generate_single_arg(&self, arg: Arg) -> Result<Stmt>;

    fn generate_args_iter(&self, args: Spanned<Expr>) -> Result<Stmt>;

    fn generate_splice(spanned: Spanned<Splice>) -> Result<Expr>;

    fn generate_stmts(&self, trees: Vec<Tree>) -> Result<Vec<Stmt>> {
        trees
            .into_iter()
            .map(|tree| {
                match tree {
                    Tree::Arg(arg) => self.generate_single_arg(arg),
                    Tree::Args(args) => self.generate_args_iter(args),
                    Tree::For(pieces) => self.generate_for(pieces),
                    Tree::If(pieces) => self.generate_if(pieces),
                    Tree::Match(pieces) => self.generate_match(pieces),
                    Tree::Cmd(expr) => {
                        expr.span()
                            .error("Command block cannot be used for command arguments")
                            .emit();
                        return Err(())
                    }
                }
            })
            .collect()
    }

    // fn generate_block(&self, Block(Spanned { elem: trees, span }): Block) -> Result<TokenTree> {
    fn generate_block(&self, block: Block) -> Result<TokenTree> {
        let Block(Spanned { elem: trees, span }) = block;
        let stmts = self.generate_stmts(trees)?;
        Ok(new_block(stmts, span))
    }

    // fn generate_for(&self, For { for_span, pat, in_tt, expr, block }: For) -> Result<Stmt> {
    fn generate_for(&self, for_item: For) -> Result<Stmt> {
        let For { for_span, pat, in_tt, expr, block } = for_item;
        let stream = once(new_spanned_ident("for", for_span))
            .chain(pat.0)
            .chain(once(in_tt))
            .chain(expr.into_stream())
            .chain(once(self.generate_block(block)?))
            .collect();
        Ok(Stmt::from_stream(stream))
    }

    // fn generate_if(&self, If { if_span, cond, then_block, else_block }: If) -> Result<Stmt> {
    fn generate_if(&self, if_item: If) -> Result<Stmt> {
        let If { if_span, cond, then_block, else_block } = if_item;
        let cond_stream = match cond {
            Condition::Bool(expr)                          => expr.into_stream(),
            Condition::IfLet(let_tt, pat, equals_tt, expr) => {
                once(let_tt)
                    .chain(pat.0)
                    .chain(once(equals_tt))
                    .chain(expr.into_stream())
                    .collect()
            }
        };
        let stream = once(new_spanned_ident("if", if_span))
            .chain(cond_stream)
            .chain(once(self.generate_block(then_block)?))
            .chain(once(new_spanned_ident("else", Span::call_site())))
            .chain(once(self.generate_block(else_block)?))
            .collect();
        Ok(Stmt::from_stream(stream))
    }

    // fn generate_match(&self, Match { match_span, expr, block_span, arms }: Match) -> Result<Stmt> {
    fn generate_match(&self, match_item: Match) -> Result<Stmt> {
        let generate_arm = |arm: Arm| {
            let (Pat(pat), (arrow_l, arrow_r), block) = arm;

            Ok(
                pat.into_iter()
                    .chain(once(arrow_l))
                    .chain(once(arrow_r))
                    .chain(once(self.generate_block(block)?))
            )
        };

        let Match { match_span, expr, block_span, arms } = match_item;
        let mut arm_stream = Vec::new();
        for arm in arms {
            arm_stream.extend(generate_arm(arm)?);
        }
        let arm_stream = arm_stream.into_iter().collect();

        let block = surround(arm_stream, Delimiter::Brace, block_span);

        let stream = once(new_spanned_ident("match", match_span))
            .chain(expr.into_stream())
            .chain(once(block))
            .collect();

        Ok(Stmt::from_stream(stream))
    }

    fn generate_literal(literal: Literal) -> Result<Expr> {
        let repr = literal.to_string();
        if repr.starts_with("'") {
            literal.span().error("Use string literals instead").emit();
            Ok(Expr::string_literal("<error>"))
        } else if repr.contains("\"") {
            Ok(Expr::from_tt(literal.into()))
        } else if repr.contains("'") {
            literal.span().error("Unsupported literal").emit();
            Ok(Expr::string_literal("<error>"))
        } else {
            Ok(Expr::string_literal(&literal.to_string()))
        }
    }

    fn generate_os_str(arg: Arg) -> Result<Expr> {
        let full_span = arg.span();
        match arg {
            Arg::Single(splice) => Self::generate_splice(splice),
            Arg::Touching(splices) => {
                let os_string = Expr::from_source("::std::ffi::OsString::new()", full_span);
                let buf_var = new_ident("buf");
                let init_stmt = Stmt::new_let(&buf_var, os_string);
                let mut stmts = vec![init_stmt];

                for splice in splices {
                    let span = splice.span;
                    stmts.push(Expr::call_method_on(
                        &buf_var,
                        "push",
                        Expr::reference(Self::generate_splice(splice)?, span),
                        span,
                    ).into_stmt())
                }

                Ok(Expr::block(stmts, Expr::from_tt(buf_var), full_span))
            }
        }
    }
}
