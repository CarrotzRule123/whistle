use super::expr::*;
use super::ident::*;
use crate::lexer::*;
use crate::parser::ast::*;
use crate::parser::Parser;

pub fn parse_stmt(parser: &mut Parser) -> Option<Stmt> {
  match parser.peek() {
    Some(Token::Keyword(keyword)) => match keyword {
      Keyword::If => parse_if_stmt(parser),
      Keyword::While => parse_while_stmt(parser),
      Keyword::Continue => parse_continue_stmt(parser),
      Keyword::Break => parse_break_stmt(parser),
      Keyword::Return => parse_return_stmt(parser),
      Keyword::Var => parse_var_decl(parser),
      Keyword::Val => parse_val_decl(parser),
      Keyword::Fun => parse_fun_decl(parser),
      Keyword::Import => parse_import(parser),
      _ => None,
    },
    Some(Token::Tip(_)) => parse_tip(parser),
    Some(Token::Punc(Punc::LeftBrace)) => parse_block_stmt(parser),
    _ => parse_expr_stmt(parser),
  }
}

pub fn parse_if_stmt(parser: &mut Parser) -> Option<Stmt> {
  if parser.eat_tok(Token::Keyword(Keyword::If)).is_some() {
    if let Some(cond) = parse_expr(parser) {
      if let Some(then_stmt) = parse_stmt(parser) {
        return Some(Stmt::If {
          cond: Box::new(cond),
          then_stmt: Box::new(then_stmt),
          else_stmt: parse_else_stmt(parser),
        });
      }
    }
  }

  None
}

pub fn parse_else_stmt(parser: &mut Parser) -> Option<Box<Stmt>> {
  if parser.eat_tok(Token::Keyword(Keyword::Else)).is_some() {
    if let Some(else_stmt) = parse_stmt(parser) {
      return Some(Box::new(else_stmt));
    }
  }

  None
}

pub fn parse_while_stmt(parser: &mut Parser) -> Option<Stmt> {
  if parser.eat_tok(Token::Keyword(Keyword::While)).is_some() {
    if let Some(cond) = parse_expr(parser) {
      if let Some(do_stmt) = parse_stmt(parser) {
        return Some(Stmt::While {
          cond: Some(Box::new(cond)),
          do_stmt: Box::new(do_stmt),
        });
      }
    }
  }

  None
}

pub fn parse_continue_stmt(parser: &mut Parser) -> Option<Stmt> {
  if parser.eat_tok(Token::Keyword(Keyword::Continue)).is_some() {
    return Some(Stmt::Continue);
  }

  None
}

pub fn parse_break_stmt(parser: &mut Parser) -> Option<Stmt> {
  if parser.eat_tok(Token::Keyword(Keyword::Break)).is_some() {
    return Some(Stmt::Break);
  }

  None
}

pub fn parse_return_stmt(parser: &mut Parser) -> Option<Stmt> {
  if parser.eat_tok(Token::Keyword(Keyword::Return)).is_some() {
    if let Some(expr) = parser.maybe(parse_expr) {
      return Some(Stmt::Return(Some(Box::new(expr))));
    } else {
      return Some(Stmt::Return(None));
    }
  }

  None
}

pub fn parse_assign(parser: &mut Parser) -> Option<Expr> {
  if let Some(Token::Operator(op)) = parser.peek() {
    if op.is_assign() {
      parser.step();
      return parse_expr(parser);
    }
  }

  None
}

pub fn parse_var_decl(parser: &mut Parser) -> Option<Stmt> {
  if parser.eat_tok(Token::Keyword(Keyword::Var)).is_some() {
    if let Some(ident_typed) = parse_ident_typed(parser) {
      if let Some(value) = parse_assign(parser) {
        return Some(Stmt::VarDecl {
          ident_typed,
          val: Box::new(value),
        });
      }
    }
  }

  None
}

pub fn parse_val_decl(parser: &mut Parser) -> Option<Stmt> {
  if parser.eat_tok(Token::Keyword(Keyword::Val)).is_some() {
    if let Some(ident_typed) = parse_ident_typed(parser) {
      if let Some(value) = parse_assign(parser) {
        return Some(Stmt::ValDecl {
          ident_typed,
          val: Box::new(value),
        });
      }
    }
  }

  None
}

pub fn parse_tip(parser: &mut Parser) -> Option<Stmt> {
  if let Some(Token::Tip(tip)) = parser.eat_type(Token::Tip(Tip {
    ident: String::new(),
    value: String::new(),
  })) {
    Some(Stmt::Tip(tip.to_owned()))
  } else {
    None
  }
}

pub fn parse_block_stmt(parser: &mut Parser) -> Option<Stmt> {
  if parser.eat_tok(Token::Punc(Punc::LeftBrace)).is_some() {
    let stmts = parser.repeating(parse_stmt);
    if parser.eat_tok(Token::Punc(Punc::RightBrace)).is_some() {
      return Some(Stmt::Block(stmts));
    }
  }

  None
}

pub fn parse_fun_decl(parser: &mut Parser) -> Option<Stmt> {
  if parser.eat_tok(Token::Keyword(Keyword::Fun)).is_some() {
    if let Some(ident) = parse_ident(parser) {
      let params = parser.maybe(parse_params);

      if parser.eat_tok(Token::Punc(Punc::Colon)).is_some() {
        if let Some(ret_type) = parse_ident_type(parser) {
          if let Some(stmt) = parse_stmt(parser) {
            return Some(Stmt::FunDecl {
              ident,
              params,
              ret_type,
              stmt: Box::new(stmt),
            });
          }
        }
      }
    }
  }

  None
}

pub fn parse_params(parser: &mut Parser) -> Option<Vec<IdentTyped>> {
  if parser.eat_tok(Token::Punc(Punc::LeftParen)).is_some() {
    let mut idents = Vec::new();

    if let Some(first) = parse_ident_typed(parser) {
      idents.push(first);

      idents.append(&mut parser.repeating(|parser| {
        if parser.eat_tok(Token::Punc(Punc::Comma)).is_some() {
          parse_ident_typed(parser)
        } else {
          None
        }
      }));
    }

    if parser.eat_tok(Token::Punc(Punc::RightParen)).is_some() {
      return Some(idents);
    }
  }

  None
}

pub fn parse_import(parser: &mut Parser) -> Option<Stmt> {
  if parser.eat_tok(Token::Keyword(Keyword::Import)).is_some() {
    let mut idents = Vec::new();

    if let Some(first) = parse_ident_import(parser) {
      idents.push(first);

      idents.append(&mut parser.repeating(|parser| {
        if parser.eat_tok(Token::Punc(Punc::Comma)).is_some() {
          parse_ident_import(parser)
        } else {
          None
        }
      }));

      parser.eat_tok(Token::Keyword(Keyword::From))?;
    }

    if let Some(Token::StrLit(from)) = parser.eat_tok(Token::StrLit(String::new())) {
      return Some(Stmt::Import {
        idents,
        from: from.to_owned(),
      });
    }
  }

  None
}

pub fn parse_expr_stmt(parser: &mut Parser) -> Option<Stmt> {
  if let Some(expr) = parse_expr(parser) {
    return Some(Stmt::Expr(expr));
  }

  None
}
