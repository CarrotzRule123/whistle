use crate::compile_stmts;
use crate::ident_type_to_val_type;
use crate::setup_extern;
use crate::Compiler;
use crate::Function;

use wasm_encoder::ConstExpr;
use wasm_encoder::ExportKind;
use wasm_encoder::GlobalType;
use wasm_encoder::Instruction;
use whistle_ast::Expr;
use whistle_ast::IdentExternFn;
use whistle_ast::IdentType;
use whistle_ast::IdentTyped;
use whistle_ast::Literal;
use whistle_ast::Primary;
use whistle_ast::Primitive;
use whistle_ast::ProgramStmt;
use whistle_ast::Span;
use whistle_ast::Stmt;
use whistle_ast::Type;
use whistle_ast::Unary;
use whistle_common::CompilerErrorKind;
use whistle_common::CompilerHandler;

pub fn compile_program(compiler: &mut Compiler, program: ProgramStmt) {
  match program {
    ProgramStmt::Extern {
      idents, namespace, ..
    } => compile_extern(compiler, idents, namespace),
    ProgramStmt::FunctionDecl {
      export,
      inline,
      ident,
      params,
      ret_type,
      stmt,
      ..
    } => compile_fn(compiler, export, inline, ident, params, ret_type, stmt),
    ProgramStmt::ValDecl {
      ident_typed,
      val,
      span,
    } => compile_val(compiler, ident_typed, val, span),
    ProgramStmt::VarDecl {
      ident_typed,
      val,
      span,
    } => compile_var(compiler, ident_typed, val, span),
    ProgramStmt::Import {
      idents: _idents,
      from: _from,
      imp_type: _imp_type,
      ..
    } => {}
    _ => compiler
      .handler
      .throw(CompilerErrorKind::Unimplemented, program.span()),
  }
}

pub fn compile_fn(
  compiler: &mut Compiler,
  export: bool,
  _inline: bool,
  ident: String,
  params: Vec<IdentTyped>,
  ret_type: IdentType,
  stmts: Vec<Stmt>,
) {
  // TODO: Inline functions, would be done with a new field in the Compiler struct
  let sym = compiler.get_sym(&ident).unwrap().clone();
  compiler.scope.enter_curr_scope();

  let mut types = Vec::new();
  for param in params {
    types.push(ident_type_to_val_type(param.type_ident.to_type()));
  }

  let encoded_ret_type = if let IdentType::Primitive { .. } = ret_type {
    vec![]
  } else {
    vec![ident_type_to_val_type(ret_type.to_type())]
  };

  compiler.module.types.function(types, encoded_ret_type);
  compiler.module.fns.function(sym.0);
  if export {
    compiler.module.exports.export(
      if &ident == "main" { "_start" } else { &ident },
      ExportKind::Func,
      sym.0,
    );
  }

  let mut fun = Function::new(ident);
  compile_stmts(compiler, &mut fun, stmts);
  fun.instruction(Instruction::End);
  compiler.module.code.function(&fun.into());
  compiler.scope.exit_scope();
}

pub fn compile_extern(compiler: &mut Compiler, idents: Vec<IdentExternFn>, namespace: String) {
  for external_fn in &idents {
    let types = Type::Function {
      params: IdentTyped::vec_to_type(&external_fn.params),
      ret_type: Box::new(external_fn.ret_type.to_type()),
    };
    setup_extern(compiler, &namespace, external_fn.ident.as_str(), types)
  }
}

pub fn compile_val(compiler: &mut Compiler, ident_typed: IdentTyped, expr: Expr, span: Span) {
  let ident_type = compiler.get_sym(&ident_typed.ident).unwrap();
  let val_type = ident_type_to_val_type(ident_type.1.types.clone());
  let constant = compile_const_expr(compiler, expr, span);
  compiler.module.globals.global(
    GlobalType {
      val_type,
      mutable: false,
    },
    &constant,
  );
}

pub fn compile_var(compiler: &mut Compiler, ident_typed: IdentTyped, expr: Expr, span: Span) {
  let ident_type = compiler.get_sym(&ident_typed.ident).unwrap();
  let val_type = ident_type_to_val_type(ident_type.1.types.clone());
  let constant = compile_const_expr(compiler, expr, span);
  compiler.module.globals.global(
    GlobalType {
      val_type,
      mutable: true,
    },
    &constant,
  );
}

pub fn compile_const_expr(compiler: &mut Compiler, expr: Expr, span: Span) -> ConstExpr {
  if let Expr::Unary {
    unary: Unary::Primary {
      prim: Primary::Literal { lit, meta_id, .. },
      ..
    },
    ..
  } = expr
  {
    match lit {
      Literal::Bool(i) => ConstExpr::i32_const(i as i32),
      Literal::Char(i) => ConstExpr::i32_const(i as i32),
      Literal::F32(i) => ConstExpr::f32_const(i as f32),
      Literal::F64(i) => ConstExpr::f64_const(i),
      Literal::Float(i) => match &compiler.substitutions[meta_id] {
        Type::Primitive(Primitive::F32) => ConstExpr::f32_const(i as f32),
        Type::Primitive(Primitive::F64) => ConstExpr::f64_const(i),
        _ => unreachable!(),
      },
      Literal::I32(i) => ConstExpr::i32_const(i as i32),
      Literal::I64(i) => ConstExpr::i64_const(i as i64),
      Literal::Int(i) => match &compiler.substitutions[meta_id] {
        Type::Primitive(Primitive::I32) => ConstExpr::i32_const(i as i32),
        Type::Primitive(Primitive::I64) => ConstExpr::i64_const(i as i64),
        _ => unreachable!(),
      },
      Literal::None => ConstExpr::i32_const(0),
      Literal::Str(string) => {
        let idx = compiler.memory.stack;
        let bytes = string.as_bytes();
        compiler.memory.buf.extend(bytes);
        compiler.memory.stack += bytes.len() as u64;
        ConstExpr::i64_const(idx as i64)
      },
      Literal::U32(i) => ConstExpr::i32_const(i as i32),
      Literal::U64(i) => ConstExpr::i64_const(i as i64),
    }
  } else {
    compiler
      .handler
      .throw(CompilerErrorKind::GlobalNotLiteral, span.clone());
    ConstExpr::i32_const(0)
  }
}
