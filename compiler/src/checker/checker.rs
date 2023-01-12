use crate::ScopeContainer;
use whistle_common::CompilerErrorKind;
use whistle_common::CompilerHandler;
use whistle_common::DiagnosticHandler;
use whistle_parser::Parser;

use whistle_ast::Operator;
use whistle_ast::Primitive;
use whistle_ast::Type;
use whistle_common::Span;

#[derive(Debug, Clone)]
pub struct Constraint {
  pub type1: Type,
  pub type2: Type,
  pub span: Option<Span>,
}

pub struct Checker {
  pub scope: ScopeContainer,
  pub substitutions: Vec<Type>,
  pub constraints: Vec<Constraint>,
  pub handler: DiagnosticHandler,
}

impl Checker {
  pub fn new(parser: Parser) -> Self {
    Self {
      scope: ScopeContainer::new(),
      substitutions: Vec::new(),
      constraints: Vec::new(),
      handler: parser.handler,
    }
  }

  pub fn constraint(&mut self, type1: Type, type2: Type, span: Option<Span>) {
    self.constraints.push(Constraint { type1, type2, span })
  }

  pub fn unify(&mut self, constraint: Constraint) {
    let Constraint { type1, type2, span } = constraint;
    match self.unify_base(type1, type2) {
      Ok(()) => return,
      Err((type1, type2)) => {
        let err = CompilerErrorKind::TypeMismatch {
          type1: self.substitute(type1),
          type2: self.substitute(type2),
        };
        self.handler.throw(err, span.unwrap());
      }
    }
  }

  pub fn unify_base(&mut self, type1: Type, type2: Type) -> Result<(), (Type, Type)> {
    // println!("{:?}\n", self.substitutions);
    // println!("Constraint {:?}, {:?}", type1, type2);
    let base1 = self.base_type(type1.clone());
    let base2 = self.base_type(type2.clone());
    // println!("Base {:?}, {:?}", base1, base2);

    match (self.base_val(base1.clone()), self.base_val(base2.clone())) {
      (Type::Array(type1), Type::Array(type2)) => match self.unify_base(*type1, *type2) {
        Ok(()) => return Ok(()),
        Err((type1, type2)) => {
          return Err((Type::Array(Box::new(type1)), Type::Array(Box::new(type2))))
        }
      },
      _ => {}
    }

    let err = match (base1, base2) {
      (Type::Var(i), Type::Var(j)) => {
        let type1 = self.substitutions[i].clone();
        let type2 = self.substitutions[j].clone();
        if Checker::is_subtype(&type1, &type2) {
          return Ok(self.substitutions[j] = Type::Var(i));
        } else if Checker::is_subtype(&type2, &type1) {
          return Ok(self.substitutions[i] = Type::Var(j));
        }
        (type1, type2)
      }
      (Type::Var(i), type2) => {
        let type1 = self.substitutions[i].clone();
        if Checker::is_subtype(&type1, &type2) {
          return Ok(());
        } else if Checker::is_subtype(&type2, &type1) {
          return Ok(self.substitutions[i] = type2);
        }
        (type1, type2)
      }
      (type1, Type::Var(j)) => {
        let type2 = self.substitutions[j].clone();
        if Checker::is_subtype(&type1, &type2) {
          return Ok(self.substitutions[j] = type1);
        } else if Checker::is_subtype(&type2, &type1) {
          return Ok(());
        }
        (type1, type2)
      }
      (type1, type2) => {
        if Checker::is_subtype(&type1, &type2) {
          return Ok(());
        } else if Checker::is_subtype(&type2, &type1) {
          return Ok(());
        }
        (type1, type2)
      }
    };

    Err(err)
  }

  pub fn coerce(types: Type) -> Type {
    match types {
      Type::Primitive(Primitive::Int) => Type::Primitive(Primitive::I32),
      Type::Primitive(Primitive::Float) => Type::Primitive(Primitive::F64),
      Type::Primitive(Primitive::Number) => Type::Primitive(Primitive::I32),
      Type::Var(_) => Type::Error,
      Type::Array(arr) => Type::Array(Box::new(Checker::coerce(*arr))),
      _ => types,
    }
  }

  pub fn new_type_val(&mut self) -> Type {
    let res = Type::Var(self.substitutions.len());
    self.substitutions.push(res.clone());
    res
  }

  pub fn base_type(&self, types: Type) -> Type {
    if let Type::Var(i) = types {
      if let Type::Var(j) = self.substitutions[i] {
        if i != j {
          return self.base_type(self.substitutions[i].clone());
        }
      }
    }
    types
  }

  pub fn base_val(&self, types: Type) -> Type {
    if let Type::Var(i) = types.clone() {
      return self.substitutions[i].clone()
    }
    types
  }

  pub fn substitute(&self, types: Type) -> Type {
    if let Type::Var(i) = types {
      if Type::Var(i) == self.substitutions[i] {
        return types;
      }
      return self.substitute(self.substitutions[i].clone());
    } else if let Type::Array(arr) = types {
      return Type::Array(Box::new(self.substitute(*arr)));
    }
    types
  }

  pub fn is_subtype(type1: &Type, type2: &Type) -> bool {
    if let Type::Var(_) = type1 {
      return false;
    }

    if type1 == type2 {
      return true;
    }

    match type2 {
      Type::Primitive(prim) => match prim {
        Primitive::Number => match type1 {
          Type::Primitive(Primitive::I32)
          | Type::Primitive(Primitive::I64)
          | Type::Primitive(Primitive::U32)
          | Type::Primitive(Primitive::U64)
          | Type::Primitive(Primitive::F32)
          | Type::Primitive(Primitive::F64)
          | Type::Primitive(Primitive::Int)
          | Type::Primitive(Primitive::Float) => true,
          _ => false,
        },
        Primitive::Int => match type1 {
          Type::Primitive(Primitive::I32)
          | Type::Primitive(Primitive::I64)
          | Type::Primitive(Primitive::U32)
          | Type::Primitive(Primitive::U64) => true,
          _ => false,
        },
        Primitive::Float => match type1 {
          Type::Primitive(Primitive::F32) | Type::Primitive(Primitive::F64) => true,
          _ => false,
        },
        _ => false,
      },
      Type::Var(_) => true,
      _ => false,
    }
  }
}

pub fn binary_to_type_val(op: &Operator) -> Type {
  match op {
    Operator::Mod
    | Operator::ModAssign
    | Operator::BitAnd
    | Operator::BitAndAssign
    | Operator::BitOr
    | Operator::BitOrAssign
    | Operator::BitXor
    | Operator::BitXorAssign
    | Operator::BitLeftShift
    | Operator::BitLeftShiftAssign
    | Operator::BitRightShift
    | Operator::BitRightShiftAssign => Type::Primitive(Primitive::Int),

    Operator::LogAnd | Operator::LogAndAssign | Operator::LogOr | Operator::LogOrAssign => {
      Type::Primitive(Primitive::Bool)
    }

    Operator::Eq | Operator::NotEq => Type::Default,

    _ => Type::Primitive(Primitive::Number),
  }
}

pub fn unary_to_type_val(op: &Operator) -> Type {
  match op {
    Operator::LogNot => Type::Primitive(Primitive::Bool),

    Operator::BitNot => Type::Primitive(Primitive::Int),

    Operator::Sub => Type::Primitive(Primitive::Number),

    _ => unreachable!(),
  }
}
