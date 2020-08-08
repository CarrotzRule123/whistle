#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
  LogAndAssign,
  LogOrAssign,
  LogAnd,
  LogOr,
  LogNot,

  BitLeftShiftAssign,
  BitRightShiftAssign,
  BitLeftShift,
  BitRightShift,
  BitAndAssign,
  BitOrAssign,
  BitXorAssign,
  BitAnd,
  BitOr,
  BitXor,
  BitNot,

  AddAssign,
  SubAssign,
  MulAssign,
  DivAssign,
  ModAssign,
  ExpAssign,
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  Exp,

  Eq,
  NotEq,
  LessThanOrEq,
  GreaterThanOrEq,
  LessThan,
  GreaterThan,

  Assign,
}

impl Operator {
  pub fn from(op: &str) -> Option<Operator> {
    match op {
      "&&=" => Some(Operator::LogAndAssign),
      "||=" => Some(Operator::LogOrAssign),
      "&&" => Some(Operator::LogAnd),
      "||" => Some(Operator::LogOr),
      "!" => Some(Operator::LogNot),

      "+=" => Some(Operator::AddAssign),
      "-=" => Some(Operator::SubAssign),
      "*=" => Some(Operator::MulAssign),
      "/=" => Some(Operator::DivAssign),
      "%=" => Some(Operator::ModAssign),
      "**=" => Some(Operator::ExpAssign),
      "+" => Some(Operator::Add),
      "-" => Some(Operator::Sub),
      "*" => Some(Operator::Mul),
      "/" => Some(Operator::Div),
      "%" => Some(Operator::Mod),
      "**" => Some(Operator::Exp),

      "<<=" => Some(Operator::BitLeftShiftAssign),
      ">>=" => Some(Operator::BitRightShiftAssign),
      "<<" => Some(Operator::BitLeftShift),
      ">>" => Some(Operator::BitRightShift),
      "&=" => Some(Operator::BitAndAssign),
      "|=" => Some(Operator::BitOrAssign),
      "^=" => Some(Operator::BitXorAssign),
      "&" => Some(Operator::BitAnd),
      "|" => Some(Operator::BitOr),
      "^" => Some(Operator::BitXor),
      "~" => Some(Operator::BitNot),

      "==" => Some(Operator::Eq),
      "!=" => Some(Operator::NotEq),
      "<=" => Some(Operator::LessThanOrEq),
      ">=" => Some(Operator::GreaterThanOrEq),
      "<" => Some(Operator::LessThan),
      ">" => Some(Operator::GreaterThan),

      "=" => Some(Operator::Assign),

      _ => None,
    }
  }

  pub fn is_unary(&self) -> bool {
    vec![Operator::BitNot, Operator::LogNot, Operator::Sub].contains(self)
  }

  pub fn is_binary(&self) -> bool {
    vec![
      Operator::LogAndAssign,
      Operator::LogOrAssign,
      Operator::LogAnd,
      Operator::LogOr,
      Operator::BitLeftShiftAssign,
      Operator::BitRightShiftAssign,
      Operator::BitLeftShift,
      Operator::BitRightShift,
      Operator::BitAndAssign,
      Operator::BitOrAssign,
      Operator::BitXorAssign,
      Operator::BitAnd,
      Operator::BitOr,
      Operator::BitXor,
      Operator::AddAssign,
      Operator::SubAssign,
      Operator::MulAssign,
      Operator::DivAssign,
      Operator::ModAssign,
      Operator::ExpAssign,
      Operator::Add,
      Operator::Sub,
      Operator::Mul,
      Operator::Div,
      Operator::Mod,
      Operator::Exp,
      Operator::Eq,
      Operator::NotEq,
      Operator::LessThanOrEq,
      Operator::GreaterThanOrEq,
      Operator::LessThan,
      Operator::GreaterThan,
      Operator::Assign,
    ]
    .contains(self)
  }

  pub fn is_assign(&self) -> bool {
    vec![Operator::Assign].contains(self)
  }
}
