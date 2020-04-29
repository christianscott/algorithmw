type literal = LInt of int | LBool of bool

type expression =
  | ELiteral of literal
  | EVariable of string
  | EApplication of expression * expression
  | ELambda of string * expression
  | ELet of string * expression * expression

type concrete_type =
  | TInt
  | TBool
  | TVariable of string
  | TFunction of concrete_type * concrete_type

val infer : expression -> concrete_type

val string_of_type : concrete_type -> string
