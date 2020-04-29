open Base

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

type scheme = string list * concrete_type

type free_type_vars = Set.M(String).t

type subst = concrete_type Base.Map.M(Base.String).t

module type Types = sig
  type t

  val free_type_vars : t -> free_type_vars

  val apply : subst -> t -> t
end

module Types_concrete_type : sig
  val free_type_vars : concrete_type -> free_type_vars

  val apply : subst -> concrete_type -> concrete_type
end

module Subst : sig
  val null : (string, 'a, Base.String.comparator_witness) Base.Map.t

  val compose : subst -> subst -> subst
end

module Types_scheme : sig
  val free_type_vars : scheme -> free_type_vars

  val apply : subst -> scheme -> scheme
end

type type_env = scheme Base.Map.M(Base.String).t

module Types_type_env : sig
  val free_type_vars : type_env -> free_type_vars

  val apply : subst -> type_env -> type_env
end

module TypeGenerator : sig
  val next_variable : int ref

  exception BadChar of string

  val next : unit -> concrete_type
end

val instantiate : string list * concrete_type -> concrete_type

exception FooBarRenameMe of string

val is_var : string -> concrete_type -> bool

val var_bind :
  string ->
  concrete_type ->
  (string, concrete_type, Base.String.comparator_witness) Base.Map.t

val unify : concrete_type -> concrete_type -> subst

exception UndefinedVariable of string

val infer_helper : type_env -> expression -> subst * concrete_type

val infer : expression -> concrete_type

val string_of_type : concrete_type -> string
