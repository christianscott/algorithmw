let printf = Printf.printf

open Base

let flip f x y = f y x

type literal =
  | LInt of int
  | LBool of bool

type expression =
  | EVariable of string
  | ELiteral of string
  | EApplication of expression * expression
  | EAbstraction of string * expression
  | ELet of string * expression * expression

type concrete_type =
  | TInt
  | TBool
  | TVariable of string
  | TFunction of concrete_type * concrete_type

type scheme = string list * concrete_type

type free_type_vars = Set.M(String).t

type subst = concrete_type Map.M(String).t

module type Types = sig
  type t
  val free_type_vars : t -> free_type_vars
  val apply : subst -> t -> t
end

module Types_concrete_type : Types with type t := concrete_type = struct
  let rec free_type_vars = function
  | TVariable (name) -> Set.singleton (module String) name
  | TFunction (t1, t2) ->
    let ftv1 = free_type_vars t1 in
    let ftv2 = free_type_vars t2 in
    Set.union ftv1 ftv2
  | TInt|TBool -> Set.empty (module String)

  let rec apply subst t = match t with
  | TVariable (name) ->
    (match Map.find subst name with
    | Some (t_nested) -> t_nested
    | None -> TVariable name)
  | TFunction (t1, t2) -> TFunction ((apply subst t1), (apply subst t2))
  | TInt|TBool -> t
end

module Types_scheme : Types with type t := scheme = struct
  let free_type_vars s =
    let (_, t) = s in
    let ftv1 = Types_concrete_type.free_type_vars t in
    ftv1

  let apply subst s = 
    let (vars, t) = s in
    let subst = List.fold_right vars ~f:(flip Map.remove) ~init:subst in
    (vars, (Types_concrete_type.apply subst t))
end

module Subst = struct 
  let null = Map.empty (module String)

  let compose (s1 : subst) (s2 : subst) =
    let subst = Map.map s2 ~f:(Types_concrete_type.apply s1) in 
    Map.fold subst ~init:s1 ~f:(fun ~key ~data s ->
      match Map.add s ~key ~data with
      | `Ok (a) -> a
      | `Duplicate -> s)
end

type type_env = scheme Map.M(String).t

module Types_type_env : Types with type t := type_env = struct
  let free_type_vars env =
    let values = Map.data env in
    let ftvs = List.map values ~f:Types_scheme.free_type_vars in
    List.fold ftvs ~f:Set.union ~init:(Set.empty (module String))

  let apply subst env = Map.map ~f:(Types_scheme.apply subst) env
end

let generalize env t : scheme =
  let ftv_t = Types_concrete_type.free_type_vars t in
  let ftv_env = Types_type_env.free_type_vars env in
  let vars = Set.to_list (Set.diff ftv_t ftv_env) in
  (vars, t)

let rec unify t1 t2 = match (t1, t2) with
| (TFunction (l1, r1), TFunction (l2, r2)) ->
  let subst1 = unify l1 l2 in
  let subst2 = unify (Types_concrete_type.apply subst1 r1) (Types_concrete_type.apply subst1 r2) in
  Subst.compose subst1 subst2
| (TInt, TInt)|(TBool, TBool)|_ -> Subst.null
