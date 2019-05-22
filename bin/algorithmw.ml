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
    let (vars, t) = s in
    let ftv1 = Types_concrete_type.free_type_vars t in
    let ftv2 = Set.of_list (module String) vars in
    Set.diff ftv1 ftv2

  let apply subst s = 
    let (vars, t) = s in
    let subst = List.fold_right vars ~f:(flip Map.remove) ~init:subst in
    (vars, (Types_concrete_type.apply subst t))
end

module Types_list (T : Types) : Types = struct
  type t = T.t list
  
  let free_type_vars ts =
    let ftvs = List.map ~f:T.free_type_vars ts in
    List.fold_right ~f:Set.union ~init:(Set.empty (module String)) ftvs

  let apply subst = List.map ~f:(T.apply subst)
end
