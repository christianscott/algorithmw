open Base

type literal = LInt of int | LBool of bool

type expression =
  (* true, 1 *)
  | ELiteral of literal
  (* x *)
  | EVariable of string
  (* Expr Expr *)
  | EApplication of expression * expression
  (* \x -> Expr *)
  | ELambda of string * expression
  (* let x = Expr in Expr *)
  | ELet of string * expression * expression

let rec string_of_expression e =
  match e with
  | ELiteral l -> (
      match l with
      | LInt n -> Int.to_string n
      | LBool b -> Bool.to_string b )
  | EVariable v -> v
  | EApplication (e1, e2) ->
      Printf.sprintf "%s %s" (string_of_expression e1) (string_of_expression e2)
  | ELambda (v, e) -> Printf.sprintf "\\%s -> %s" v (string_of_expression e)
  | ELet (v, e1, e2) ->
      Printf.sprintf "let %s = %s in %s" v (string_of_expression e1)
        (string_of_expression e2)

type concrete_type =
  | TInt
  | TBool
  | TVariable of string
  | TFunction of concrete_type * concrete_type

let rec string_of_type t =
  match t with
  | TInt -> "int"
  | TBool -> "bool"
  | TVariable var -> Printf.sprintf "'%s" var
  | TFunction (t1, t2) ->
      Printf.sprintf "%s -> %s" (string_of_type t1) (string_of_type t2)

(* 
 * quantifiers ???
 *
 * e.g. ∀a. a → a
 *)
type scheme = string list * concrete_type

type free_type_vars = Set.M(String).t

(* a mapping from type variables to concrete types *)
type subst = concrete_type Map.M(String).t

module type Types = sig
  type t

  (* determine the free type variables of t *)
  val free_type_vars : t -> free_type_vars

  (* replace names with concrete types as specified by subst *)
  val apply : subst -> t -> t
end

module Types_concrete_type : Types with type t := concrete_type = struct
  let rec free_type_vars = function
    | TVariable name -> Set.singleton (module String) name
    | TFunction (t1, t2) ->
        let ftv1 = free_type_vars t1 in
        let ftv2 = free_type_vars t2 in
        Set.union ftv1 ftv2
    | TInt | TBool -> Set.empty (module String)

  let rec apply subst t =
    match t with
    | TVariable name -> (
        match Map.find subst name with
        | Some t_nested -> t_nested
        | None -> TVariable name )
    | TFunction (t1, t2) -> TFunction (apply subst t1, apply subst t2)
    | TInt | TBool -> t
end

module Subst = struct
  (* empty substitution *)
  let null = Map.empty (module String)

  (* merge two substitutions *)
  let compose (s1 : subst) (s2 : subst) =
    let subst = Map.map s2 ~f:(Types_concrete_type.apply s1) in
    Map.fold subst ~init:s1 ~f:(fun ~key ~data s ->
        match Map.add s ~key ~data with
        | `Ok a -> a
        | `Duplicate -> s)
end

module Types_scheme : Types with type t := scheme = struct
  let free_type_vars s =
    let _, t = s in
    let ftv1 = Types_concrete_type.free_type_vars t in
    ftv1

  let apply subst s =
    let flip f x y = f y x in
    let vars, t = s in
    (* remove all variables in the scheme from subst before applying *)
    let subst = List.fold_right vars ~f:(flip Map.remove) ~init:subst in
    (vars, Types_concrete_type.apply subst t)
end

(* also called a "context" *)
type type_env = scheme Map.M(String).t

module Types_type_env : Types with type t := type_env = struct
  let free_type_vars env =
    (* extract all schemes *)
    let values = Map.data env in
    (* extract free type variables from all schemes *)
    let ftvs = List.map values ~f:Types_scheme.free_type_vars in
    (* merge all free type vars from all schemes *)
    List.fold ftvs ~f:Set.union ~init:(Set.empty (module String))

  let apply subst env = Map.map ~f:(Types_scheme.apply subst) env
end

module Type_vars : sig
  type t

  val create : unit -> t

  val next : t -> concrete_type
end = struct
  type t = int ref

  let create () = ref (Char.to_int 'a')

  let next s =
    let i = !s in
    Int.incr s;
    TVariable (String.of_char (Char.of_int_exn i))
end

(* a scheme needs to be instantiated to become a type *)
let instantiate tv (vars, t) =
  let type_var = Type_vars.next tv in
  let var_with_type var = (var, type_var) in
  let vars_with_types = List.map ~f:var_with_type vars in
  let subst = Map.of_alist_exn (module String) vars_with_types in
  Types_concrete_type.apply subst t

exception FooBarRenameMe of string

let is_var var t =
  match t with
  | TVariable tt -> String.equal var tt
  | _ -> false

let var_bind var t =
  if is_var var t then Subst.null
  else
    let ftvs = Types_concrete_type.free_type_vars t in
    match Set.find ftvs ~f:(String.equal var) with
    | Some _ -> raise (FooBarRenameMe "AHHHH")
    | None -> Map.singleton (module String) var t

exception NotUnifiable of string

let rec unify t1 t2 =
  match (t1, t2) with
  | TFunction (l1, r1), TFunction (l2, r2) ->
      let subst1 = unify l1 l2 in
      let subst2 =
        unify
          (Types_concrete_type.apply subst1 r1)
          (Types_concrete_type.apply subst1 r2)
      in
      Subst.compose subst1 subst2
  | TVariable u, t -> var_bind u t
  | t, TVariable u -> var_bind u t
  | TInt, TInt | TBool, TBool -> Subst.null
  | t, u ->
      let msg =
        Printf.sprintf "types %s and %s cannot be unified" (string_of_type t)
          (string_of_type u)
      in
      raise (NotUnifiable msg)

exception UndefinedVariable of string

(* infer the type of an expression using a type environment *)
let rec infer_helper tv env expr : subst * concrete_type =
  match expr with
  | ELiteral (LInt _) -> (Subst.null, TInt)
  | ELiteral (LBool _) -> (Subst.null, TBool)
  | EVariable var -> (
      match Map.find env var with
      | Some scheme -> (Subst.null, instantiate tv scheme)
      | None ->
          raise
            (UndefinedVariable (Printf.sprintf "could not find variable %s" var))
      )
  | ELambda (binder, body) ->
      let binder_t = Type_vars.next tv in
      let next_env =
        match Map.add env ~key:binder ~data:([], binder_t) with
        | `Ok e -> e
        | `Duplicate -> env
      in
      let subst, body_t = infer_helper tv next_env body in
      (subst, TFunction (binder_t, body_t))
  | ELet (x, expr, body) ->
      let subst1, expr_t = infer_helper tv env expr in
      let next_env =
        match Map.add env ~key:x ~data:([], expr_t) with
        | `Ok e -> e
        | `Duplicate -> env
      in
      let subst2, body_t =
        infer_helper tv (Types_type_env.apply subst1 next_env) body
      in
      let merged_subst = Subst.compose subst1 subst2 in
      (merged_subst, body_t)
  | EApplication (func, arg) ->
      let result_t = Type_vars.next tv in
      let s1, func_t = infer_helper tv env func in
      let s2, arg_t = infer_helper tv (Types_type_env.apply s1 env) arg in
      let s3 =
        let x = Types_concrete_type.apply s2 func_t in
        let y = TFunction (arg_t, result_t) in
        unify x y
      in
      let subst = Subst.compose s3 (Subst.compose s2 s1) in
      (subst, Types_concrete_type.apply s3 result_t)

let infer expr =
  let tv = Type_vars.create () in
  let _, inferred = infer_helper tv Subst.null expr in
  inferred
