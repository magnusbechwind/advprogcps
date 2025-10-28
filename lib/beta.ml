open Cps

type answer = int
and dval =
  Tuple of dval list * int
| Int of int
| Bool of bool
| Fun of (dval list -> answer)

type f_env = Ast.ident -> (Ast.ident list * cexpr)

let f_env0 _ = failwith "undefined"

let subst_val param arg = function
  | Var id -> if id == param then arg else Var id
  | Int i -> Int i
  | Bool b -> Bool b

let rec subst param arg = function
  | Halt -> Halt
  | App (f, args) -> App (subst_val param arg f, List.map (subst_val param arg) args)
  | Fix (decls, body) -> failwith "todo" (* make sure we don't subst var with a new var bound by decl!!! *)
  | Tuple (vl, id, c) -> Tuple (List.map (fun (v, i) -> (subst_val param arg v, i)) vl, id, subst param arg c)
  | Select (i, v, id, c) -> failwith "todo"
  | Primop _ -> failwith "todo"

let subst_n (params : Ast.ident list) (args : value list) (body : cexpr) =
  List.combine params args |>
  List.fold_left (fun acc (param, arg) -> subst param arg acc) body

let bind (f_env : f_env) (v : Ast.ident) (f : Ast.ident list * cexpr) =
  fun w -> if v = w then f else f_env w

let bind_n = List.fold_left (fun acc (f, params, cexpr) -> bind acc f (params, cexpr))

let to_ids_opt args = 
  let rec inner acc vl = match (acc, vl) with 
  | Some acc', Var id :: rest -> inner (Some (id :: acc')) rest
  | _, _ :: _ -> None
  | Some acc', [] -> Some acc'
  | None, [] -> None in
  let rev = inner (Some []) args in
  Option.map List.rev rev

let to_id = function
  | Var id -> id
  | _ -> failwith "expected var"

let rec to_beta (f_env : f_env) (cexpr: Cps.cexpr) =
  match cexpr with
  | Halt -> Halt
  | App (v, args) -> App (v, args)
  | Fix (decls, body) ->
    let f_env' = bind_n f_env decls in
    Fix (decls, to_beta f_env' body)
  | Tuple (vl, id1, App (v, [Var id2])) ->
      let (args, c') = f_env (to_id v) in
      Tuple (vl, id1, c')
  | Tuple (vl, id, c) -> Tuple (vl, id, to_beta f_env c)
  | Select (i, vl, id, c) -> Select (i, vl, id, to_beta (f_env) c)
  | Primop (op, vl, ids, cs) -> Primop (op, vl, ids, List.map (to_beta f_env) cs)
  
