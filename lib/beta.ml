open Cps

type answer = int
and dval =
  Tuple of dval list * int
| Int of int
| Bool of bool
| Fun of (dval list -> answer)

type f_env = Ast.ident -> (Ast.ident list * cexpr)
type calls = Ast.ident -> int

let f_env0 _ = failwith "undefined"
let calls0 _ = 0

let string_of_ident (Ast.Ident p) = p
let string_of_val = function
  | Var v -> string_of_ident v
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b

let subst_val param arg = function
  | Var id -> if id = param then arg else Var id
  | Int i -> Int i
  | Bool b -> Bool b

(** Precondition: `param` and `f_params` should be disjoint *)
let rec subst_decl params args (id, f_params, cexp) = (id, f_params, subst params args cexp)

and subst param arg cexpr =
  match cexpr with
  | Halt v -> Halt (subst_val param arg v)
  | App (f, args) -> App (subst_val param arg f, List.map (subst_val param arg) args)
  | Fix (decls, body) -> Fix (List.map (subst_decl param arg) decls, subst param arg body)
  | Tuple (vl, id, c) -> Tuple (List.map (fun (v, i) -> (subst_val param arg v, i)) vl, id, subst param arg c)
  | Select (i, v, id, c) -> Select (i, subst_val param arg v, id, subst param arg c)
  | Primop (op, vs, ids, cs) ->
    Primop (op, List.map (subst_val param arg) vs, ids, List.map (subst param arg) cs)
  | Switch (i, cs) -> Switch (i, List.map(subst param arg) cs)


let subst_n (params : Ast.ident list) (args : value list) (body : cexpr) =
  List.combine params args |>
  List.fold_left (fun acc (param, arg) -> subst param arg acc) body

let bind (f_env : f_env) (v : Ast.ident) (f : Ast.ident list * cexpr) =
  fun w -> if v = w then f else f_env w

let bind_n = List.fold_left (fun acc (f, params, cexpr) -> bind acc f (params, cexpr))

let incr (id : Ast.ident) (calls : calls) =
  fun w -> if id = w then calls w + 1 else calls w

let decr (id : Ast.ident) (calls : calls) =
  fun w ->
    let count = calls w in
    if count < 1 then
      failwith "attempting to decrement call count that is less than 1"
    else
      if id = w then calls w - 1 else calls w

let combine_calls (x : calls) (y : calls) = fun w -> (x w) + (y w)
let (++) = combine_calls


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


let count_calls_value (v : value) (calls : calls) =
  match v with
  | Var id -> incr id calls
  | Int _ -> calls
  | Bool _ -> calls

let count_calls (cexpr : cexpr) =
  let rec count_calls_aux (cexpr : cexpr) (calls : calls) = 
    match cexpr with
    | Halt _ -> calls
    | App (v, _) -> count_calls_value v calls
    | Fix (decls, body) ->
      let bodies = body :: List.map (fun (_, _, body') -> body') decls in
      List.fold_left (fun acc body' -> count_calls_aux body' acc) calls bodies
    | Tuple (_, _, c) ->
      count_calls_aux c calls
    | Select (_, _, _, c) -> calls |> count_calls_aux c
    | Primop (_, _, _, cs) ->
      List.fold_left (fun acc c -> count_calls_aux c acc) calls cs
    | Switch (_, cs) ->
      List.fold_left (fun acc c -> count_calls_aux c acc) calls cs in
  count_calls_aux cexpr calls0


let rm_from_list (x : 'a) (ls : 'a list) = List.filter (fun e -> e != x) ls

let beta (f_env : f_env) (cexpr : cexpr) =
  let calls = count_calls cexpr in
  let rec beta_aux (f_env : f_env) = function
  | Halt v -> Halt v
  | App (v, args) ->
    let id = to_id v in
    let (params, body) = f_env id in

    if calls id = 1 then
      beta_aux f_env (subst_n params args body)
    else
      App (v, args)
  | Fix (decls, body) ->
    let f_env' = bind_n f_env decls in
    Fix (decls, beta_aux f_env' body)
  | Tuple (vl, id, c) -> Tuple (vl, id, beta_aux f_env c)
  | Select (i, vl, id, c) -> Select (i, vl, id, beta_aux f_env c)
  | Primop (op, vl, ids, cs) -> Primop (op, vl, ids, List.map (beta_aux f_env) cs)
  | Switch (i, cs) -> Switch (i, List.map (beta_aux f_env) cs) in
  beta_aux f_env cexpr

let occurs_in_val id = function
| Var v -> v = id
| Int _ -> false
| Bool _ -> false

let fixpoint f x =
  let rec fp acc f x = if x = acc then acc else fp (f acc) f (f x) in
  fp (f x) f x

let rec occurs id = function
| Halt v -> occurs_in_val id v
| App (v, args) -> List.exists (occurs_in_val id) (v :: args)
| Fix (decls, body) -> List.exists (fun (_, _, b) -> occurs id b) decls || occurs id body
| Tuple (vl, id, c) -> List.exists (fun (v, _) -> occurs_in_val id v) vl || occurs id c
| Select (_, vl, id, c) -> occurs_in_val id vl || occurs id c
| Primop (_, vl, _, cs) -> List.exists (occurs_in_val id) vl || List.exists (occurs id) cs
| Switch (i, cs) -> occurs_in_val id i || List.exists (occurs id) cs

let rec dead_fix = function
| Halt v -> Halt v
| App (v, args) -> App (v, args)
| Fix (decls, body) ->
  let decls' = List.filter (fun (id, _, _) ->
    let occurs_in_decl = List.exists (fun (_, _, b) -> occurs id b) decls in
    let occurs_in_body = occurs id body in
    occurs_in_decl || occurs_in_body
  ) decls in

  if List.is_empty decls' then
      dead_fix body
  else 
      Fix (decls', dead_fix body)
| Tuple (vl, id, c) -> Tuple (vl, id, dead_fix c)
| Select (i, vl, id, c) -> Select (i, vl, id, dead_fix c)
| Primop (op, vl, ids, cs) -> Primop (op, vl, ids, List.map dead_fix cs)
| Switch (i, cs) -> Switch (i, List.map dead_fix cs)

let beta_contract = fixpoint (fun x -> x |> beta f_env0 |> dead_fix)
