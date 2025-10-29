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

let rec count_calls (cexpr : cexpr) (calls : calls) = 
  match cexpr with
  | Halt -> calls
  | App (v, _) -> count_calls_value v calls
  | Fix (decls, body) -> 
    let bodies = body :: List.map (fun (_, _, body') -> body') decls in
    List.fold_left (fun acc body' -> count_calls body' acc) calls bodies
  | Tuple (vl, _, c) ->
    let calls' = List.fold_left (fun acc (x, _) -> match x with Var id -> incr id acc | _ -> acc) calls vl in
    count_calls c calls'
  | Select (_, v, _, c) -> calls |> count_calls_value v |> count_calls c
  | Primop (_, vs, _, cs) -> 
      let calls' = List.fold_left (fun acc v -> count_calls_value v acc) calls vs in
      List.fold_left (fun acc c -> count_calls c acc) calls' cs

let rm_from_list (x : 'a) (ls : 'a list) = List.filter (fun e -> e != x) ls

let rec beta_contraction (f_env : f_env) (calls : calls) = function
| Halt -> Halt
| App (v, args) -> 
  let id = to_id v in
  let (params, body) = f_env id in
  if calls id == 1 then      
    subst_n params args body
  else
    App (v, args)
| Fix (decls, body) ->
  let f_env' = bind_n f_env decls in
  let calls' = count_calls body calls in
  Fix (decls, beta_contraction f_env' calls' body)
| Tuple (vl, id, c) -> Tuple (vl, id, beta_contraction f_env calls c)
| Select (i, vl, id, c) -> Select (i, vl, id, beta_contraction f_env calls c)
| Primop (op, vl, ids, cs) -> Primop (op, vl, ids, List.map (beta_contraction f_env calls) cs)
  
