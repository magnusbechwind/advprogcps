open Cps

let compose fs = List.fold_left (Fun.flip Fun.compose) Fun.id fs

let fix fs x =
  let f = compose fs in
  let rec fix_aux acc f x = if x = acc then acc else fix_aux (f acc) f (f x) in
  fix_aux (f x) f x

let subst_val param arg = function
  | Var id -> if id = param then arg else Var id
  | Int i -> Int i
  | Bool b -> Bool b
  | String s -> String s

(** Precondition: `param` and `f_params` should be disjoint *)
let rec subst_decl params args (id, f_params, cexp) = (id, f_params, subst params args cexp)

and subst param arg cexpr =
  match cexpr with
  | Halt v -> Halt (subst_val param arg v)
  | App (f, args) -> App (subst_val param arg f, List.map (subst_val param arg) args)
  | Fix (decls, body) -> Fix (List.map (subst_decl param arg) decls, subst param arg body)
  | Tuple (vl, id, c) -> Tuple (List.map (fun v -> subst_val param arg v) vl, id, subst param arg c)
  | Select (i, v, id, c) -> Select (i, subst_val param arg v, id, subst param arg c)
  | Primop (op, vs, ids, cs) ->
    Primop (op, List.map (subst_val param arg) vs, ids, List.map (subst param arg) cs)
  | If (i, cs) -> If (subst_val param arg i, List.map(subst param arg) cs)


let subst_n (params : Ast.ident list) (args : value list) (body : cexpr) =
  List.combine params args |>
  List.fold_left (fun acc (param, arg) -> subst param arg acc) body

