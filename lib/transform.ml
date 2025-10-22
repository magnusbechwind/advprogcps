open Ast
open Cps

let fresh_var =
  let i = ref 0 in
  fun _: (Ast.ident) ->
    let n = !i in i := n + 1; Ast.Ident ("v" ^ (string_of_int n))

let fresh_cont =
  let i = ref 0 in
  fun _: (Ast.ident) ->
    let n = !i in i := n + 1; Ast.Ident ("k" ^ (string_of_int n))

let rec tuple_cps tpl (c: Cps.value list -> Cps.cexpr) =
  let rec g r (w: Cps.value list) =
    match r with
      t :: ts -> to_cps t (fun v -> g ts (v :: w))
    | [] -> c (List.rev w)
  in g tpl []

and to_cps (exp: Ast.expr) (c: Cps.value -> Cps.cexpr) = 
  match exp with
  | Ast.Var v -> c (Cps.Var v)
  | Ast.Int i -> c (Cps.Int i)
  | Ast.Bool b -> c (Cps.Bool b)
  | Ast.Tuple [] -> c (Cps.Int 0)
  | Ast.Tuple tpl -> 
    let x = fresh_cont () in
    let lambda a = Cps.Tuple ( (List.map (fun v -> (v, 0)) a, x, c (Cps.Var x))) in
    tuple_cps tpl lambda
  | Ast.Select (i, e) -> 
    let w = fresh_cont() in
    let select = fun v -> Cps.Select (i, v, w, c (Cps.Var w)) in
    to_cps e select
  | Ast.Fn (v,e) ->
    let f = fresh_cont () in
    let k = fresh_var () in
    let ap z = Cps.App(Cps.Var k, [z]) in
    Cps.Fix ([f, [v; k], to_cps e ap], c (Cps.Var f))
      (* We need to distinguish between multiple cases for the first argument to App *)

  | Ast.App (Ast.Primop i, Ast.Tuple tpl) ->
    let w = fresh_cont () in
    tuple_cps tpl (fun a -> Cps.Primop (i, a, [w], [c (Cps.Var w)]))
  | Ast.App (Ast.Primop i, e) ->
    let w = fresh_cont () in
    to_cps e (fun v -> Cps.Primop (i, [v], [w], [c (Cps.Var w)]))
  | Ast.App (f,e) ->
    let r = fresh_var () in
    let x = fresh_cont () in
    let lambda = fun f_ ->
      let inner = fun e_ -> Cps.App(f_, [e_; Cps.Var r]) in
      to_cps e inner in
    Cps.Fix([r, [x], c (Cps.Var x)], to_cps f lambda)
  | _ -> failwith "Missing cases in to_cps"

let cps exp =
  to_cps exp (fun _ -> Halt)