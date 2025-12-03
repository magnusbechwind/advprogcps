(** Name generator for introducing new variables; returns a higher-order function that will generate names in increasing order for the CPS transformation *)
let fresh arg =
  let i = ref 0 in
    fun _: (Ast.ident) ->
      let n = !i in i := n + 1; Ast.Ident (arg ^ (string_of_int n))

let fresh_fun = fresh "f"

let fresh_var = fresh "v"

let fresh_cont = fresh "k"

(** Described on page 56 in Chapter 5.2 of Compiling with Continuations *)
let rec tuple_cps tpl (c: Cps.value list -> Cps.cexpr) =
  let rec g r (w: Cps.value list) =
    match r with
      t :: ts -> to_cps t (fun v -> g ts (v :: w))
    | [] -> c (List.rev w)
  in g tpl []

(** Our version of the 'F' function used in Chapter 5 of Compiling with Continations. Given a Lambda language expression and a continuation function (mapping CPS values to CPS expression), produce a CPS expression *)
and to_cps (exp: Ast.expr) (c: Cps.value -> Cps.cexpr) = 
match exp with
| Ast.Var v -> c (Cps.Var v)
| Ast.Int i -> c (Cps.Int i)
| Ast.Bool b -> c (Cps.Bool b)
| Ast.String str -> c (Cps.String str)
| Ast.Tuple [] -> c (Cps.Int 0)
| Ast.Tuple tpl -> 
  let x = fresh_cont () in
  let lambda a = Cps.Tuple (List.map (fun v -> v) a, x, c (Cps.Var x)) in
  tuple_cps tpl lambda
| Ast.Select (i, e) -> 
  let w = fresh_cont() in
  let select = fun v -> Cps.Select (i, v, w, c (Cps.Var w)) in
  to_cps e select
| Ast.Fn (v,e) ->
  let f = fresh_fun () in
  let k = fresh_cont () in
  let ap = fun z -> Cps.App(Cps.Var k, [z]) in
  Cps.Fix ([f, [v; k], to_cps e ap], c (Cps.Var f))

  (* We need to distinguish between multiple cases for the first argument to App *)
| Ast.App(Ast.Primop Callcc, f) ->
  (* [[callcc a]] = λk. [[a]] (λf . f k k) from https://xavierleroy.org/mpri/2-4/transformations.2up.pdf (not used) *)
  let k = fresh_cont() in
  let k' = fresh_cont() in
  let x = fresh_var() in
  Cps.Fix([k, [x], c (Cps.Var x); (k', [x], c (Cps.Var x))], to_cps f (fun v -> Cps.App(v, [Cps.Var k; Cps.Var k'])))

| Ast.App(Ast.Primop Throw, Ast.Tuple [a;b]) ->
  (* [[throw a b]] = λk. [[a]] (λv_a. [[b]] (λv_b. v_a v_b)) from https://xavierleroy.org/mpri/2-4/transformations.2up.pdf *)
  let a = to_cps a in
  let b = to_cps b in
  a (fun v_a -> b (fun v_b -> Cps.App(v_a, [v_b])))
  (* book description *)
| Ast.App(Ast.Primop Throw, e) ->
  let f = fresh_fun () in 
  let x = fresh_var() in 
  let j = fresh_var() in
  to_cps e (fun k -> Cps.Fix([f, [x;j], Cps.App(k, [Cps.Var x])], c (Cps.Var f)))
| Ast.App (Ast.Primop i, Ast.Tuple tpl) ->
  let w = fresh_cont () in
  tuple_cps tpl (fun a -> Cps.Primop (i, a, [w], [c (Cps.Var w)]))
| Ast.App (Ast.Primop i, e) ->
  let w = fresh_cont () in
  to_cps e (fun v -> Cps.Primop (i, [v], [w], [c (Cps.Var w)]))
| Ast.App (f,e) ->
  let r = fresh_cont () in
  let x = fresh_var () in
  let lambda = fun f_ ->
    let inner = fun e_ -> Cps.App(f_, [e_; Cps.Var r]) in
    to_cps e inner in
  Cps.Fix([r, [x], c (Cps.Var x)], to_cps f lambda)
| Ast.Fix(_, _) ->
    failwith "this AST node is not used and therefore this case should be impossible"
 (* Does not follow from Compiling with Continuations, but done with inspiration from how the 'boxed' predicate is translated in Chapter 5.3 *)
| Ast.IfEl (cond, e1, e2) ->
  let ifb = fun b -> Cps.Switch (b, [to_cps e1 c; to_cps e2 c]) in
  to_cps cond ifb
| e -> failwith (Printf.sprintf "Missing case in to_cps: %s" (PrintBox_text.to_string (Pretty.program_to_tree e)))

  (** Given a Lambda language expression, translate it to CPS and apply the identity function (Halt) to extract it to a value *)
let cps exp =
  to_cps exp (fun v -> Halt v)