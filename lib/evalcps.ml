open Ast
open Cps

type loc = string
type answer = int

and dval =
  Tuple of dval list * int
| Int of int
| Bool of bool
| Fun of (dval list -> answer)

type env = Ast.ident -> dval

type denotation = (env -> answer)

let dv env cval =
  match cval with
    Cps.Int i -> Int i
  | Cps.Bool b -> Bool b
  | Cps.Var v -> env v

let bind (env: env) (v: Ast.ident) d =
  fun w -> if v = w then d else env w

let rec bindn (env: env) vl dl: env = 
  match (vl, dl) with
  (v::vl, d::dl) ->
  bindn(bind env v d) vl dl
| ([], []) -> env
| _ -> failwith "bindn illegael argument"

let field (Ast.Tuple tpl) i = List.nth tpl i

let overflow (n: unit -> int) (c: dval list -> answer) =
  if (n() >= min_int && n() <= max_int) then c [Int (n())] else failwith "lol"

let evalop = function
| Ast.Add -> (+)
| Ast.Sub -> (-)
| Ast.Mul -> ( * )
| Ast.Div -> (/)
| _ -> failwith "todo evalprim"

let evalprim' op [Int i; Int j] [c] : answer = 
  c [(evalop op i j)]

(* let evalprim' _ _ _ = failwith "something wrong" *)


let evalprim = function
| (Ast.Add, [Int i; Int j], [c]) -> overflow (fun () -> i + j) c
| (Ast.Sub, [Int i; Int j], [c]) -> overflow (fun () -> i - j) c
| (Ast.Mul, [Int i; Int j], [c]) -> overflow (fun () -> i * j) c
| (Ast.Div, [Int i; Int j], [c]) -> overflow (fun () -> i / j) c
| _ -> failwith "todo evalprim"

let rec e cexp (env: env) =
  match cexp with
    Cps.App (f, vl) ->
      let Fun g = dv env f in
        g (List.map (dv env) vl) 
  | Cps.Primop (p, vl, wl, el) ->
    let l1 = List.map (dv env) vl in
    let l2 = (List.map (fun e_ al -> e e_ (bindn env wl al)) el) in
    (* evalprim' p l1 l2 *)
    evalprim (p, l1, l2)
  | Cps.Fix _ -> failwith "eval fix"
  | Cps.Halt -> failwith "eval halt"
  | _ -> failwith "todo e"
      
let env0 = fun _ -> failwith "undefined"

let eval vl e_ dl = e e_ (bindn env0 vl dl)
  (* let eval vars cexp dvals  *)