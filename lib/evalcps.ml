module StrMap = Map.Make(String)

(* type answer = int *)

type dval =
Tuple of dval list * int
| Int of int
| Bool of bool
| Fun of dval_fun

and dval_fun = dval list -> dval

type env = Ast.ident -> dval

let dv env = function
    Cps.Int i -> Int i
  | Cps.Bool b -> Bool b
  | Cps.Var v -> env v

let bind (env: env) (v: Ast.ident) d =
  (* Printf.printf "bound %s\n" (Prettycps.ident_str v);  *)
  fun w ->
    (* Printf.printf "bind lookup %s %s\n" (Prettycps.ident_str v) (Prettycps.ident_str w); *)
    if v = w then d else env w

let rec bindn (env: env) vl dl: env = 
  match (vl, dl) with
  (v::vl, d::dl) ->
  bindn(bind env v d) vl dl
| ([], []) -> env
| _ -> failwith "bindn illegal argument"

let field i = function
| Ast.Tuple tpl -> List.nth tpl i
| _ -> failwith "only tuples should be reachable for field"

let evalalarith = function
| Ast.Add -> (+)
| Ast.Sub -> (-)
| Ast.Mul -> ( * )
| Ast.Div -> (/)
| _ -> failwith "unreachable evalalarith"

let env0 : (Ast.ident -> dval) = fun v -> Printf.printf "Variable %s not found!\n" (Prettycps.ident_str (v)); failwith "undefined"

let val_to_int env = function
| Cps.Var v ->
  begin match env v with
  | Int i -> i
  | _ -> failwith "unreachable"
  end
| Cps.Int i -> i
| Cps.Bool b -> (if b then 1 else 0)

let rec eval' (env: env) = function
| Cps.Halt v -> begin match v with
  | Cps.Var v -> env v
  | Cps.Int i -> Int i
  | Cps.Bool b -> Bool b
end
| Cps.App (f, vl) ->
  begin match dv env f with
  | Fun g -> g (List.map (dv env) vl)
  | _ -> failwith "f needs to be a function symbol"
  end
| Cps.Primop (p, [a;b], [id], [c]) ->
  let i = val_to_int env a in
  let j = val_to_int env b in
  let op = evalalarith p in
  let env = bind env id (Int (op i j)) in
  eval' env c
| Cps.Fix (funs,c) ->
  let rec bindargs r1 (_,vl,b) =
    Fun (fun al ->
      eval' (bindn (updateenv r1) vl al) b)
  and updateenv r =
    bindn r (List.map ((fun (f,_,_) -> f)) funs) (List.map (bindargs r) funs) in
  eval' (updateenv env) c
| e -> Printf.printf "\ne: %s\n" (Prettycps.cps_ast_repr e);failwith "e"


let eval vl e_ dl = eval' (bindn env0 vl dl) e_