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
| Tuple (tpl, _) -> List.nth tpl i
| _ -> failwith "only tuples should be reachable for field"

let evalarith = function
| Ast.Add -> (+)
| Ast.Sub -> (-)
| Ast.Mul -> ( * )
| Ast.Div -> (/)
| _ -> failwith "unreachable evalarith"

let evalbool = function
| Ast.Eq -> (=)
| Ast.Lt -> (<)
| _ -> failwith "unreachable evalbool"

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
  let env = begin match p with
  | Ast.Add | Sub | Mul | Div -> 
  let i = val_to_int env a in
  let j = val_to_int env b in
  let op = evalarith p in
  bind env id (Int (op i j))
  | Ast.Eq | Ast.Lt -> 
  let i = val_to_int env a in
  let j = val_to_int env b in
  let op = evalbool p in
  bind env id (Bool (op i j))
  | _ -> failwith "todo rest of primops in primop case of evalcps"
  end in
  eval' env c
| Cps.Primop (Ast.Callcc, [f], [k],[c]) ->
  
  failwith "missing calcc"
| Cps.Primop _ -> failwith "missing primop cases"
| Cps.Fix (funs,c) ->
  let rec bindargs r1 (_,vl,b) =
    Fun (fun al ->
      eval' (bindn (updateenv r1) vl al) b)
  and updateenv r =
    bindn r (List.map ((fun (f,_,_) -> f)) funs) (List.map (bindargs r) funs) in
  eval' (updateenv env) c
| Cps.Tuple (vl, id, c) ->
  let vl' = Tuple (List.map (fun (v, i) -> dv env v) vl, List.length vl) in
  let env' = bind env id vl' in
  eval' env' c
| Cps.Select (i, v, id, c) ->
  let v' = field i (dv env v) in
  let env' = bind env id v' in
  eval' env' c
| Cps.Switch (value, [a;b]) ->
  let cond = dv env value in
  eval' env (begin match cond with
  | Bool v -> if v then a else b
  | _ -> failwith "conditionals must evaluate to a boolean"
end)
  (* failwith "todoo" *)
| Cps.Switch _ -> failwith "missing switch cases"


let eval vl e_ dl = eval' (bindn env0 vl dl) e_

let interp e = match eval [] e [] with
  | Int i -> Some i
  | _ -> None
