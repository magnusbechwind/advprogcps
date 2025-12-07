(** Type of values based on the denotational semantics presented in Chapter 3 of Compiling with Continuations *)
type dval =
Tuple of dval list * int
| Int of int
| Bool of bool
| Fun of dval_fun * string option
| String of string

(** For the sake of debugging, open programs (i.e. functions with unbound arguments) are also dvals *)
and dval_fun = dval list -> dval

(** Printing utility for dvals *)
let rec dval_str = function
| Tuple (dvals, _) -> List.fold_left (fun acc x -> acc ^ dval_str x ^ ", " ) "" dvals
| Int i -> string_of_int i
| Bool b -> string_of_bool b
| String str -> str
| Fun (_,Some msg) -> Printf.sprintf "dval fun with %s" msg
| Fun _ -> "dval fun w/o msg"

type env = Ast.ident -> dval

(** Transform a CPS value to a dval (result) *)
let dv env = function
    Cps.Int i -> Int i
  | Cps.Bool b -> Bool b
  | Cps.Var v -> env v
  | Cps.String str -> String str

(** Given a lookup, bind an identifier to a dval and return the extended environment *)
let bind (env: env) (v: Ast.ident) (d: dval) =
  (* StrMap.update (Ast.ident_str v) d *)
  (* Printf.printf "bound %s\n" (Ast.ident_str v);  *)
  fun w ->
    (* Printf.printf "bind lookup %s %s\n" (Pretty.id ent_str v) (Ast.ident_str w); *)
    if v = w then d else env w

let prb = false

(** Yeah no, this is a mess *)
let pr ?(b_=prb) a b =
  if b_ then
    Printf.printf "bindn binding; ids %s: %i; vals %s: %i\n" (List.fold_left (fun acc x -> acc ^ Ast.ident_str x ^ ", ") "" a) (List.length a) (List.fold_left (fun acc x -> acc ^ dval_str x ^ ", ") "" b) (List.length b)
  else ()
  
(** Bind n arguments to n values given and return the updated environment *)
let rec bindn (env: env) vl dl: env = 
  let print () = if prb then print_endline "pr in bindn" else () in
  print();
  pr ~b_:false vl dl;
  match (vl, dl) with
  (v::vl', d::dl') ->
  bindn (bind env v d) vl'
   dl'
(* If both lists have the same size, we recurse to two empty lists and return the updated environment; else we either have too many or too few arguments and abort accordingly *)
| ([], []) -> env
| (l, []) -> failwith (Printf.sprintf "too many (%i) parameters in bindn compared to arguments. Surplus parameters are:%s" (List.length l) (List.fold_left (fun acc x -> acc ^ " " ^ (Ast.ident_str x)) "" l))
| ([], l) ->
  print();
  pr vl dl;
  failwith (Printf.sprintf "too few (%i) parameters in bindn compared to arguments. Surplus parameters are:%s" (List.length l) (List.fold_left (fun acc x -> acc ^ " " ^ (dval_str x)) "" l))

(** Extract the ith field of a tuple *)
let field i = function
| Tuple (tpl, _) -> List.nth tpl i
| _ -> failwith "only tuples should be reachable for field"

(** Return the appropriate function for arithmetic (integer) types *)
let evalarith = function
| Ast.Add -> (+)
| Ast.Sub -> (-)
| Ast.Mul -> ( * )
| Ast.Div -> (/)
| _ -> failwith "unreachable evalarith"

(** Return the appropriate function for boolean types *)
let evalbool = function
| Ast.Eq -> (=)
| Ast.Lt -> (<)
| _ -> failwith "unreachable evalbool"

(** Base case for env: looking up a value not in the environment will abort evaluation *)
let env0 : (Ast.ident -> dval) = fun v -> Printf.printf "Variable %s not found!\n" (Ast.ident_str (v)); failwith "undefined"

(** Extract an integer representation of a dval (if suitable; otherwise we abort) *)
let val_to_int env = function
| Cps.Var v ->
  begin match env v with
  | Int i -> i
  | _ -> failwith "unreachable"
  end
| Cps.Int i -> i
| Cps.Bool b -> (if b then 1 else 0)
| Cps.String _ -> failwith "strings cannot be represented as integers"

(** Evaluate a CPS expression given an environment *)
let rec eval' (env: env) = function
| Cps.Halt v -> dv env v
| Cps.App (f, vl) ->
  (* Printf.printf "f: %s\n" (Prettycps.value_repr f); *)
  (* Printf.printf "vl: %s\n" (List.fold_left (fun acc x -> acc ^ Prettycps.value_repr x ^ " ") "" vl);   *)
  begin match dv env f with
  | Fun (g, _) -> g (List.map (dv env) vl)
  | e -> failwith (Printf.sprintf "f needs to be a function symbol; was %s" (dval_str e))
  end
| Cps.Primop (Ast.Print, [str], [k], [c]) ->
  Printf.printf "%s" (begin match str with
  | String str -> str
  | Var v -> dval_str (env v)
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
end);
  eval' (bind env k (Int (-1))) c
| Cps.Primop (Ast.Println, [str], [k], [c]) ->
  Printf.printf "%s\n" (begin match str with
  | String str -> str
  | Var v -> dval_str (env v)
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
end);
  eval' (bind env k (Int (-1))) c
  | Cps.Primop (Ast.Read, _,[i], [c]) ->
  eval' (bind env i (String (read_line ()))) c
| Cps.Primop (Ast.Callcc, [_], [_],[_]) ->
  failwith "missing calcc"
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
| Cps.Primop (op, _,_,_) -> failwith (Printf.sprintf "missing primop case: %s\n" (Pretty.str_of_op op))
| Cps.Fix (funs,c) ->
  let rec bindargs r1 (f,vl,b) =
    if prb then Printf.printf "bindargs for %s; %s\n" (Ast.ident_str f) (List.fold_left (fun acc x -> acc ^ ", " ^ Ast.ident_str x) "" vl) else ();
    Fun ((fun al ->
      if prb then (Printf.printf "f: %s; vl: %s : %i; al: %s: %i\n" (Ast.ident_str f)(List.fold_left (fun acc x -> acc ^ Ast.ident_str x ^ ", ") "" vl) (List.length vl) (List.fold_left (fun acc x -> acc ^ dval_str x ^ ", ") "" al) (List.length al);
      Printf.printf "\nb: \n%s\n\n" (Prettycps.cps_ast_repr b);
      print_endline ((Printf.sprintf "binding arguments of %s") (Ast.ident_str f));
      pr vl al) else ();
    eval' (bindn (updateenv r1) vl al) b), Some (List.fold_left (fun acc a -> acc ^ (Ast.ident_str a) ^ " ") "" vl))

  and updateenv r =
    let function_bindings = List.map (fun (f,_,_) ->
      if prb then Printf.printf "updating env for %s\n" (Ast.ident_str f);
      f) funs in
    let argument_bindings = List.map (bindargs r) funs in
      if prb then Printf.printf "pr in updateenv; %s\n" (List.fold_left (fun acc (x,_,_) ->  ", " ^ acc ^Ast.ident_str x) "" funs);
    pr function_bindings argument_bindings;
    bindn r function_bindings argument_bindings
  in
    eval' (updateenv env) c
| Cps.Tuple (vl, id, c) ->
  let vl' = Tuple (List.map (dv env) vl, List.length vl) in
  let env' = bind env id vl' in
  eval' env' c
| Cps.Select (i, v, id, c) ->
  let v' = field i (dv env v) in
  let env' = bind env  id v' in
  eval' env' c
| Cps.If (value, [a;b]) ->
  let cond = dv env value in
  eval' env (begin match cond with
  | Bool v -> if v then a else b
  | e -> failwith (Printf.sprintf "conditionals must evaluate to a boolean; was %s" (dval_str e))
end)
| Cps.If _ -> failwith "missing switch cases"


let eval vl e_ dl = eval' (bindn env0 vl dl) e_

(** Given no bound variables (no identifiers and dval to bind), extract the result of an expression *)
let interp e = eval [] e []
