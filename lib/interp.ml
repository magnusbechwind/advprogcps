open Ast

(* TODO: Refactor into Errors module and add pretty priting. Store types etc *)
exception UnboundVar of string
exception IllegalTupleAccess of string
exception TypeMismatch of string
exception UnsupportedOp of string


module Env = struct 
  (* Stupid evil hack to make show work. Might break everything. Who knows*)
  include Map.Make(String)
  let pp _ _ _ = ()
end


(* List of function declarations. Used for mutual recursion.*)
type decl = ident * expr
[@@deriving show]

type answer =
  | IntVal of int 
  | BoolVal of bool
  (* Closures are an identifier, an expression, env at decl time and functions declared alongside (for fix)*)
  | ClosureVal of ident * expr * env * decl list
  | TupleVal of answer list
  | OpVal of op
and 
 env = answer Env.t 
[@@deriving show]

let cmp_answer l r = match (l, r) with
 | IntVal(l), IntVal(r) -> l = r
 | BoolVal(l), BoolVal(r) -> l = r
 |  _,_ -> raise @@ TypeMismatch ("Expected comparable types but found " ^ show_answer l ^ " and " ^ show_answer r)

let int_of_val x = match x with | IntVal i -> i | _ -> raise @@ TypeMismatch ("Expected Int but fould" ^ show_answer x)

let int_bin_fun_of_op op = match op with
| Add -> (+)
| Sub -> (-)
| Div -> (/)
| Mul -> ( * )
| _ -> raise @@ UnsupportedOp ("Unsupported integer op: " ^ show_op op)


let lookup env = function
  | Ast.Ident ident -> 
    begin match Env.find_opt ident env with
      | Some v -> v
      | None -> raise @@ UnboundVar (Printf.sprintf "identifier: %s not bound in env" ident)
    end
  | Wildcard -> raise @@ UnboundVar "Wildcards are not variables"

let insert env ident value = 
  match ident with
  | Ast.Ident ident ->
    Env.add ident value env
  | Wildcard -> env

let rec eval (env: env) (expr: expr) = 
  match expr with 

  (* Immediately evaluates to answers*)
  | Var (ident) -> lookup env ident
  | Int (i) -> IntVal(i)
  | Bool (b) -> BoolVal(b)
  | Fn (ident, expr) -> ClosureVal (ident, expr, env, [])
  | Tuple(exprs) -> TupleVal(List.map (fun x -> eval env x) exprs)

  | App (callable_expr, arg_expr) -> 
    eval_callable env callable_expr arg_expr

  | Select(int, expr) -> 
    let tuple = evalt env expr in

    begin match List.nth_opt tuple int with 
    | Some v -> v
    | None -> raise @@ IllegalTupleAccess (Printf.sprintf "Tried to acces invalid index %d of tuple (%s)" int (List.fold_right (fun t acc -> (show_answer t) ^ acc) tuple ""))
    end
  
  | IfEl (e1, e2, e3) -> 
    let b = evalb env e1 in
    if b then eval env e2 else eval env e3

  | Primop op -> OpVal op

  | Fix (decls, expr) -> 
    
    let env' = List.fold_left (fun acc_env (id, f_expr) -> 
       insert acc_env id @@ (clos_with_decls env f_expr decls)
    ) env decls in
    
    eval env' expr
  
  and clos_with_decls env expr decls =
    let answer = eval env expr in
    match answer with
    | ClosureVal(ident, expr, env, _) -> ClosureVal(ident, expr, env, decls)
    | _ -> raise @@ TypeMismatch ("Expected Closure but found" ^ show_answer answer) 
  
  and eval_callable env expr arg_expr =
    let answer = eval env expr in
    match answer with 

    (* Closures implemented according to programming languages week 6 slide 17 *)
    | ClosureVal(ident, expr, cenv, decls) -> 

      let cenv' = List.fold_left (fun acc_env (f_id, f_expr) -> 
       insert acc_env f_id @@ (clos_with_decls cenv f_expr decls)
      ) cenv decls in 

      let body_env = insert cenv' ident (eval env arg_expr) in 

      eval body_env expr

    | OpVal op -> evalop env op arg_expr
    | _ -> raise @@ TypeMismatch ("Expected Callable but found" ^ show_answer answer)

  and evalt env expr =
    let answer = eval env expr in
    match answer with
    | TupleVal(answers) -> answers
    | _ -> raise @@ TypeMismatch ("Expected Tuple but found" ^ show_answer answer)
  and evalt2 env expr = 
    begin match evalt env expr with 
    | l :: r :: [] -> (l, r)
      | _ -> raise @@ TypeMismatch ("Expected Tuple of two elements but found")
    end
  and evalb env expr = 
    let answer = eval env expr in
    match answer with
    | BoolVal(answer) -> answer
    | _ -> raise @@ TypeMismatch ("Expected Boolean but found" ^ show_answer answer)
  and evalop env op arg_expr =
    match op with 

    | Add | Sub | Mul | Div ->
      let f = int_bin_fun_of_op op in
      let (l, r) =  evalt2 env arg_expr in
      IntVal (f (int_of_val l) (int_of_val r))

    | Eq ->
      let (l, r) = evalt2 env arg_expr in
      BoolVal (cmp_answer l r)

    | _ -> raise @@ UnsupportedOp ("Unsupported primop: " ^ show_op op)


let interp expr =
  let env = Env.empty in 
  let answer = eval env expr in
  
  match answer with 

  | IntVal(i) -> Some(i)
  | _ -> None
