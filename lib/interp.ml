open Ast

exception UnsupporterdExpression of string
exception UnboundVar of string
exception ClosureMismatch of string
exception TupleMismatch of string
exception IllegalTupleAccess of string


type answer =
  | IntVal of int
  | BoolVal of bool
  | ClosureVal of ident * expr
  | TupleVal of answer list
[@@deriving show]

module Env = Map.Make(String)



let lookup env (Ident(ident)) = match Env.find_opt ident env with
  | Some v -> v
  | None -> raise @@ UnboundVar (Printf.sprintf "identifier: %s not bound in env" ident)

let insert env (Ident(ident)) value = 
  Env.add ident value env
  
let rec eval (env: answer Env.t) (expr: expr) = 
  match expr with 

  (* Immediately evaluates to answers*)
  | Var (ident) -> lookup env ident
  | Int (i) -> IntVal(i)
  | Bool (b) -> BoolVal(b)
  | Fn (ident, expr) -> ClosureVal (ident, expr)
  | Tuple(exprs) -> TupleVal(List.map (fun x -> eval env x) exprs)

  | App (closure_expr, arg_expr) -> 
    let (param, body) = evalc env closure_expr in 
    let body_env = insert env param (eval env arg_expr) in
    eval body_env body

  | Select(int, expr) -> 
    let tuple = evalt env expr in
    begin match List.nth_opt tuple int with 
    | Some v -> v
    | None -> raise @@ IllegalTupleAccess "Tried to access invalid index of tuple"

  
  end




  (* Leftovers from old AST. Should ideally be removed if not needed *)
  | Let (_) | BinOp(_) | Const(_) -> raise @@ UnsupporterdExpression ("Unsupported expression: " ^ show_expr expr)
  | _ -> failwith "err"
  
  and evalc env expr =
    let answer = eval env expr in
    match answer with 
    | ClosureVal(ident, expr) -> (ident, expr)
    | _ -> raise @@ ClosureMismatch ("Expected closure but found: " ^ show_answer answer)
  and evalt env expr =
    let answer = eval env expr in
    match answer with
    | TupleVal(answers) -> answers
    | _ -> raise @@ TupleMismatch ("Expected tuple but found: " ^ show_answer answer)
    