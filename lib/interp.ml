open Ast

(* TODO: Refactor into Errors module and add pretty priting. Store types etc *)
exception UnsupporterdExpression of string
exception UnboundVar of string
exception CallableMismatch of string
exception TupleMismatch of string
exception IllegalTupleAccess of string
exception BoolMismatch of string
exception UnsupportedOp of string
exception TypeMismatch of string


type answer =
  | IntVal of int
  | BoolVal of bool
  (* TODO: Closure val needs env for mutual recursion *)
  | ClosureVal of ident * expr
  | TupleVal of answer list

  (* TODO: Maybe the notion of an answer is a bit wrong. Ops aren't answers for sure*)
  | OpVal of op
[@@deriving show]

let int_of_val x = match x with | IntVal i -> i | _ -> raise @@ TypeMismatch ("Expected Int but found: " ^ show_answer x)
let int_bin_fun_of_op op = match op with
| Add -> (+)
| Sub -> (-)
| Div -> (/)
| Mul -> ( * )
| _ -> raise @@ UnsupportedOp ("Unsupported integer op: " ^ show_op op)

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

  (* TODO: This can also be application of op *)
  | App (callable_expr, arg_expr) -> 
    evalc env callable_expr arg_expr

  | Select(int, expr) -> 
    let tuple = evalt env expr in

    begin match List.nth_opt tuple int with 
    | Some v -> v
    | None -> raise @@ IllegalTupleAccess "Tried to access invalid index of tuple"
    end
  
  | IfEl (e1, e2, e3) -> 
    let b = evalb env e1 in
    if b then eval env e2 else eval env e3

  | Primop op -> OpVal op

  | _ -> failwith "err"
  

  
  and evalc env expr arg_expr =
    let answer = eval env expr in
    match answer with 
    | ClosureVal(ident, expr) -> 
      let body_env = insert env ident (eval env arg_expr) in 
      eval body_env expr

    | OpVal op -> evalop env op arg_expr
    | _ -> raise @@ CallableMismatch ("Expected callable but found: " ^ show_answer answer)
  and evalt env expr =
    let answer = eval env expr in
    match answer with
    | TupleVal(answers) -> answers
    | _ -> raise @@ TupleMismatch ("Expected tuple but found: " ^ show_answer answer)
  and evalb env expr = 
    let answer = eval env expr in
    match answer with
    | BoolVal(answer) -> answer
    | _ -> raise @@ BoolMismatch ("Expected bool but found: " ^ show_answer answer)
  and evalop env op arg_expr =
    let arg_tuple = evalt env arg_expr in
    match op with
    | Add | Sub | Mul | Div -> 
      let acc = begin match op with | Add | Sub -> 0 | Mul | Div -> 1 | _ -> failwith "err" end in
      IntVal (List.fold_left (fun acc x -> (int_bin_fun_of_op op) acc (int_of_val x)) acc arg_tuple)
    
    | _ -> raise @@ UnsupportedOp ("Unsupported primop: " ^ show_op op)