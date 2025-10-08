type ident = Ident of string

type op =
  | Add

type expr =
  | Const of value
  | Let of ident * expr * expr
  | IfEl of expr * expr * expr
  | App of expr * expr list
  | BinOp of op * expr * expr (* Might make sense to only encode binops. No need to handle lists then *)
and value = 
  | Int of int
  | Bool of bool
  | Var of ident
  | Lambda of ident list * expr

type prog = expr


(* This might not be the right representation of continuations *)
type continuation = Cont of ident * expr
type cprog = expr * continuation