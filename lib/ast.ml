type ident = Ident of string
[@@deriving show]

exception Todo of string (* for incomplete match cases (the warnings get annoying) *)


type op =
  (* arithmetic operators*) | Add | Sub | Mul | Div
  (* logical operators *) |  Eq
  (* first-class continuations (todo) *) | Callcc | Throw | Reset | Shift
  [@@deriving show]

  type expr =
  | Const of value
  | Var of ident
  | Int of int
  | Bool of bool
  | Fn of ident * expr
  | App of expr * expr
  | Tuple of expr list
  | Select of int * expr
  | IfEl of expr * expr * expr
  | Primop of op
  | Fix of ident list * expr list * expr
  (* the types below may have to be discarded based on how Compiling with Continuations described their lambda language *)
  | Let of ident * expr * expr
  | BinOp of op * expr * expr (* Might make sense to only encode binops. No need to handle lists then *)
and value = 
  | Lambda of ident list * expr
[@@deriving show]

type prog = expr option

(* This might not be the right representation of continuations *)
type continuation = Cont of ident * expr
type cprog = expr * continuation