type ident = Ident of string

exception Todo of string (* for incomplete match cases (the warnings get annoying) *)

type op =
  (* arithmetic operators*) | Add | Sub | Mul | Div
  (* logical operators *) |  Eq
  (* first-class continuations (todo) *) | Callcc | Throw | Reset | Shift

  type expr =
  | Var of ident
  | Fn of ident * expr
  | Fix of ident list * expr list * expr
  | Int of int
  | Bool of bool
  | App of expr * expr
  | Primop of op * expr list
  | IfEl of expr * expr * expr
  (* the types below may have to be discarded based on how Compiling with Continuations described their lambda language *)
  | Const of value
  | Let of ident * expr * expr
  | BinOp of op * expr * expr (* Might make sense to only encode binops. No need to handle lists then *)
and value = 
  | Int of int
  | Bool of bool
  | Var of ident
  | Lambda of ident list * expr

type prog = expr option


(* This might not be the right representation of continuations *)
type continuation = Cont of ident * expr
type cprog = expr * continuation