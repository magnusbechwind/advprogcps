type ident = Ident of string
[@@deriving show]

exception Todo of string (* for incomplete match cases (the warnings get annoying) *)


type op =
  (* arithmetic operators*) | Add | Sub | Mul | Div
  (* logical operators *) |  Eq
  (* first-class continuations (todo) *) | Callcc | Throw | Reset | Shift
  [@@deriving show]

  type expr =
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
[@@deriving show]

type prog = expr option

(* This might not be the right representation of continuations *)
type continuation = Cont of ident * expr
type cprog = expr * continuation