open Base

type ident = Ident of string
[@@deriving show, compare, sexp, equal]


exception Todo of string (* for incomplete match cases (the warnings get annoying) *)


type op =
  (* arithmetic operators*) | Add | Sub | Mul | Div
  (* logical operators *) | Eq | Lt
  (* first-class continuations (todo) *) | Callcc | Throw | Reset | Shift
  [@@deriving show, compare, sexp, equal]

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
  | Fix of (ident * expr) list * expr
[@@deriving show, compare, sexp, equal]

type prog = expr option
[@@deriving show, compare, sexp, equal]

(* This might not be the right representation of continuations *)
type continuation = Cont of ident * expr
type cprog = expr * continuation
