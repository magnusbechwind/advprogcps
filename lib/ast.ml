open! Base

type ident =
| Ident of string
| Wildcard
[@@deriving show, compare, sexp, equal]

let ident_str = function
| Ident v -> v
| Wildcard -> "Wildcard"

exception Todo of string (* for incomplete match cases (the warnings get annoying) *)

type op =
  (* arithmetic operators*) | Add | Sub | Mul | Div
  (* logical operators *) | Eq | Lt
  (* first-class continuations (todo) *) | Callcc | Throw
  (* I/O primitives *) | Print | Println | Read
  [@@deriving show, compare, sexp, equal]

type expr =
  | Var of ident
  | Int of int
  | Bool of bool
  | String of string
  | Fn of ident * expr
  | App of expr * expr
  | Tuple of expr list
  | Select of int * expr
  | IfEl of expr * expr * expr
  | Primop of op
  | Fix of (ident * expr) list * expr (* unused *)
[@@deriving show, compare, sexp, equal]

type prog = expr option
[@@deriving show, compare, sexp, equal]
