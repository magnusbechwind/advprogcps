open Ast
open Base

type value =
  Var of ident
| Int of int
| Bool of bool
| String of string
[@@deriving show, compare, sexp, equal]

let val_str = function
  | Var v -> ident_str v
  | Int i -> Int.to_string i
  | Bool b -> Bool.to_string b
  | String s -> s

let try_to_id = function
| Var id -> Some id
| _ -> None

type cexpr =
  Halt of value
| App of value * value list
| Fix of (ident * ident list * cexpr) list * cexpr
| Tuple of value list * ident * cexpr
| Select of int * value * ident * cexpr
| Switch of value * cexpr list
| Primop of op * value list * ident list * cexpr list
[@@deriving show, compare, sexp, equal]

