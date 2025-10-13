open Ast

type value =
  Var of ident
| Int of int
| Bool of bool

type cexpr =
  Halt
| App of value * value list
| Fix of (ident * ident list * cexpr) list * cexpr
| Tuple of (value * int) list * ident * cexpr
| Select of int * value * ident * cexpr
| Primop of op * value list * ident list * cexpr list