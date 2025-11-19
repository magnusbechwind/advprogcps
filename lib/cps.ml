open Ast

type value =
  Var of ident
| Int of int
| Bool of bool
| String of string

type cexpr =
  Halt of value
| App of value * value list
| Fix of (ident * ident list * cexpr) list * cexpr
| Tuple of value list * ident * cexpr
| Select of int * value * ident * cexpr
| Switch of value * cexpr list
| Primop of op * value list * ident list * cexpr list

