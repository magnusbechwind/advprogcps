open Ast
exception UnboundVar of string
exception IllegalTupleAccess of string
exception TypeMismatch of string
exception UnsupportedOp of string

module Env : sig
  (* Stupid evil hack to make show work. Might break everything. Who knows*)
  val pp : 'a -> 'b -> 'c -> unit
end

(* List of function declarations. Used for mutual recursion.*)
type decl = ident * expr
[@@deriving show]
type answer =
  | IntVal of int 
  | BoolVal of bool
  | StringVal of string
  (* Closures are an identifier, an expression, env at decl time and functions declared alongside (for fix)*)
  | ClosureVal of ident * expr * env * decl list
  | TupleVal of answer list
  | OpVal of op
and env = answer Map.Make(String).t
[@@deriving show]

(** Interprets expression 'expr'. Returns [answer] *)
val interp : Ast.expr -> answer

