exception UnboundVar of string
exception IllegalTupleAccess of string
exception TypeMismatch of string
exception UnsupportedOp of string

(** Interprets expression 'expr'. Returns Some(i) if expr evaluates to an integer, None otherwise *)
val interp : Ast.expr -> int option
