open Base
open Lib

let%test_unit "rev" =
  [%test_eq: int list] (List.rev [ 3; 2; 1 ]) [ 1; 2; 3 ]

let%test_unit "eval lambda AST 2 gives 2" =
  [%test_eq: int option] (Interp.interp (Int 2)) (Some 2)

let%test_unit "eval lambda AST (x -> x + 1)1 gives 2" =
  let p: Ast.expr = App (
    Fn ( Ident "x",
      App ( Primop Add, Tuple [ Var (Ident "x"); Int 1 ] )
    ),
    Int 1
  ) in
  [%test_eq: int option] (Interp.interp p) (Some 2)

(* AST program `(x -> (if x == 0 then (y -> 1) else (y -> y * 2) ) x ) (2 + 1)` *)
let non_trivial_program: Ast.expr = App (
  Fn ( Ident "x",
    App (
      IfEl (
        App ( Primop Eq, Tuple [ Var (Ident "x"); Int 0 ] ),
        Fn ( Ident "y", Int 1 ),
        Fn ( Ident "y", App ( Primop Mul, Tuple [ Var (Ident "y"); Int 2 ] ) )
      ),
      Var (Ident "x")
    )
  ),
  App ( Primop Add, Tuple [ Int 2; Int 1 ] )
)

let%test_unit "eval lambda AST non-trivial program" =
  [%test_eq: int option] (Interp.interp non_trivial_program) (Some 6)
