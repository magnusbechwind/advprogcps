open! Base
open Lib

let%test_unit "parse arith" =
  let p = Utils.parse "arith.lambda" in
  [%test_eq: Ast.prog] p (Some (
    App ( Primop Mul,
      Tuple [
        App ( Primop Add, Tuple [ Int 3; Int 1 ] );
        App ( Primop Add, Tuple [ Int 3; Int 9 ] )
      ]
    )
  ))

let%test_unit "parse if" =
  let p = Utils.parse "if.lambda" in
  [%test_eq: Ast.prog] p (Some (
    IfEl (
      App ( Primop Eq,
        Tuple [
          App ( Primop Add, Tuple [ Int 2; Int 2 ] );
          Int 4
        ]
      ),
      Int 0,
      Int 1
    )
  ))

let%test_unit "parse cont1" =
  let p = Utils.parse "cont1.lambda" in
  [%test_eq: Ast.prog] p (Some (
    App (
      Fn ( Ident "a",
        App (
          Fn ( Ident "b",
            App (
              Fn ( Ident "c",
                App (
                  Fn ( Ident "d",
                    App (
                      Fn ( Ident "f",
                        App ( Primop Mul,
                          Tuple [
                            App ( Var (Ident "f"),
                              App ( Primop Add,
                                Tuple [ Var (Ident "a"); Var (Ident "b") ]
                              )
                            );
                            App ( Var (Ident "f"),
                              App ( Primop Add,
                                Tuple [ Var (Ident "c"); Var (Ident "d") ]
                              )
                            )
                          ]
                        )
                      ),
                      Fn ( Ident "x",
                        App ( Primop Add,
                          Tuple [
                            App ( Primop Mul,
                              Tuple [ Int 2; Var (Ident "x") ]
                            );
                            Int 1
                          ]
                        )
                      )
                    )),
                  Int 4
                )),
              Int 3
            )),
          Int 2
        )),
      Int 1
    )
  ))
