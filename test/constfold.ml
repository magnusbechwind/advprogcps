open Lib
open Base

let test_constfold_eq file =
  let cps = file |> Utils.parse |> Transform.cps in
  let folded = Constfold.const_fold cps in

  Utils.test_cps_eval_eq cps folded

let test_constfold_ast file cps =
  let folded = file |> Utils.parse |> Transform.cps |> Constfold.const_fold in
  [%test_eq: Cps.cexpr] folded cps

let%test_unit "constfold > cont1" = test_constfold_eq "cont1.lambda"

let%test_unit "constfold > arith" = test_constfold_eq "arith.lambda"

let%test_unit "constfold > if" = test_constfold_eq "if.lambda"

let%test_unit "constfold > if AST" = test_constfold_ast "if.lambda" (Cps.Halt (Cps.Int 0))

let%test_unit "constfold > f_in_constarith" = test_constfold_eq "f_in_constarith.lambda"

let%test_unit "constfold > constarith_in_f" = test_constfold_eq "constarith_in_f.lambda"

let%test_unit "constfold > arith AST" = test_constfold_ast "arith.lambda" (Cps.Halt (Cps.Int 48))
