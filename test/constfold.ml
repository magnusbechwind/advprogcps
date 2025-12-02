open Lib
open Base

let constfold_eq_test file =
  let lambda = file |> Utils.parseTest |> Stdlib.Option.get in
  let cps = Transform.cps lambda in
  let folded = Constfold.const_fold cps in

  let cps_res = Evalcps.interp cps in
  let folded_res = Evalcps.interp folded in

  [%test_eq: int option] cps_res folded_res

let constfold_ast_test file cps =
  let folded = file |> Utils.parseTest |> Stdlib.Option.get |> Transform.cps |> Constfold.const_fold in
  [%test_eq: Cps.cexpr] folded cps

let%test_unit "constfold cont1" = constfold_eq_test "cont1.lambda"

let%test_unit "constfold arith" = constfold_eq_test "arith.lambda"

let%test_unit "constfold if" = constfold_eq_test "if.lambda"

let%test_unit "constfold if AST" = constfold_ast_test "if.lambda" (Cps.Halt (Cps.Int 0))

let%test_unit "constfold f_in_constarith" = constfold_eq_test "f_in_constarith.lambda"

let%test_unit "constfold constarith_in_f" = constfold_eq_test "constarith_in_f.lambda"

let%test_unit "constfold arith AST" = constfold_ast_test "arith.lambda" (Cps.Halt (Cps.Int 48))
