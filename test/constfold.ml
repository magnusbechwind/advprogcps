open Lib
open! Base

let constfold_test file =
  let lambda = file |> Utils.parseTest |> Stdlib.Option.get in
  let cps = Transform.cps lambda in
  let folded = Constfold.const_fold cps in

  let cps_res = Evalcps.interp cps in
  let folded_res = Evalcps.interp folded in

  [%test_eq: int option] cps_res folded_res

let%test_unit "beta cont1" = constfold_test "cont1.lambda"

let%test_unit "beta arith" = constfold_test "arith.lambda"

let%test_unit "beta if" = constfold_test "if.lambda"
