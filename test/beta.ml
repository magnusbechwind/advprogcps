open Lib
open! Base

let beta_test file =
  let cps = file |> Utils.parse |> Transform.cps in
  let contracted = Beta.beta_contract cps in

  Utils.test_cps_eval_eq cps contracted

let%test_unit "beta > cont1" = beta_test "cont1.lambda"

let%test_unit "beta > arith" = beta_test "arith.lambda"

let%test_unit "beta > if" = beta_test "if.lambda"
