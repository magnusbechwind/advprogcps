open Lib
open! Base

let beta_test file =
  let lambda = file |> Utils.parseTest |> Stdlib.Option.get in
  let cps = Transform.cps lambda in
  let contracted = Beta.beta_contract cps in

  let cps_res = Evalcps.interp cps in
  let contracted_res = Evalcps.interp contracted in

  [%test_eq: int option] cps_res contracted_res

let%test_unit "beta cont1" = beta_test "cont1.lambda"

let%test_unit "beta arith" = beta_test "arith.lambda"

let%test_unit "beta if" = beta_test "if.lambda"
