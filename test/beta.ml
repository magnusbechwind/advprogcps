open Lib
open! Base

let%test_unit "test" =
  let lambda = Utils.parseTest "cont1.lambda" |> Stdlib.Option.get in
  let cps = Transform.cps lambda in
  let contracted = Beta.beta_contract cps in

  let cps_res = Evalcps.interp cps in
  let contracted_res = Evalcps.interp contracted in

  [%test_eq: int option] cps_res contracted_res

let%test_unit "test2" =
  let lambda = Utils.parseTest "arith.lambda" |> Stdlib.Option.get in
  let cps = Transform.cps lambda in
  let contracted = Beta.beta_contract cps in

  let cps_res = Evalcps.interp cps in
  let contracted_res = Evalcps.interp contracted in

  [%test_eq: int option] cps_res contracted_res

