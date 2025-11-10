open Base
open Lib

let comparisonTest filename =
  let lambda_prog = filename |> Fileparser.parse |> Stdlib.Option.get in
  let cps_prog = Transform.cps lambda_prog in

  let lambda_res = Interp.interp lambda_prog in
  let cps_res = Evalcps.interp cps_prog in

  [%test_eq: int option] lambda_res cps_res
