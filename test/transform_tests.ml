open Lib
open! Base

let test_transform lam =
  let cps = Transform.cps lam in
  Utils.test_lam_cps_eval_eq lam cps

let%test_unit "transform > arith" = "arith.lambda" |> Utils.parse |> test_transform

let%test_unit "transform > if" = "if.lambda" |> Utils.parse |> test_transform

let%test_unit "transform > cont1" = "cont1.lambda" |> Utils.parse |> test_transform

