open Lib
open Base

let cps_test trans file =
  let cps = file |> Utils.parseTest |> Stdlib.Option.get |> Transform.cps in
  let optimized = cps |> trans in

  let cps_res = Evalcps.interp cps in
  let optimized_res = Evalcps.interp optimized in
  [%test_eq: int option] cps_res optimized_res

let beta_fold_fix = Optim.fix [Beta.beta_contract; Constfold.const_fold]

let%test_unit "beta-fold beta_fold eval" = cps_test beta_fold_fix "beta_fold.lambda"

let%test_unit "beta-fold beta_fold AST" =
  let optimized = "beta_fold.lambda" |> Utils.parseTest |> Stdlib.Option.get |> Transform.cps |> beta_fold_fix in
  [%test_eq: Cps.cexpr] optimized (Cps.Halt (Cps.Int 8))

let%test_unit "beta-fold beta_fold_2 eval" = cps_test beta_fold_fix "beta_fold_2.lambda"
