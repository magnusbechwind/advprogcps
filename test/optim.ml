open Lib
open Base

let cps_test trans file =
  let cps = file |> Utils.parse |> Transform.cps in
  let optimized = cps |> trans in
  Utils.test_cps_eval_eq cps optimized

let beta_fold_fix = Optim.fix [Beta.beta_contract; Constfold.const_fold]

let%test_unit "beta-fold > beta_fold eval" = cps_test beta_fold_fix "beta_fold.lambda"

let%test_unit "beta-fold > beta_fold AST" =
  let optimized = "beta_fold.lambda" |> Utils.parse |> Transform.cps |> beta_fold_fix in
  [%test_eq: Cps.cexpr] (Cps.Halt (Cps.Int 8)) optimized

let%test_unit "beta-fold > beta_fold_2 eval" = cps_test beta_fold_fix "beta_fold_2.lambda"

let%test_unit "beta-fold > beta_fold_3 AST" =
  let optimized = "beta_fold_3.lambda" |> Utils.parse |> Transform.cps |> beta_fold_fix in
  [%test_eq: Cps.cexpr] (Cps.Halt (Cps.Int 4)) optimized
