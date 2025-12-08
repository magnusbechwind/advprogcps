open Lib
open! Base

let test_transform lam =
  let cps = Transform.cps lam in
  Utils.test_lam_cps_eval_eq lam cps

let%test_unit "transform" =
  Utils.test_files ["test_programs"] |> Stdlib.List.iter (fun file ->
    file |> Fileparser.parse |> Stdlib.Option.get |> test_transform
  )
