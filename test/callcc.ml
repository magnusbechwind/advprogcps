open Lib
open Base

let test_callcc file dval =
  let cps = file |> Utils.parse_callcc |> Transform.cps in
  let res = Evalcps.interp cps in

  Utils.test_dval dval res


let%test_unit "callcc > 101 eval" = test_callcc "101.lambda" (Int 101)

let%test_unit "callcc > 111 eval" = test_callcc "111.lambda" (Int 111)

let%test_unit "callcc > argfc" = test_callcc "argfc.lambda" (Bool true)

let%test_unit "callcc > callcc1 eval" = test_callcc "callcc1.lambda" (Int 1)

let%expect_test "callcc > callccfoo" = 
  let cps = "callccfoo.lambda" |> Utils.parse_callcc |> Transform.cps in
  let res = Evalcps.interp cps in
  [%expect {| foo |}];
  Utils.test_dval (Int 10) res

let%test_unit "callcc > callccif" = test_callcc "callccif.lambda" (Int 1)

let%test_unit "callcc > escape" = test_callcc "escape.lambda" (Int 2)

let%test_unit "callcc > escapedCont" = test_callcc "escapedCont.lambda" (Int 0)

let%test_unit "callcc > escapedWiki" = test_callcc "escapedWiki.lambda" (Int 2)

let%test_unit "callcc > wiki" = test_callcc "wiki.lambda" (Int 2)
