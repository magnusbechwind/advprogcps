open! Base

let%test_unit "transform arith test" = Utils.comparisonTest "arith.lambda"

let%test_unit "transform if test" = Utils.comparisonTest "if.lambda"

let%test_unit "transform cont1 test" = Utils.comparisonTest "cont1.lambda"

