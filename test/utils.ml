open Lib

let parse file = Fileparser.parse ("../../../../../test/test_programs/" ^ file) |> Option.get

open Base

type eval =
| Int of int
| Bool of bool
| String of string
| Tuple of eval list
[@@deriving show, compare, sexp, equal]

module Eval = struct
  let (let*) = Stdlib.Result.bind
  let ret = Stdlib.Result.ok

  let rec try_from_answer = function
  | Interp.IntVal i -> Ok (Int i)
  | Interp.BoolVal b -> Ok (Bool b)
  | Interp.StringVal s -> Ok (String s)
  | Interp.TupleVal xs ->
    let* xs' = Stdlib.List.fold_left (fun acc x ->
      let* acc' = acc in
      let* e = try_from_answer x in
      let res = Stdlib.List.rev (e :: acc') in
      ret res
    ) (Ok []) xs in
    ret (Tuple xs')
  | Interp.ClosureVal _ -> Error "Unexpected answer type ClosureVal"
  | Interp.OpVal _ -> Error "Unexpected answer type OpVal"

  let rec try_from_dval = function
  | Evalcps.Int i -> Ok (Int i)
  | Evalcps.Bool b -> Ok (Bool b)
  | Evalcps.String s -> Ok (String s)
  | Evalcps.Tuple (xs, _) ->
    let* xs' = Stdlib.List.fold_left (fun acc x ->
      let* acc' = acc in
      let* e = try_from_dval x in
      let res = Stdlib.List.rev (e :: acc') in
      ret res
    ) (Ok []) xs in
    ret (Tuple xs')
  | Evalcps.Fun (_, _) -> Error "Unexpected dval type Fun"
end

(** Returns [x] if input is [Ok x] and raises [Failure] with [s] if input is [Error s]. *)
let ok_or_failwith = function
| Ok x -> x
| Error s -> failwith s

(** Evaluates two CPS programs and compares the results.

    Return [true] if the outputs are the same, [false] otherwise.

    Raises [Failure] if the either output doesn't evaluate to one of the following
    value types:
    - Int
    - Bool
    - String
    - Tuple
*)
let test_cps_eval_eq cps1 cps2 =
  let res1 = cps1 |> Evalcps.interp |> Eval.try_from_dval |> ok_or_failwith in
  let res2 = cps2 |> Evalcps.interp |> Eval.try_from_dval |> ok_or_failwith in
  
  [%test_eq: eval] res1 res2

(** Tests if an [answer] matches an [eval].
    Raises [Failure] if the [answer] isn't one of the following types
    - IntVal
    - BoolVal
    - StringVal
    - TupleVal
*)
let test_answer eval answer = 
  let answer' = answer |> Eval.try_from_answer |> ok_or_failwith in
  [%test_eq: eval] eval answer'

(** Tests if a [dval] matches an [eval].
    Raises [Failure] if the [dval] isn't one of the following types
    - Int
    - Bool
    - String
    - Tuple
*)
let test_dval eval dval =
  let dval' = dval |> Eval.try_from_dval |> ok_or_failwith in
  [%test_eq: eval] eval dval'

(** Evaluates a lambda program [lam] and CPS program [cps] and checks 

    Raises [Failure] the [lam] doesn't evaluate to one of the following
    value types:
    - Int
    - Bool
    - String
    - Tuple
*)
let test_lam_cps_eval_eq lam cps =
  let lambda_res = lam |> Interp.interp |> Eval.try_from_answer |> ok_or_failwith in
  let cps_res = cps |> Evalcps.interp |> Eval.try_from_dval |> ok_or_failwith in

  [%test_eq: eval] lambda_res cps_res
