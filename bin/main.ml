open Lib

let args = Sys.argv

let usage_msg = "main [--betac] [--cps] [--eval] ... <file>"

let betac = ref false
let cfold = ref false
let ast = ref false
let cps = ref false
let raw = ref false
let comp = ref false
let eval = ref false
let input_file = ref None

let file_handle file = input_file := Some (if (String.ends_with ~suffix:".lambda" file) then file else (Printf.sprintf "%s.lambda" file)) (* allows calling functions with or without the correct file extension *)

let speclist = [
  ("--betac", Arg.Set betac, "Adds beta contraction optimization");
  ("--cfold", Arg.Set cfold, "Adds constant folding optimization");
  ("--ast", Arg.Set ast, "Prints lambda AST");
  ("--cps", Arg.Set cps, "Prints CPS AST");
  ("--raw", Arg.Set raw, "Prints CPS AST before optimizations");
  ("--comp", Arg.Set comp, "Prints complexities");
  ("--eval", Arg.Set eval, "Evaluates program");
]

let format_spec str res =
  Printf.printf "%s\n%s\n" str (PrintBox_text.to_string res)

let print_ast ast =
  format_spec "AST:" (Pretty.program_to_tree ast)

let print_raw cps =
  format_spec "(Raw) CPS:" (Prettycps.cps_to_tree cps)

let print_cps cps =
  format_spec "(Optimized) CPS:" (Prettycps.cps_to_tree cps)

let () =
  Arg.parse speclist file_handle usage_msg;
  let beta = if !betac then Some Beta.beta_contract else None in
  let fold = if !cfold then Some Constfold.const_fold else None in
  let trans = [
    beta;
    fold;
  ] |> List.filter_map Fun.id in

  let file = Option.value !input_file ~default:"test.lambda" in

  let lambda = Fileparser.parse file in
    lambda |>
    Option.iter(fun e ->
    if !ast then print_ast e;

    let cp = Transform.cps e in

    if !raw then print_raw cp;

    let optimized = Optim.fix trans cp in

    if !cps then print_cps optimized;

    if !comp then (
      if !raw then (
        let c = Data.complexity cp in
        format_spec "Complexity of (raw) CPS :" (Data.comp_to_tree c)
      );
      (* Printf.printf "\nComplexity of CPS with details:\n";
      PrintBox_text.output stdout (Data.comp_to_tree ~show_details:true comp); *)
      if !cps then (
        let c2 = Data.complexity optimized in
        format_spec "Complexity of (optimized) CPS :" (Data.comp_to_tree c2)
      );

      (* Printf.printf "\nComplexity of BETA + FOLD with details:\n";
      PrintBox_text.output stdout (Data.comp_to_tree ~show_details:true c2); *)
    );

    (* Printf.printf "\nCPS AST:\n%s\n" (Prettycps.cps_ast_repr cp); *)

    if !eval then (
      let rec evaldval = function
      | Evalcps.Bool b -> string_of_bool b
      | Evalcps.Int i -> string_of_int i
      | Evalcps.Fun (_,Some str) -> (Printf.sprintf "CPS evaluated to a function but we don't want that :(; value was %s" str)
      | Evalcps.String str -> str
      | Evalcps.Tuple (tpl,_) -> List.fold_left (fun acc x -> acc ^ ", " ^ evaldval x  ) "" tpl
      | _ -> "Not a value"
      in
      Printf.printf "Result: %s\n" (evaldval (Evalcps.interp cp));
    );
  )
