open Lib

let args = Sys.argv

let () = 
  let x = Fileparser.parse (match Array.length args with
  | 1 -> "test.lambda"
  | _ -> (Array.get args 1) ^ ".lambda") in
  match x with
    Some e ->
    Printf.printf "AST:\n";
    PrintBox_text.output stdout (Pretty.program_to_tree e); print_endline "\n";
    let cp = Transform.cps e in
    Printf.printf "CPS:\n";
    PrintBox_text.output stdout (Prettycps.cps_to_tree cp);

    (* let bd = Beta.beta_contract cp in
    Printf.printf "BETA DEAD FIX FIXPOINT:\n";
    PrintBox_text.output stdout (Prettycps.cps_to_tree bd); *)

    (* let comp = Data.complexity cp in
    Printf.printf "\nComplexity of CPS :\n";
    PrintBox_text.output stdout (Data.comp_to_tree comp);
    Printf.printf "\nComplexity of CPS with details:\n";
    PrintBox_text.output stdout (Data.comp_to_tree ~show_details:true comp); *)


    Printf.printf "\nCPS AST:\n%s\n" (Prettycps.cps_ast_repr cp);
    let rec evaldval = function
    | Evalcps.Bool b -> string_of_bool b
    | Evalcps.Int i -> string_of_int i
    | Evalcps.Fun (_,Some str) -> (Printf.sprintf "CPS evaluated to a function but we don't want that :(; value was %s" str)
    | Evalcps.String str -> str
    | Evalcps.Tuple (tpl,i) -> List.fold_left (fun acc x -> acc ^ ", " ^ evaldval x  ) "" tpl
    | _ -> "Not a value"
    in
    Printf.printf "Result: %s\n" (evaldval (Evalcps.eval [] cp []));
    ()
  | _ -> ()
