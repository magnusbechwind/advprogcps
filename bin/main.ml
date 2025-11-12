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

    let comp = Data.complexity cp in
    Printf.printf "\nComplexity of CPS :\n";
    PrintBox_text.output stdout (Data.comp_to_tree comp);
    Printf.printf "\nComplexity of CPS with details:\n";
    PrintBox_text.output stdout (Data.comp_to_tree ~show_details:true comp);


    Printf.printf "\nCPS AST:\n%s\n" (Prettycps.cps_ast_repr cp);
    Printf.printf "Result: %s\n" (match Evalcps.eval [] cp [] with
    | Evalcps.Bool b -> string_of_bool b
    | Evalcps.Int i -> string_of_int i
    | _ -> "something went wrong"
    );
    
    ()
  | _ -> ()
