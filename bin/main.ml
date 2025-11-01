open Lib

let initLexer filename =
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
  filebuf.lex_curr_p <- { filebuf.lex_curr_p with pos_fname = filename };
  (input, filebuf)
  
let parse file =
  let input, filebuf = initLexer file in
  let lexRes =
      Parser.prog Lexer.token filebuf
    in
  close_in input;
  lexRes

let args = Sys.argv

let () = 
  let x = parse (match Array.length args with
  | 1 -> "test.lambda"
  | _ -> (Array.get args 1) ^ ".lambda") in
  match x with
    Some e ->
    Printf.printf "AST:\n";
    PrintBox_text.output stdout (Pretty.program_to_tree e); print_endline "\n";
    let cp = Transform.cps e in
    Printf.printf "CPS:\n";
    PrintBox_text.output stdout (Prettycps.cps_to_tree cp); print_endline "\n";
    (* Printf.printf "\nCPS AST:\n%s\n" (Prettycps.cps_ast_repr cp); *)
    let rec evaldval = function
    | Evalcps.Bool b -> string_of_bool b
    | Evalcps.Int i -> string_of_int i
    | Evalcps.Fun (f,_) -> evaldval (f [])
    | Evalcps.String str -> str
    | _ -> "not a value"
    in
    Printf.printf "Result: %s\n" (evaldval (Evalcps.eval [] cp []));
    ()
  | _ -> ()
