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
  
let () = 
  let x = parse "test.lambda" in
  match x with
    Some e -> PrintBox_text.output stdout (Pretty.program_to_tree e); print_endline "\n";
    let cp = Transform.cps e in
    PrintBox_text.output stdout (Prettycps.cps_to_tree cp);
    ()
  | _ -> ()
