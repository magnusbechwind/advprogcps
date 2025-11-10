let initLexer filename =
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
  filebuf.lex_curr_p <- { filebuf.lex_curr_p with pos_fname = filename };
  (input, filebuf)

let parse file =
  let input, filebuf = initLexer file in
  let lexRes = Parser.prog Lexer.token filebuf in
  close_in input;
  lexRes
