open Lib
open Ast


let int int = Const(Int(int))

let add l r = BinOp(Add,l, r)

let ident ident = Ident(ident)

let var name = Var(ident name)
let const_var ident = Const(var ident)

let let_in ident value expr = Let(ident, value, expr)

let lambda params body = Lambda(params, body)

let ifel cond ifexpr elseexpr = IfEl(cond, ifexpr, elseexpr)

let bool b = Const(Bool(b))


let one_plus_two = add (int 1) (int 2)
let let_x = let_in (ident "x") (one_plus_two) (add (Const(var "x")) (int 1)) 
let lambda_exp = Const(lambda [ident "x"; ident "y"] (add (const_var "x") (const_var "y")))
let if_el_expr = ifel (bool true) one_plus_two lambda_exp
(* let prog = App(lambda_exp, [one_plus_two; int 3]) *)

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
    Some e -> PrintBox_text.output stdout (Pretty.program_to_tree e); print_endline "\n"
  | _ -> ()

(* let () = PrintBox_text.output stdout (Pretty.program_to_tree if_el_expr); print_endline "\n" *)
