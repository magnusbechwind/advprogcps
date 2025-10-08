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
let prog = App(lambda_exp, [one_plus_two; int 3])




let () = PrintBox_text.output stdout (Pretty.program_to_tree prog); print_endline "\n"
