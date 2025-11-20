module PBox = PrintBox
open Ast

(**
  Pretty printer adapted from the Compilers course at AU
*)

(* producing trees for pretty printing *)
let typ_style = PBox.Style.fg_color PBox.Style.Green
let ident_style = PBox.Style.fg_color PBox.Style.Yellow
let fieldname_style = ident_style
let keyword_style = PBox.Style.fg_color PBox.Style.Blue

let info_node_style = PBox.Style.fg_color PBox.Style.Cyan

let make_typ_line name = PBox.line_with_style typ_style name
let make_fieldname_line name = PBox.line_with_style fieldname_style name
let make_ident_line name = PBox.line_with_style ident_style name
let make_keyword_line name = PBox.line_with_style keyword_style name

let make_info_node_line info = PBox.line_with_style info_node_style info

let ident_str = function
| Ast.Ident v -> v
| Wildcard -> "Wildcard"


let ident_to_tree ident = make_ident_line (ident_str ident)

    let str_of_op op = 
  match op with
    | Ast.Add -> "Add"
    | Sub -> "Sub"
    | Mul -> "Mul"
    | Div -> "Div"
    | Callcc -> "Call/CC"
    | Throw -> "Throw"
    | Eq -> "Eq"
    | Lt -> "Lt"
    | Print -> "Print"
    | Println -> "Println"
    | Read -> "Read"
    (* | _ -> raise (Ast.Todo "str_of_op  is missing a case") *)

let op_to_tree op =
  make_keyword_line (str_of_op op)

let rec expr_to_tree e =
  match e with 
    | Ast.Int(int) -> PBox.hlist ~bars:false [make_info_node_line "Int("; PBox.line (string_of_int int); make_info_node_line ")"]
    | Bool(bool) -> PBox.hlist ~bars:false [make_info_node_line "Bool("; make_keyword_line (if bool then "true" else "false"); make_info_node_line ")"]
    | Var(ident) -> PBox.hlist ~bars:false [make_info_node_line "Var("; ident_to_tree ident; make_info_node_line ")"]
    | IfEl(cond, thenexpr, elseexpr) ->
      PBox.tree (make_keyword_line "IfElse")
      ([PBox.hlist ~bars:false [make_info_node_line "Cond: "; expr_to_tree cond]; PBox.hlist ~bars:false [make_info_node_line "Then-Branch: "; expr_to_tree thenexpr]] @
      [PBox.hlist ~bars:false [make_info_node_line "Else-Branch: "; expr_to_tree elseexpr]])
    | App(expr, exprs) -> PBox.tree (make_keyword_line "Application") @@
      [PBox.hlist ~bars:false [expr_to_tree expr]; 
      (* sorry i broke it :-) *)
      PBox.hlist ~bars:false [expr_to_tree exprs]]
    | Primop (op) -> 
      PBox.tree (make_keyword_line (str_of_op op)) []
    | Fn (ident, expr) ->
      PBox.tree (make_keyword_line "Fn") [PBox.tree (make_info_node_line "Arg") [ident_to_tree ident]; PBox.tree (make_info_node_line "Body") [expr_to_tree expr]]
    | Tuple exprs ->
      let f x =
        expr_to_tree x in
      PBox.tree (make_keyword_line "Tuple") (List.map f exprs)
    | Select (i, expr) ->
      PBox.tree (make_keyword_line "Select")
      [PBox.hlist ~bars:false [make_info_node_line "Index: "; PBox.line (string_of_int i)];
        PBox.hlist ~bars:false [make_info_node_line "Expr: "; expr_to_tree expr]]
    | String str -> PBox.tree (make_keyword_line "String") [PBox.line str]
    | _ -> raise (Ast.Todo "missing cases in expr_to_tree")

let program_to_tree prog = 
  PBox.tree (make_info_node_line "Program") [expr_to_tree prog]
