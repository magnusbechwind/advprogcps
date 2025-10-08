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

let ident_to_tree (Ident(ident)) = make_ident_line ident



let op_to_tree op =
  match op with
  | Ast.Add -> make_keyword_line "Add"

let str_of_op op = 
  match op with
    | Ast.Add -> "Add"


let rec val_to_tree v =
  match v with 
    | Ast.Int(int) -> PBox.hlist ~bars:false [make_info_node_line "Int("; PBox.line (string_of_int int); make_info_node_line ")"]
    | Bool(bool) -> PBox.hlist ~bars:false [make_info_node_line "Bool("; make_keyword_line (if bool then "true" else "false"); make_info_node_line ")"]
    | Var(ident) -> PBox.hlist ~bars:false [make_info_node_line "Var("; ident_to_tree ident; make_info_node_line ")"]
    | Lambda(params, body) -> PBox.tree (make_info_node_line "Lambda")
      [PBox.tree (make_info_node_line "Params") (List.map (fun e -> ident_to_tree e) params); PBox.tree (make_info_node_line "Body") [expr_to_tree body]]
and expr_to_tree e =
  match e with 
    | Const(value) -> val_to_tree value
    | BinOp(op, l, r) -> PBox.tree (make_info_node_line "BinOp") [expr_to_tree l; op_to_tree op; expr_to_tree r]
    | Let(ident, body, in_exp) -> PBox.tree (make_keyword_line "Let") 
        [PBox.hlist ~bars:false [make_info_node_line "Ident: "; ident_to_tree ident]; 
        PBox.hlist ~bars:false [make_info_node_line "Body: "; expr_to_tree body];
        PBox.tree (make_info_node_line "In: ") [expr_to_tree in_exp]]
    | IfEl(cond, thenexpr, elseexpr) ->
        PBox.tree (make_keyword_line "IfElse")
        ([PBox.hlist ~bars:false [make_info_node_line "Cond: "; expr_to_tree cond]; PBox.hlist ~bars:false [make_info_node_line "Then-Branch: "; expr_to_tree thenexpr]] @
        [PBox.hlist ~bars:false [make_info_node_line "Else-Branch: "; expr_to_tree elseexpr]])
    | App(expr, exprs) -> PBox.tree (make_keyword_line "Application") @@
        [PBox.hlist ~bars:false [expr_to_tree expr]; 
        (PBox.tree (make_info_node_line "Args") (List.map expr_to_tree exprs))]

let program_to_tree prog = 
  PBox.tree (make_info_node_line "Program") [expr_to_tree prog]