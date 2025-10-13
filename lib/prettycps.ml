module PBox = PrintBox
open Cps

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

let ident_to_tree (Ast.Ident(ident)) = make_ident_line ident

let rec value_to_tree value = 
  match value with
    | Cps.Int(int) -> PBox.hlist ~bars:false [make_info_node_line "Int("; PBox.line (string_of_int int); make_info_node_line ")"]
    | Bool(bool) -> PBox.hlist ~bars:false [make_info_node_line "Bool("; make_keyword_line (if bool then "true" else "false"); make_info_node_line ")"]
    | Var(ident) -> PBox.hlist ~bars:false [make_info_node_line "Var("; ident_to_tree ident; make_info_node_line ")"]

and fix_to_tree cexp =
  failwith "todo fix_to_tree"

and cps_to_tree cexp =
  match cexp with
    Cps.Halt -> PBox.hlist ~bars:false [make_info_node_line "Halt"]
  | Cps.App (e1, e2) -> PBox.tree (make_keyword_line "App") [PBox.tree (make_info_node_line "Arg") [value_to_tree e1]; PBox.tree (make_info_node_line "Body") (List.fold_left (fun acc x -> value_to_tree x :: acc) [] e2)]
  | Cps.Fix (fix, cexp) ->
    PBox.tree (make_keyword_line "Fix")
    [ PBox.tree (make_keyword_line "Functions") (List.fold_left (fun acc x -> fix_to_tree x :: acc) [] fix);
    PBox.tree (make_info_node_line "Body") [cps_to_tree cexp]]
  | Cps.Tuple (fields, (Ast.Ident id), cexp) ->
    let field_ids acc (v,i) = [make_fieldname_line (string_of_int i); value_to_tree v] @ acc in
      PBox.tree (make_keyword_line "Tuple")
      [ PBox.tree (make_keyword_line "Fields") (List.fold_left field_ids [] fields ); make_keyword_line id; cps_to_tree cexp]
  | Cps.Select (i, v, Ast.Ident id, cexp) -> failwith "cps_to_tree select todo"
  | Cps.Primop (op, vals, ids, cexps) ->
    let valtree = List.fold_left (fun acc x -> value_to_tree x :: acc) [] vals in
    let idtree = List.fold_left (fun acc x -> ident_to_tree x :: acc) [] ids in
    let cexptree = List.fold_left (fun acc x -> cps_to_tree x :: acc) [] cexps in
    PBox.tree (make_keyword_line "Primop") (Pretty.op_to_tree op :: valtree @ idtree @ cexptree)
  | _ -> failwith "missing cases in cps_to_tree"