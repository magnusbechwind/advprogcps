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

let ident_str (Ast.Ident v) = v

let rec value_to_tree value = 
  match value with
    | Cps.Int(int) -> PBox.hlist ~bars:false [make_info_node_line "Int("; PBox.line (string_of_int int); make_info_node_line ")"]
    | Bool(bool) -> PBox.hlist ~bars:false [make_info_node_line "Bool("; make_keyword_line (if bool then "true" else "false"); make_info_node_line ")"]
    | Var(ident) -> PBox.hlist ~bars:false [make_info_node_line "Var("; ident_to_tree ident; make_info_node_line ")"]

and fix_to_tree = function
| (id, ids, a) -> 
  let x = PBox.tree (make_info_node_line "Args") (List.map ident_to_tree ids) in
  let y = PBox.tree (make_info_node_line "Cont.") [cps_to_tree a] in
  let z = [x; y] in
  let w = PBox.tree (make_keyword_line (ident_str id)) z in w

and cps_to_tree cexp =
  match cexp with
    Cps.Halt v -> PBox.hlist ~bars:false [make_info_node_line "Halt"; value_to_tree v]
  | Cps.App (e1, e2) -> PBox.tree (make_keyword_line "App") [PBox.tree (make_info_node_line "Fun") [value_to_tree e1]; PBox.tree (make_info_node_line "Args") (List.map value_to_tree e2)]
  | Cps.Fix (fix, cexp) ->
    PBox.tree (make_keyword_line "Fix")
    [ PBox.tree (make_keyword_line "Functions") (List.map fix_to_tree fix);
    PBox.tree (make_info_node_line "Body") [cps_to_tree cexp]]
  | Cps.Tuple (fields, (Ast.Ident id), cexp) ->
    let field_ids acc (v,i) = [make_fieldname_line (string_of_int i); value_to_tree v] @ acc in
      PBox.tree (make_keyword_line "Tuple")
      [ PBox.tree (make_keyword_line "Fields") (List.fold_left field_ids [] fields); make_keyword_line id; cps_to_tree cexp]
  | Cps.Select (_, _, Ast.Ident _, _) -> failwith "cps_to_tree select todo"
  | Cps.Primop (op, vals, ids, cexps) ->
    let valtree = List.map value_to_tree vals in
    let idtree = List.map ident_to_tree ids in
    let cexptree = List.map cps_to_tree cexps in
    PBox.tree (make_keyword_line "Primop") (Pretty.op_to_tree op :: valtree @ idtree @ cexptree)
  (* | _ -> failwith "missing cases in cps_to_tree" *)

and value_repr = function
| Cps.Var (Ast.Ident v) -> v
| Cps.Int i -> string_of_int i
| Cps.Bool b -> string_of_bool b

and cps_ast_repr = function
| Cps.Halt v ->
  "(halt)"^value_repr v
| Cps.App (v, vals) ->
  "(app)"^value_repr v ^ List.fold_left (fun acc x -> acc ^ " " ^ value_repr x) "" vals
| Cps.Fix (fix, cexp) ->
  "(fix)"^List.fold_left
    (fun acc (id, ids, cexp') -> "let " ^ ident_str id ^ " =" ^ List.fold_left
      (fun acc' x' -> acc' ^ " " ^ ident_str x'
      ) "" ids ^
      acc ^ "->\n" ^ cps_ast_repr cexp' ^ "\n"
    ) " " fix ^
    " in\n(fix end)\n" ^ cps_ast_repr cexp
| Cps.Tuple (vals, id, cexp) ->
  "(tuple)"^"let " ^ ident_str id ^ " = (" ^ value_repr (fst (List.hd vals)) ^ List.fold_left (fun acc (x,_) -> ", " ^ value_repr x ^ acc) "" (List.tl vals) ^ ") in \n" ^ cps_ast_repr cexp
| Cps.Select _ -> failwith "Select not implemented (in cps_ast_repr)"
| Cps.Primop (op, [a;b], [id], cexps) -> 
  let op = match op with
  | Ast.Add -> "+"
  | Ast.Sub -> "-"
  | Ast.Mul -> "*"
  | Ast.Div -> "/"
  | _ -> failwith "unreachable"
in
  "(primop)"^"let " ^ ident_str id ^ " = " ^ value_repr a ^ " " ^ op ^ " " ^ value_repr b ^ "" ^ List.fold_left (fun acc x -> " in\n" ^ cps_ast_repr x ^ acc ) "" cexps
  | _ -> failwith "abc"