open Cps
module P = PrintBox

(** Helper module to calculate "complexity" of a CPS tree. For evaluation of optimizations *)



(** Complexity of cps expression. 
Total is total amount of AST nodes
Simple is amount of 'simple nodes' Halt | Fix | Tuple | Select | Switch | Primop
App is amount of function applications
Detailed is a count of every node.
*)
type complexity = {total: int; simple: int; app: int; details: details} 
and details = {halt: int; app: int; fix: int; tuple: int; select: int; switch: int; primop: int}

let empty_details = {halt = 0; app = 0;fix = 0; tuple = 0; select = 0; switch = 0; primop = 0}
let empty_comp = {total = 0; simple = 0; app = 0; details = empty_details}

let add_details d1 d2 =
{halt = d1.halt + d2.halt; 
app = d1.app + d2.app; 
fix = d1.fix + d2.fix; 
tuple = d1.tuple + d2.tuple; 
select = d1.select + d2.select; 
switch = d1.switch + d2.switch; 
primop = d1.primop + d2.primop}

let add_complexity (comp1: complexity) (comp2: complexity) : complexity =
  {total = comp1.total + comp2.total; 
  simple = comp1.simple + comp2.simple; 
  app = comp1.app + comp2.app; 
  details = add_details comp1.details comp2.details}

let rec complexity (cexp: cexpr) : complexity = match cexp with
| Halt _ -> {empty_comp with total = 1; simple = 1; details = {empty_details with halt = 1}}
| App (_, _) -> {empty_comp with total = 1; app = 1; details = {empty_details with app = 1}}

(* Counting function complexity at declaraiton time doesn't differentiate between complex functions called once and many times.  
Worth considering binding identifiers to expressions and counting complexity at point of applicaton. *)
| Fix (decls, expr) -> 
  
  (* Add the complexity of all the declared functions *)
  let decl_comp = List.fold_left (fun acc (_, _, dexpr) -> add_complexity acc (complexity dexpr)) empty_comp decls in

  let comp = add_complexity decl_comp @@ complexity expr in

  {comp with total = comp.total + 1; 
  simple = comp.simple + 1; 
  details = {comp.details with fix = comp.details.fix + 1}}

(* Likewise, if this is a tuple declaration, it might be prudent to count complexity at selection site *)
| Tuple (_, _, expr) -> 
  
  let expr_comp = complexity expr in

  {expr_comp with total = expr_comp.total + 1; 
  simple = expr_comp.simple + 1; 
  details = {expr_comp.details with tuple = expr_comp.details.tuple + 1}}

| Select (_, _, _, expr) -> 
  let comp = complexity expr in

  {comp with total = comp.total + 1; 
  simple = comp.simple + 1; 
  details = {comp.details with select = comp.details.select + 1}}

(* TODO: How to measure complexity of switch statements. Adding complexity of all branches does not seem fair *)
| Switch (_, expr_list) -> failwith "TODO, measure complexity of switch"

(* TODO: Might be prudent to also take the op into account. Call\cc, throw etc more complex than Add *)
| Primop (_, _, _, expr_list) -> 
  let comp = List.fold_left (fun acc expr -> add_complexity acc (complexity expr)) empty_comp expr_list in

  {comp with total = comp.total + 1; 
  simple = comp.simple + 1; 
  details = {comp.details with primop = comp.details.primop + 1}}

let details_to_tree d = let open Pretty in
  P.tree (make_typ_line "Details") [
    PBox.hlist ~bars:false [make_info_node_line "Halt: "; P.line (string_of_int d.halt)];
    PBox.hlist ~bars:false [make_info_node_line "App: "; P.line (string_of_int d.app)];
    PBox.hlist ~bars:false [make_info_node_line "Fix: "; P.line (string_of_int d.fix)];
    PBox.hlist ~bars:false [make_info_node_line "Tuple: "; P.line (string_of_int d.tuple)];
    PBox.hlist ~bars:false [make_info_node_line "Select: "; P.line (string_of_int d.select)];
    PBox.hlist ~bars:false [make_info_node_line "Switch: "; P.line (string_of_int d.switch)];
    PBox.hlist ~bars:false [make_info_node_line "Primop: "; P.line (string_of_int d.primop)];
  ]
  
let comp_to_tree ?(show_details = false) (comp: complexity) = let open Pretty in

  let {total; simple; app; details} = comp in

  let details = if show_details then details_to_tree details else P.empty in

  P.tree (make_typ_line "Complexity") @@ [
    P.hlist ~bars:false [make_info_node_line "Total: "; P.line (string_of_int total)];
    P.hlist ~bars:false [make_info_node_line "Simple: "; P.line (string_of_int simple)];
    P.hlist ~bars:false [make_info_node_line "App: "; P.line (string_of_int app)];
    details
  ]