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
type complexity_field = Total | Simple | App | Halt | Fix | Tuple | Select | Switch | Primop

let empty_details = {halt = 0; app = 0;fix = 0; tuple = 0; select = 0; switch = 0; primop = 0}
let empty_comp = {total = 0; simple = 0; app = 0; details = empty_details}


(** Module for comparing complexities *)
module Compare = struct
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

  let diff_details d1 d2 = 
    {halt = d1.halt - d2.halt; 
     app = d1.app - d2.app; 
     fix = d1.fix - d2.fix; 
     tuple = d1.tuple - d2.tuple; 
     select = d1.select - d2.select; 
     switch = d1.switch - d2.switch; 
     primop = d1.primop - d2.primop}

  (** Computes the difference between complexities. I.e. {field = comp1.field - comp2.field} for all fields *)
  let diff_complexity comp1 comp2 =
    {total = comp1.total - comp2.total; 
     simple = comp1.simple - comp2.simple; 
     app = comp1.app - comp2.app; 
     details = diff_details comp1.details comp2.details}

  let div_comp_n comp n = 
    let details = comp.details in
    {total = comp.total / n;
     simple = comp.simple / n;
     app = comp.app / n;
     details = {
       halt = details.halt / n;
       app = details.app / n; 
       fix = details.fix / n;
       tuple = details.tuple / n;
       select = details.select / n;
       switch = details.switch / n;
       primop = details.primop / n;
     }}

  (** Takes a list of complexities 'comps' and reuturns a complexity with the average at each field. Assumes length of 'comps' > 0 *)
  let avg comps = 
    let total = List.fold_left (fun acc comp -> add_complexity acc comp) empty_comp comps in
    let n = List.length comps in
    div_comp_n total n

  let get_field comp field =
    match field with 
    | Total -> comp.total
    | Simple -> comp.simple
    | App -> comp.app
    | Halt -> comp.details.halt
    | Fix -> comp.details.fix
    | Tuple -> comp.details.tuple
    | Select -> comp.details.select
    | Switch -> comp.details.switch
    | Primop -> comp.details.primop


  (** Compares complexities on, possibly different, fields. Returns 0 if comp1.field1 == comp2.field2, 
      negative integer if comp1.field1 < comp2.field2 and positive integer if comp1.field1 > comp2.field2 *)
  let compare_by2 comp1 comp2 field1 field2 = 
    let field1 = get_field comp1 field1 in
    let field2 = get_field comp2 field2 in
    compare field1 field2

  (** Compares complexities by given comparison criteria. Returns 0 if comp1.comparison_field == comp2.comparison_field, 
      negative integer if comp1.comparison_field < comp2.comparison_field and positive integer if comp1.comparison_field > comp2.comparison_field *)
  let compare_by comp1 comp2 field =
    compare_by2 comp1 comp2 field field

  (** Returns the maximal complexity when comparing by given field *)
  let max_by_field comps field =
    List.fold_left (fun acc comp -> if (compare_by acc comp field >= 0) then acc else comp) empty_comp comps

  (** Returns the minimal complexity when comparing by given field *)
  let min_by_field comps field = 
    List.fold_left (fun acc comp -> if (compare_by acc comp field <= 0) then acc else comp) empty_comp comps

  let max_by_total comps = max_by_field comps Total

  (** true iff comp1.field >= comp2.field *)
  let greater_by_field comp1 comp2 field =
    compare_by comp1 comp2 field >= 0

  (** true iff comp1.field <= comp2.field *)
  let smaller_by_field comp1 comp2 field =
    not @@ greater_by_field comp1 comp2 field
end

let rec complexity (cexp: cexpr) : complexity = let open Compare in match cexp with
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


  (* Take "worst case scenario" *)
  | Switch (_, expr_list) -> 

    let worst_case = List.fold_left (fun acc expr -> max_by_total [acc; (complexity expr)]) empty_comp expr_list in

    (* Take the most complex expr (by total nodes) and add the switch node itself *)
    {worst_case with total = worst_case.total + 1; 
                     simple = worst_case.simple + 1; 
                     details = {worst_case.details with switch = worst_case.details.switch + 1}}

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