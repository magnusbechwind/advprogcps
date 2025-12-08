open Lib


module StringMap = Map.Make(String)

let run_test path = 
  let ast = Option.get @@ Fileparser.parse path in
  let cps = Transform.cps ast in 
  let cps_beta = Beta.beta_contract cps in
  let cps_fold = Constfold.const_fold cps in
  let cps_all = Optim.fix [Beta.beta_contract; Constfold.const_fold] cps in

  List.map Data.complexity [cps;  cps_beta; cps_fold; cps_all]


let print_entry (name, comps) = 
  let open Pretty in
  let comp_trees = List.map Data.comp_to_tree comps in
  
  print_endline "\n";
  PrintBox_text.output stdout @@ PrintBox.tree (make_ident_line name) comp_trees

let write_to_file output =
  let oc = open_out "data.txt" in
  Printf.fprintf oc output

let string_of_comp (comp: Data.complexity) = 
  Printf.sprintf "\tTotal: %d\n\tSimple: %d\n\tApp: %d" comp.total comp.simple comp.app

let save_entry oc (name, comps) = 

  let comp_strings = List.map string_of_comp comps in
  let cps = List.nth comp_strings 0 in
  let beta = List.nth comp_strings 1 in
  let fold = List.nth comp_strings 2 in
  let all = List.nth comp_strings 3 in

  Printf.fprintf oc "%s \nCPS:\n%s\nBeta:\n%s\nFold:\n%s\nAll:\n%s\n\n" name cps beta fold all

let _ =
  let test_dirs = ["./test/test_programs"; "./test/callcc"] in

  let names = List.fold_left (fun acc dir -> Sys.readdir dir |> Array.to_list |> List.map (fun name -> dir ^ "/" ^ name) |> List.append acc) [] test_dirs in
  let names =  List.filter (fun name -> String.ends_with ~suffix:".lambda" name) names in

  let complexities = List.fold_left (fun acc name -> acc @ [name, run_test name]) [] names in

  let oc = open_out "data.txt" in
  List.iter (print_entry) complexities;
  List.iter (save_entry oc) complexities;
  close_out oc