open Cps

let (let*) = Option.bind
let ret x = Some x

let try_to_int = function
| Int i -> Some i
| _ -> None

let try_to_bool = function
| Bool b -> Some b
| _ -> None

let all_to f ls = 
  ls |> List.fold_left (fun acc e ->
    let* e' = f e in
    let* acc' = acc in
    ret (e' :: acc')
  ) (ret []) |> Option.map List.rev

let ret_int x = Int x
let ret_bool x = Bool x

let try_to_int2 tuple =
  let (x, y) = tuple in
  let* x' = try_to_int x in
  let* y' = try_to_int y in
  ret (x', y')

let try_to_consts = all_to (fun v -> begin match v with
  | Cps.Var _ -> None
  | Cps.Int i -> Some(Cps.Int i)
  | Cps.Bool b -> Some(Cps.Bool b)
  | Cps.String s -> Some(Cps.String s)
  end
)

let to_pair m =
  match m with
| x :: y :: [] -> (x, y)
| _ -> failwith "too many arguments to primop"

(**
  Attempts constant-folding over primop.

  Returns [Some v] where [v] is the result of constant folding over
  operation [op] with values [vs] if constant folding is possible in this case,
  returns [None] otherwise.
*)
let primop_const_simpl op vs =
  let* vs' = try_to_consts vs in
  match op with
  | Ast.Add -> vs' |> to_pair |> try_to_int2 |> Option.map (fun (x, y) -> x + y) |> Option.map ret_int
  | Ast.Sub -> vs' |> to_pair |> try_to_int2 |> Option.map (fun (x, y) -> x - y) |> Option.map ret_int
  | Ast.Mul -> vs' |> to_pair |> try_to_int2 |> Option.map (fun (x, y) -> x * y) |> Option.map ret_int
  | Ast.Div -> vs' |> to_pair |> try_to_int2 |> Option.map (fun (x, y) -> x / y) |> Option.map ret_int
  | Ast.Eq -> begin match to_pair vs' with
    | (Int x, Int y) -> (x = y) |> ret_bool |> Option.some
    | (Bool x, Bool y) -> (x = y) |> ret_bool |> Option.some
    | (String x, String y) -> (x = y) |> ret_bool |> Option.some
    | _ -> failwith "illegal argument type to == comparison"
    end
  | Ast.Lt -> begin match to_pair vs' with
    | (Int x, Int y) -> (x < y) |> ret_bool |> Option.some
    | _ -> failwith "illegal argument type to < comparison"
    end
  | Ast.Callcc | Ast.Throw | Ast.Print | Ast.Println | Ast.Read -> None

type tuple_env = Ast.ident -> value list option

let tuple_env0 _ = None
let insert (id : Ast.ident) (tuple : value list) (env : tuple_env) = 
  fun w -> if w = id then Some tuple else env w

let get_or_fail fail = function
| Some x -> x
| None -> failwith fail

let lookup_tuple env i v =
  let id = v |> Cps.try_to_id |> get_or_fail "lookup tuple with non-var" in
  let* tuple = env id in
  List.nth_opt tuple i

let rec decl_const_fold env (f, params, body) = (f, params, const_fold_aux env body)

and decls_const_fold env = List.map (decl_const_fold env)

and select_const_fold env i v id c =
  match lookup_tuple env i v with
  | Some v' -> Optim.subst id v' c
  | None -> Select (i, v, id, const_fold_aux env c)

and switch_const_fold env v cs =
  begin match try_to_bool v with
  | Some true -> List.nth cs 0 |> const_fold_aux env
  | Some false -> List.nth cs 1 |> const_fold_aux env
  | None -> If (v, List.map (const_fold_aux env) cs)
  end

and primop_const_fold env op vs id c =
  let v_opt = primop_const_simpl op vs in
  begin match v_opt with
  | None -> Primop (op, vs, [id], [const_fold_aux env c])
  | Some v ->
    let c' = Optim.subst id v c in
    const_fold_aux env c'
  end

(** Applies constant-folding over CPS program. *)
and const_fold_aux (env : tuple_env) = function
| Halt v -> Halt v
| App (op, vs) -> App (op, vs)
| Fix (decls, c) -> Fix (decls_const_fold env decls, const_fold_aux env c)
| Tuple (vs, id, c) ->
  let env' = insert id vs env in
  Tuple (vs, id, const_fold_aux env' c)
| Select (i, v, id, c) -> select_const_fold env i v id c
| If (v, cs) -> switch_const_fold env v cs
| Primop (op, vs, [id], [c]) -> primop_const_fold env op vs id c
| Primop (op, vs, ids, cs) -> Primop (op, vs, ids, List.map (const_fold_aux env) cs)

let const_fold = const_fold_aux tuple_env0

