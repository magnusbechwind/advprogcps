open Cps

  let (let*) = Option.bind
  let ret x = Some x

  let try_to_int = function
  | Int i -> Some i
  | _ -> None

  let to_bool = function
  | Bool b -> Some b
  | _ -> None

  let all_to f ls = 
    List.fold_left (fun acc e ->
      let* e' = f e in
      let* acc' = acc in
      ret (e' :: acc')
    ) (ret []) ls

  let to_ints = all_to try_to_int
  let to_bools = all_to to_bool

  let sum = List.fold_left (+) 0
  let dif = function
  | x :: xs -> List.fold_left (-) x xs
  | [] -> 0
  let prod = List.fold_left (Int.mul) 1
  let div = function
  | x :: xs -> List.fold_left (/) x xs
  | [] -> 0
  let eq = function
  | x :: xs -> x :: xs |> List.fold_left (fun acc y ->
      let* acc' = acc in
      if acc' = y then ret y else None
    ) (ret x) |> Option.is_some
  | [] -> true

  type 'a lt_res =
  | First
  | HoldsTo of 'a
  | Fails

  let lt ls = 
    let rec lt_aux acc ls = 
      match ls with
      | [] -> acc
      | x :: xs -> match acc with
        | First -> lt_aux (HoldsTo x) xs
        | HoldsTo acc' -> lt_aux (if acc' < x then HoldsTo x else Fails) xs
        | Fails -> Fails
    in
    let res = lt_aux First ls in
    match res with
    | First -> failwith "not enough arguments to lt"
    | HoldsTo _ -> true
    | Fails -> false

  let ret_int x = Int x
  let ret_bool x = Bool x

  (**
    Attempts constant-folding over primop.

    Returns [Some v] where [v] is the result of constant folding over
    operation [op] with values [vs] if constant folding is possible in this case,
    returns [None] otherwise.
  *)
  let primop_const_simpl op vs =
    match op with
    | Ast.Add -> vs |> to_ints |> Option.map sum |> Option.map ret_int
    | Ast.Sub -> vs |> to_ints |> Option.map dif |> Option.map ret_int
    | Ast.Mul -> vs |> to_ints |> Option.map prod |> Option.map ret_int
    | Ast.Div -> vs |> to_ints |> Option.map div |> Option.map ret_int
    | Ast.Eq -> begin match to_ints vs with
      | Some vs' -> vs' |> eq |> ret_bool |> ret
      | None -> vs |> to_bools |> Option.map eq |> Option.map ret_bool
      end
    | Ast.Lt -> vs |> to_ints |> Option.map lt |> Option.map ret_bool
    | Ast.Callcc | Ast.Throw | Ast.Print | Ast.Println | Ast.Read -> None

  let to_id = function
  | Var id -> id
  | _ -> failwith "expected id"

  type tuple_env = Ast.ident -> value list option

  let tuple_env0 _ = None
  let insert (id : Ast.ident) (tuple : value list) (env : tuple_env) = 
    fun w -> if w = id then Some tuple else env w

  let lookup_tuple env i v =
    let id = to_id v in
    let* tuple = env id in
    List.nth_opt tuple i

  let rec decl_const_fold env (f, params, body) = (f, params, const_fold_aux env body)

  and decls_const_fold env = List.map (decl_const_fold env)

  and select_const_fold env i v id c =
    match lookup_tuple env i v with
    | Some v' -> Beta.subst id v' c
    | None -> Select (i, v, id, const_fold_aux env c)

  and switch_const_fold env v cs =
    begin match try_to_int v with
    | Some i -> List.nth cs i |> const_fold_aux env
    | None -> Switch (v, List.map (const_fold_aux env) cs)
    end

  and primop_const_fold env op vs id c =
    let v_opt = primop_const_simpl op vs in
    begin match v_opt with
    | None -> Primop (op, vs, [id], [const_fold_aux env c])
    | Some v ->
      let c' = Beta.subst id v c in
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
  | Switch (v, cs) -> switch_const_fold env v cs
  | Primop (op, vs, [id], [c]) -> primop_const_fold env op vs id c
  | Primop (op, vs, ids, cs) -> Primop (op, vs, ids, List.map (const_fold_aux env) cs)

  let const_fold = const_fold_aux tuple_env0

