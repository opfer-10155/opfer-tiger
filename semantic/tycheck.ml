open Ast
open SymTable
open Type

(* 型名 -> 型 *)
let rec actual_ty tenv t =
  match t with
  | NameTy {id; pos} -> (
      match lookup id tenv with
        | Some ty -> ty
        | None -> Error.undefined pos "type" id
  )
  | ArrayTy {ty; _} -> (
      let t = actual_ty tenv ty in
      ArrTy (t , ref ())
  )
  | RecordTy {fields; _} -> (
      RecTy (
        List.map (fun (Param {id; ty; _}) -> (id , actual_ty tenv ty)) fields,
        ref ()
      )
  )


(* 左辺値もしくは変数が指す型 *)
let rec typeof_var ctx var =
  match var with
  | SimpleVar {id; pos} -> (
      match lookup id ctx with
      | Some ty -> ty
      | None -> Error.undefined pos "variable" id
  )
  (*
  * 1. parentがRecordである
  * 2. idがparentの属性である
  *)
  | FieldVar {parent; id; pos} -> (
      match typeof_var ctx parent with
      | RecTy (fields , _)  ->
          let (_ , t) =(
            match List.find_opt
              (fun (fid , _) -> Symbol.equal_symbol fid id)
              fields
            with
            | Some field -> field
            | None ->
                Format.printf "type: %s \n" (show_ty (typeof_var ctx parent));
                Error.not_record_argument pos parent id
          ) in
          t
      | badty -> Error.not_record pos parent badty
  )
  | SubscriptVar {var; pos; _} -> (
      match typeof_var ctx var with
      | ArrTy (ty , _) -> ty
      | badty -> Error.not_array pos var badty
  )

(* 関数の基本情報 *)
let func_header tenv (FunDec { id; params; rty; body; _ }) =
  (* 実際の返却型 *)
  let ac_rty = match rty with
    | Some nty -> actual_ty tenv nty
    | None -> UnitTy
  in
  (* 実際の引数型 *)
  let ac_params (Param {id; ty; _}) = (id , actual_ty tenv ty) in
  (id, List.map ac_params params, ac_rty , body)


(* 型定義の循環を調べる *)
(* let rec find_circulation left_id vty =
  match vty with
  | TmpTy (NameTy {id; _}) -> Symbol.equal_symbol left_id id
  | TmpTy (ArrayTy {ty; _}) -> find_circulation left_id (TmpTy ty)
  | TmpTy (RecordTy {fields; _}) ->
      List.fold_left
        (fun flag (Param {ty;_}) -> flag || find_circulation left_id (TmpTy ty))
        false fields
  | _ -> false
 *)
(* let rec parseTydec tenv left_id ty =
  match ty with
  | NameTy {id; pos} -> (
      match lookup id tenv with
      | Some (TmpTy t) ->
          (* 未展開の型 *)
          (* 左辺名と同じなら再帰する *)
          (* 左辺値と違うなら次の展開を待つ *)
          if Symbol.equal_symbol left_id id
          then NameTy(id , tenv)
          else (TmpTy t , false)
      | Some t -> t
      | None -> Error.undefined pos "type" id
  )
  (* 内部を展開する *)
  | ArrayTy {ty; _} -> ArrTy ((parseTydec tenv (left_id , ty)), ref())
  | RecordTy {fields; _} -> (
      RecTy (
        List.map (
          fun (Param {id; ty; _}) ->
            (id , parseTydec tenv (left_id , ty))) fields,
            ref ()
      )
  ) *)

(* 型宣言を展開してヘッダ情報を返す *)
let rec parseTydec tenv (TypeDec {id; ty; pos}) =
  let rec parseTy ty = 
    match ty with
      | NameTy {id; _} -> (
          match lookup id tenv with
          | Some t -> t
          | None -> RecursiveTy (id , ref None)
      )
      | ArrayTy {ty; _} -> ArrTy ((parseTy ty), ref())
      | RecordTy {fields; _} ->
          RecTy (
            List.map
              (fun (Param {id; ty; _}) -> (id , parseTy ty))
              fields,
            ref ()
          )
  in
  (id , parseTy ty, pos)

(*
* !! 副作用を起こす
* tyの中のNameTyに中身(参照先)を与える
*)
let rec parseNameTy tenv ty pos =
  let rec solve subty =
    match subty with
    | RecursiveTy (id , t_ref) -> (
        if !t_ref == None then
          match lookup id tenv with
            | Some t -> t_ref := Some t
            | None -> Error.undefined pos "type" id
        else ()
    )
    | ArrTy (ty , _) -> solve ty
    | RecTy (fields , _) ->
        List.fold_left
          (fun () (_ , t) -> solve t)
          () fields
    | _ -> ()
  in
  solve ty

(*
* let で渡された1つ以上の型定義を展開して、型環境tenvに挿入して返す
* TODO: 循環の検出をする気がありません
*)
let rec parseTydecs tenv tydecs =
  let headers = List.map (parseTydec tenv) tydecs in
  let vtenv = List.fold_left (fun env (id , ty, _) -> enter id ty env) tenv headers in
  let _ = List.map (
    fun (_ , ty, pos) -> parseNameTy vtenv ty pos
  ) headers in
  let rtenv = List.fold_left (fun env (id , ty, _) -> enter id ty env) tenv headers in
  rtenv


let rec tycheck tenv ctx exp =
  match exp with
  | IntExp _ -> IntTy
  | NilExp _ -> NilTy
  | StrExp _ -> StrTy
  | VarExp {var;_} -> typeof_var ctx var
  | OpExp { op; l; r; _ } -> (
    match op with
    (* 数値演算 *)
    | PlusOp | MinusOp | TimesOp | DivOp
    | LtOp | GtOp | LeqOp | GeqOp
    | AndOp | OrOp   -> 
        let _ = type_shouldbe tenv ctx l IntTy in
        let _ = type_shouldbe tenv ctx r IntTy in
        IntTy
    (* 等価比較 *)
    | EqOp | NeqOp ->
        let t = tycheck tenv ctx l in
        let _ = type_shouldbe tenv ctx r t in
        IntTy
  )
  | SeqExp {l; r} ->
      let _ = tycheck tenv ctx l in
      let tr = tycheck tenv ctx r in
      tr
  | IfExp { c; t1; t2;_ } ->
      let _ = type_shouldbe tenv ctx c IntTy in (
        match t2 with
        | Some e ->
            let ty1 = tycheck tenv ctx t1 in
            type_shouldbe tenv ctx e ty1
        | None ->
            type_shouldbe tenv ctx t1 UnitTy
      )
      (* let ty1 = tycheck tenv ctx c in
      let ty2 = tycheck tenv ctx t1 in
      let ty3 = (
        match t2 with
        | Some e -> tycheck tenv ctx e
        | None -> UnitTy
      ) in (
        match (ty1 , ty2 , ty3) with
        | (IntTy , t1 , t2) -> if t1 == t2 then t1 else raise (TypeError pos)
        | _ -> raise (TypeError pos)
      ) *)
  | WhileExp {c; body; _} ->
      let _ = type_shouldbe tenv ctx c IntTy in
      let _ = tycheck tenv ctx body in
      UnitTy
      (* let t = tycheck tenv ctx c in
      if t == IntTy then
        let _ = tycheck tenv ctx body in
        UnitTy
      else type_mismatch pos IntTy t *)
  | BreakExp _ -> UnitTy
  | AssignExp { var; e; _ } ->
      let tv = typeof_var ctx var in
      let _ = type_shouldbe tenv ctx e tv in
      UnitTy
      (* let te = tycheck tenv ctx e in
      if tv == te then UnitTy else raise (TypeError pos) *)
  | ForExp { id; low; high; body; _} ->
      let _ = type_shouldbe tenv ctx low IntTy in
      let _ = type_shouldbe tenv ctx high IntTy in
      let _ = tycheck tenv (enter id IntTy ctx) body in
      UnitTy
      (* let tl = tycheck tenv ctx low in
      let th = tycheck tenv ctx high in (
        match (tl , th) with
        | (IntTy , IntTy) ->
            let _ = tycheck (enter id IntTy tenv) ctx body in
            UnitTy
        | _ -> raise (TypeError pos)
      ) *)
  | ArrayExp { ty; init; _ } ->
      let t = actual_ty tenv ty in
      let _ = type_shouldbe tenv ctx init t in
      ArrTy (t , ref())
      (* let t = actual_ty tenv ty in
      let t_init = tycheck tenv ctx init in
      if t == t_init then ArrTy (t , ref()) else raise (TypeError pos) *)
  | RecordExp { fields; _ } ->
      RecTy (
        List.map (fun (Field { id; e;_ }) -> (id , tycheck tenv ctx e)) fields,
        ref()
      )
  | CallExp { id; args; pos } -> (
      match lookup id ctx with
      | Some FuncTy (t_args, rty) ->
          let _:ty list = List.map
            (fun (t , e) -> type_shouldbe tenv ctx e t)
            (List.combine t_args args)
        in
        rty
      | Some badty -> Error.not_a_function pos id badty
      | None -> Error.undefined pos "function" id
  )
  | LetExp { decs; body; _; } -> (
      match decs with
      | VarDec { id; ty; e; _ } -> (
          match ty with
          | Some vty ->
              let t = actual_ty tenv vty in
              let _ = type_shouldbe tenv ctx e t in
              tycheck tenv (enter id t ctx) body
              (* let t = actual_ty tenv vty in
              if t == tycheck tenv ctx e
              then tycheck tenv (enter id t ctx) body
              else raise (TypeError pos) *)
          | None ->
              let t = tycheck tenv ctx e in
              tycheck tenv (enter id t ctx) body
      )
      | FunDecs fdecs ->
          (*
          * 1. 関数の基本情報を抽出
          * 2. 関数名とその型のペアを作る
          * 3. それを現在の型環境に挿入する
          * 4. 関数のbodyの型検査を行う
          * 5. letのbodyを検査
          *)
          let headers = List.map (func_header tenv) fdecs in
          let fty_list = List.map
            (fun (id , args , rty , _) ->
              let t_args = List.map snd args in
              (id , FuncTy (t_args, rty)))
            headers in
          let nctx = enter_list fty_list ctx in
          let func_check t_args rty fbody =
            type_shouldbe tenv (enter_list t_args nctx) fbody rty
            (* let t = tycheck tenv nctx fbody in
            if rty != t then raise (TypeError pos) in *)
          in
          let _ = List.map (fun (_, t_args, rty , body) -> func_check t_args rty body) headers in
          tycheck tenv nctx body
      | TypeDecs tydecs ->
          (* let headers = List.map tydec_header tydecs in
          let vtenv = List.fold_left (fun env (id , ty) -> enter id ty env) tenv headers in
          let realtypes = List.map (fun (id , ty) -> (id , type_expand vtenv (id , ty) pos)) headers in *)
          let rtenv = parseTydecs tenv tydecs in
          tycheck rtenv ctx body
  )

and type_shouldbe tenv ctx e expected : ty =
  let t = tycheck tenv ctx e in
  if type_match t expected then t
  else
    Error.type_mismatch (exp_pos e) e expected t


(* intは型環境に入っているものなのか？ int -> Int と x -> Int は別物のような気がする *)
(* [int -> Int , x -> Int] : var a : x = 1 が書ける *)

(*
type b = a
type a = int
type c = b

=>
tenv: [int: Int] header:[(b, Name(a)) , (a, Name(int)) , (c, Name(b))]

=>
tenv: [int: Int , b:Name(a) , a:Name(int) , c:Name(b)] header:[(b, Name(a)) , (a, Name(int)) , (c, Name(b))]

=>
tenv: [int: Int , b:Name(a) , a:Name(int) , c:Name(b)] header:[(b, Name(a)) , (a, Name(int)) , (c, Name(b))]

=> headerをtenvに参照する
tenv: [int: Int , b:Name(a) , a:Name(int) , c:Name(b)] header:[(b, Name(int)) , (a, Name(int)) , (c, Name(b))]

=> headerをtenvに参照する
tenv: [int: Int , b:Name(a) , a:Name(int) , c:Name(b)] header:[(b, Name(int)) , (a, IntTy) , (c, Name(a))]

*)



(* 仮想型を実際の型に展開 *)
(* let rec type_expand tenv (left_id , ty) pos =
  (* if find_circulation left_id ty
  then Error.circulation_found pos left_id *)
  (* else  *)
  match ty with
  | TmpTy (NameTy {id; pos}) -> (
      match lookup id tenv  with
      | Some (TmpTy t) -> 
      | Some t -> type_expand tenv (left_id , t) pos
      | None -> Error.undefined pos "type" id
  )
  | TmpTy (ArrayTy {ty; pos}) -> (
      ArrTy ((type_expand tenv (left_id , TmpTy ty) pos) , ref ())
  )
  | TmpTy (RecordTy {fields; pos}) -> (
      RecTy (
        List.map
          (fun (Param {id; ty; _}) ->
            (id , type_expand tenv (left_id , TmpTy ty) pos))
          fields,
        ref()
      )
  )
  | ArrTy (ty , _) -> ArrTy (type_expand tenv (left_id , ty) pos , ref())
  | RecTy (fields, _) ->
      RecTy (
        List.map
          (fun (id , ty) -> (id , type_expand tenv (left_id , ty) pos))
          fields,
        ref()
      )
  | _ -> ty *)

