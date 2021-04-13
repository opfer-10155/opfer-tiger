open Type
open Ast
open Env
open SymTable
open Position

(* TODO: エラーハンドリングをちゃんとする *)
(* TODO: テストを書く *)

exception TypeError of pos


(* 型名 -> 型 *)
let rec actual_ty tenv t =
  match t with
  | NameTy {id; pos} -> (
      match lookup id tenv with
        | Some ty -> ty
        | None -> raise (TypeError pos)
  )
  | ArrayTy {ty; pos} -> (
      let t = actual_ty tenv ty in
      ArrTy (t , ref ())
  )
  | RecordTy {fields; pos} -> (
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
        | None -> raise (TypeError pos)
  )
  | FieldVar {parent; id; pos} -> (
      match typeof_var ctx parent with
      | RecTy (fields , _)  ->
          let (_ , t) = List.find
            (fun (fid , _) -> fid == id)
            fields
          in
          t
      | _ -> raise (TypeError pos)
  )
  | SubscriptVar {var; pos; _} -> (
      match typeof_var ctx var with
      | ArrTy (ty , _) -> ty
      | _ -> raise (TypeError pos)
  )

(* 関数の基本情報 *)
let func_header tenv (FunDec { id; params; rty; body; _ }) =
  (* 実際の返却型 *)
  let ac_rty = match rty with
    | Some nty -> actual_ty tenv nty
    | None -> UnitTy
  in
  (* 実際の引数型 *)
  let ac_paramty (Param {ty; _}) = actual_ty tenv ty in
  (id, List.map ac_paramty params, ac_rty , body)

(* 型定義のヘッダ情報 *)
let tydec_header (TypeDec {id; ty; _}) =
  (id , TmpTy ty)

(* 型定義の循環を調べる *)
let rec find_circulation left_id vty =
  match vty with
  | TmpTy (NameTy {id; _}) -> left_id == id
  | TmpTy (ArrayTy {ty; _}) -> find_circulation left_id (TmpTy ty)
  | TmpTy (RecordTy {fields; _}) ->
      List.fold_left
        (fun flag (Param {ty;_}) -> flag || find_circulation left_id (TmpTy ty))
        false fields
  | _ -> false


(* 仮想型を実際の型に展開 *)
let rec type_expand tenv (left_id , ty) pos =
  if find_circulation left_id ty then raise (TypeError pos)
  else match ty with
  | TmpTy (NameTy {id; pos}) -> (
      match lookup id tenv  with
      | Some t -> type_expand tenv (left_id , t) pos
      | None -> raise (TypeError pos)
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
  | _ -> ty


let rec tycheck tenv ctx exp =
  match exp with
  | IntExp _ -> IntTy
  | NilExp _ -> NilTy
  | StrExp _ -> StrTy
  | VarExp {var; pos} -> typeof_var tenv var
  | OpExp { op; l; r; pos; } ->
      let t1 = tycheck tenv ctx l in
      let t2 = tycheck tenv ctx r in (
        match (t1 , t2) with
        | (IntTy , IntTy) -> IntTy
        | _ -> raise (TypeError pos)
      )
  | SeqExp {l; r} ->
      let _ = tycheck tenv ctx l in
      let tr = tycheck tenv ctx r in
      tr
  | IfExp { c; t1; t2; pos; } ->
      let ty1 = tycheck tenv ctx c in
      let ty2 = tycheck tenv ctx t1 in
      let ty3 = (
        match t2 with
        | Some e -> tycheck tenv ctx e
        | None -> UnitTy
      ) in (
        match (ty1 , ty2 , ty3) with
        | (IntTy , t1 , t2) -> if t1 == t2 then t1 else raise (TypeError pos)
        | _ -> raise (TypeError pos)
      )
  | WhileExp {c; body; pos;} ->
      let t = tycheck tenv ctx c in
      if t == IntTy then
        let _ = tycheck tenv ctx body in
        UnitTy
      else raise (TypeError pos)
  | BreakExp _ -> UnitTy
  | AssignExp { var; e; pos; } ->
      let tv = typeof_var tenv var in
      let te = tycheck tenv ctx e in
      if tv == te then UnitTy else raise (TypeError pos)
  | ForExp { id; low; high; body; pos} ->
      let tl = tycheck tenv ctx low in
      let th = tycheck tenv ctx high in (
        match (tl , th) with
        | (IntTy , IntTy) ->
            let _ = tycheck (enter id IntTy tenv) ctx body in
            UnitTy
        | _ -> raise (TypeError pos)
      )
  | ArrayExp { ty; len; init; pos; } ->
      let t = actual_ty tenv ty in
      let t_init = tycheck tenv ctx init in
      if t == t_init then ArrTy (t , ref()) else raise (TypeError pos)
  | RecordExp { fields; pos; } ->
      RecTy (
        List.map (fun (Field { id; e; pos }) -> (id , tycheck tenv ctx e)) fields,
        ref()
      )
  | CallExp { id; args; pos; } -> (
      match lookup id tenv with
      | Some FuncTy (targs, rty) ->
          if targs == List.map (tycheck tenv ctx) args then rty
          else raise (TypeError pos)
      | Some badty -> raise (TypeError pos)
      | None -> raise (TypeError pos)
  )
  | LetExp { decs; body; pos; } -> (
      match decs with
      | VarDec { id; ty; e; pos; } -> (
          match ty with
          | Some vty ->
              let t = actual_ty tenv vty in
              if t == tycheck tenv ctx e
              then tycheck tenv (enter id t ctx) body
              else raise (TypeError pos)
          | None ->
              let t = tycheck tenv ctx e in
              tycheck tenv (enter id t ctx) body
      )
      | FunDecs fdecs ->
          let headers = List.map (func_header tenv) fdecs in
          let nctx = List.fold_left (
            fun ctx (id , targs , rty , _) -> enter id (FuncTy (targs, rty)) ctx
          ) ctx headers in
          (* 返り値の型チェック *)
          let func_check rty fbody =
            let t = tycheck tenv nctx fbody in
            if rty != t then raise (TypeError pos) in
          let _ = List.map (fun (_, _, rty , body) -> func_check rty body) headers in
          tycheck tenv nctx body
      | TypeDecs tydecs ->
          let headers = List.map tydec_header tydecs in
          let vtenv = List.fold_left (fun env (id , ty) -> enter id ty env) tenv headers in
          let realtypes = List.map (fun (id , ty) -> (id , type_expand vtenv (id , ty) pos)) headers in
          let rtenv = List.fold_left (fun env (id , ty) -> enter id ty env) tenv realtypes in
          tycheck rtenv ctx body
  )


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



