open Type
(* open Ast *)

type expty = {exp: unit; ty : ty}

(* transVar : venv * tenv * var -> expty *)

(* let transExp venv tenv (exp: exp) =
  match exp with
  | OpExp { op; l; r; pos; } ->
      let (expty1 , ty1) = transExp venv tenv l in
      let (expty2 , ty2) = transExp venv tenv r in
      match (ty1 , ty2) with
      | (IntTy , IntTy) ->  *)

(* 型検査が通れば中間コード変換も型がつくという仮定 *)
(* 型検査とコード変換のときに別々にシンボルを作らないように、構文解析時にシンボルを生成する *)
