open Symbol

type fields = symbol * ty

(* 配列型やレコード型の参照比較のためのユニーク値 *)
and unique = unit ref

and ty =
    | UnitTy
    | IntTy
    | StrTy
    (* 任意のRecordのsubtype *)
    | NilTy
    | ArrTy of ty          * unit ref
    | RecTy of fields list * unit ref
    | FuncTy of ty list * ty
    (*
    * 一時的な型、型の相互再帰の検査に利用する
    *)
    | TmpTy of Ast.ty
    | RecursiveTy of symbol * (ty option) ref
[@@deriving show, eq]


(* 2つの型が等しいか検査する *)
let rec type_match t1 t2 = match (t1 , t2) with
    | (RecursiveTy(s1 , _), RecursiveTy(s2 , _)) -> equal_symbol s1 s2

    | (RecursiveTy (_, {contents=Some t1}), t2) -> type_match t1 t2
    | (t1, RecursiveTy (_, {contents=Some t2})) -> type_match t1 t2

    | (ArrTy (t1 , _) , ArrTy (t2 , _)) -> type_match t1 t2
    | (RecTy (fields1 , _), RecTy (fields2 , _)) ->
        List.fold_left
            (fun flag ((s1, t1), (s2, t2)) -> flag && (equal_symbol s1 s2) && (type_match t1 t2))
            true
            (List.combine fields1 fields2)
    | (NilTy , RecTy _)
    | (RecTy _ , NilTy) -> true
    | _ -> equal_ty t1 t2

(*
*  type list = {first: int, rest: list}
*  var a: list = {first: 0, rest: getList()}
*  var a: list = {first: 0, rest: null}
*)

(*
* Tmpty "list" {first: int, rest: list}
* {first: int, rest: RecursiveTy("list")}
*)
