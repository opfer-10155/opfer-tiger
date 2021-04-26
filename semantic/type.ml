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
[@@deriving show, eq]


(* 2つの型が等しいか検査する *)
let type_match t1 t2 = match (t1 , t2) with
    | (ArrTy (_ , u1) , ArrTy (_ , u2))
    | (RecTy (_ , u1), RecTy (_ , u2))  ->  u1 == u2
    | (NilTy , RecTy _)
    | (RecTy _ , NilTy) -> true
    | _ -> t1 == t2

