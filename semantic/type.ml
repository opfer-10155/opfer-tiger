open Symbol

type fields = symbol * ty

(* 配列型やレコード型の参照比較のためのユニーク値 *)
and unique = unit ref

and ty =
  | UnitTy
  | IntTy
  | StrTy
  (* | NilTy *)
  | ArrTy of ty          * unit ref [@equal fun (ArrTy (_ , u1)) (ArrTy (_ , u2)) -> u1 == u2]
  | RecTy of fields list * unit ref [@equal fun (RecTy (_ , u1)) (RecTy (_ , u2)) -> u1 == u2]
  | FuncTy of ty list * ty
  | TmpTy of Ast.ty
[@@deriving show, eq]

