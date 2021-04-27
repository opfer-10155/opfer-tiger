open Position
open Symbol

(* 左辺値 *)
type variable =
  (* 変数 *)
  | SimpleVar    of { id:symbol ; pos:pos }
  (* レコードメンバ *)
  | FieldVar     of { parent:variable; id:symbol; pos:pos }
  (* 配列参照 *)
  | SubscriptVar of { var:variable; index:exp; pos: pos}
[@@deriving show, eq]

(* 二項演算 *)
and bin_op =
  (* 算術演算 *)
  | PlusOp
  | MinusOp
  | TimesOp
  | DivOp

  (* 比較演算 *)
  | LtOp
  | GtOp
  | LeqOp
  | GeqOp
  | EqOp
  | NeqOp

  (* 論理演算 *)
  | AndOp
  | OrOp
[@@deriving show, eq]

(* 引数パラメタ, レコードメンバー修飾子 *)
and param   = Param   of { id:symbol; ty:ty; pos:pos } [@@deriving show, eq]
(* 関数宣言ヘッダ *)
and fundec  = FunDec  of { id:symbol; params:(param list); rty:ty option; body:exp; pos:pos } [@@deriving show, eq]
(* 型宣言ヘッダ *)
and typedec = TypeDec of { id:symbol; ty:ty; pos:pos } [@@deriving show, eq]
(* レコードフィールド代入子 *)
and field   = Field   of { id:symbol; e:exp; pos:pos } [@@deriving show, eq]

(* let宣言 *)
and dec =
  | FunDecs  of fundec list
  | VarDec   of {id:symbol; ty:ty option; e:exp; pos:pos}
  | TypeDecs of typedec list
[@@deriving show, eq]

(* ASTノードとしての型 *)
and ty =
  | RecordTy of {fields:param list; pos:pos}
  | ArrayTy  of {ty:ty; pos:pos}
  (* 型名 *)
  | NameTy   of {id: symbol; pos:pos}
[@@deriving show, eq]


and exp =
  | NilExp    of { pos:pos }
  | IntExp    of { i:int; pos:pos }
  | StrExp    of { s:string; pos:pos }
  | VarExp    of { var:variable; pos:pos }
  | CallExp   of { id:symbol; args:(exp list); pos:pos }
  | OpExp     of { op:bin_op; l:exp; r:exp; pos:pos }
  | SeqExp    of { l:exp; r:exp; }
  | AssignExp of { var:variable; e:exp; pos:pos }
  | IfExp     of { c:exp; t1:exp; t2:(exp option); pos:pos }
  | WhileExp  of { c:exp; body:exp; pos:pos }
  | ForExp    of { id:symbol; low:exp; high:exp; body:exp; pos:pos } (* bool ref *)
  | BreakExp  of { pos:pos }
  | LetExp    of { decs:dec; body:exp; pos:pos }
  | ArrayExp  of { ty:ty; len:exp; init:exp; pos:pos }
  | RecordExp of { fields:field list; pos:pos }
[@@deriving show, eq]


(* expからposを抽出する *)
let rec exp_pos e =
  match e with
  | NilExp    {pos} -> pos
  | IntExp    {pos;_} -> pos
  | StrExp    {pos;_} -> pos
  | VarExp    {pos;_} -> pos
  | CallExp   {pos;_} -> pos
  | OpExp     {pos;_} -> pos
  | SeqExp    { l;_ } -> exp_pos l
  | AssignExp {pos;_} -> pos
  | IfExp     {pos;_} -> pos
  | WhileExp  {pos;_} -> pos
  | ForExp    {pos;_} -> pos
  | BreakExp  {pos;_} -> pos
  | LetExp    {pos;_} -> pos
  | ArrayExp  {pos;_} -> pos
  | RecordExp {pos;_} -> pos


(* variable値からidを抽出 *)
let rec var_toSymbol var =
  match var with
  | SimpleVar {id; _}
  | FieldVar  {id; _}     -> id
  | SubscriptVar {var; _} -> var_toSymbol var

