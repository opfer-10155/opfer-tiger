open Position

type variable =
  | SimpleVar    of { id:string ; pos:pos }
  | FieldVar     of { parent:variable; id:string; pos:pos }
  | SubscriptVar of { var:variable; index:exp; pos: pos}
[@@deriving show, eq]

and bin_op =
  | PlusOp
  | MinusOp
  | TimesOp
  | DivOp
  | LtOp
  | GtOp
  | LeqOp
  | GeqOp
  | AndOp
  | OrOp
  | EqOp
  | NeqOp
[@@deriving show, eq]

and param   = Param   of { id:string; ty:ty; pos:pos }
and fundec  = FunDec  of { id:string; params:(param list); rty:ty option; body:exp; pos:pos }
and typedec = TypeDec of { id:string; ty:ty; pos:pos }
and field   = Field   of { id:string; e:exp; pos:pos }

and dec =
  | FunDecs  of fundec list
  | VarDec   of {var:variable; ty:ty option; e:exp; pos:pos}
  | TypeDecs of typedec list
[@@deriving show, eq]


and ty =
  | RecordTy of {fields:param list; pos:pos}
  | ArrayTy  of {ty:ty; pos:pos}
  | NameTy   of {id: string; pos:pos}
[@@deriving show, eq]

and exp =
  | NilExp    of { pos:pos }
  | IntExp    of { i:int; pos:pos }
  | StrExp    of { s:string; pos:pos }
  | VarExp    of { var:variable; pos:pos }
  | CallExp   of { id:string; args:(exp list); pos:pos }
  | OpExp     of { op:bin_op; l:exp; r:exp; pos:pos }
  | SeqExp    of { l:exp; r:exp; }
  | AssignExp of { var:variable; e:exp; pos:pos }
  | IfExp     of { c:exp; t1:exp; t2:(exp option); pos:pos }
  | WhileExp  of { c:exp; body:exp; pos:pos }
  | ForExp    of { id:string; low:exp; high:exp; body:exp; pos:pos } (* bool ref *)
  | BreakExp  of { pos:pos }
  | LetExp    of { decs:dec; body:exp; pos:pos}
  | ArrayExp  of { ty:ty; len:exp; init:exp; pos:pos }
  | RecordExp of { fields:field list; pos:pos }
[@@deriving show, eq]

(* type exp_info = {
  exp : exp ;
  pos : position
}

type var_info = {
  var : variable ;
  pos : position
}

type dec_info = {
  dec : dec ;
  pos : position
}

type ty_info = {
  ty : ty;
  pos : position
} *)

(* name : type *)
(* and param = string * string * position *)


