%{
  open Semantic.Ast
  open Semantic.Position
  open Semantic.Symbol
  (* toSymbol は重複して作らないのでここでimportする必要はなかった *)
%}

%token <int> INT
%token <string> ID STR
%token EOF
%token NIL
%token LPAREN RPAREN    // ()
%token LBRACKET RBRACKET // []
%token LBRACE RBRACE    // {}
%token SEMICOLON COLON  // ; :
%token COMMA DOT        // , .
%token EQ COLONEQ       // = :=
%token VAR TYPE FUNCTION    //  var type function
%token PLUS MINUS TIMES DIVIDE  // + - * /
%token EQEQ NEQ LT GT LEQ GEQ LAND LOR // == != < > <= >= & |
%token IF THEN ELSE  // if else
%token WHILE DO   // while do
%token FOR TO     // for to
%token BREAK      // break
%token ARRAY OF   // array of
%token LET IN END // let ... in ... end
%token UMINUS     // unary ops
%token RECORD     // record
%token AND        // and

%nonassoc COLONEQ
%nonassoc DO THEN
%nonassoc ELSE OF
%nonassoc EQEQ NEQ
%nonassoc GEQ LEQ LT GT
%left SEMICOLON
%left LOR LAND
%left PLUS MINUS
%left TIMES DIVIDE
%left UMINUS

%start <exp> program

%%

program:
  | e=exp EOF {e}

// 式
exp:
  | v=value                                           { v }
  | v=left_value                                      { VarExp { var=v; pos=to_pos($startpos) } }

  // 括弧で閉じる
  | LPAREN e=exp RPAREN                               { e }

  // 関数適用
  | s=ID LPAREN es=separated_list(COMMA, exp) RPAREN  { CallExp { id=toSymbol s; args=es; pos=to_pos($startpos) } }

  // 二項演算
  | e=bin_op_exp                                      { e }

  // 代入 v := e
  | v = left_value COLONEQ e = exp                    { AssignExp { var = v; e = e; pos = to_pos($startpos)} }

  // 単項マイナス
  | MINUS e=exp %prec UMINUS                          { OpExp { op=MinusOp; l=IntExp {i=0; pos=dummy_pos}; r=e; pos=to_pos($startpos)} }

  // if then else
  | IF e1=exp THEN e2=exp ELSE e3=exp                 { IfExp { c=e1; t1=e2; t2=(Some e3); pos=to_pos($startpos)} }
  // if then
  | IF e1=exp THEN e2=exp                             { IfExp { c=e1; t1=e2; t2=None; pos=to_pos($startpos)} }

  // while condition do ...
  | WHILE e1=exp DO e2=exp                            { WhileExp { c=e1; body=e2; pos=to_pos($startpos) } }

  // forループ for i = e1 to e2 do exp
  | FOR s=ID EQ e1=exp TO e2=exp DO body=exp          { ForExp { id=toSymbol s; low=e1; high=e2; body=body; pos=to_pos($startpos) } }

  | BREAK                                             { BreakExp { pos=to_pos($startpos) } }

  // シークエンス
  | e1=exp SEMICOLON e2=exp                           { SeqExp { l=e1; r=e2; } }// a = 10; a + 1

  // let式
  // let
  //     typedecs
  //     funcdecs
  //     vardec
  //  in e end
  // 上記のような式は
  // let typedecs in
  // let funcdecs in
  // let vardec  in e end
  // の糖衣構文とする
  | LET ds=nonempty_list(decs) IN e=exp END
  {
    List.fold_right
      (fun dec e -> LetExp{decs=dec; body=e; pos=to_pos($startpos)})
      ds
      e
  }




value:
  // 数値
  | i=INT                                             { IntExp { i=i; pos=to_pos($startpos) }  }
  // 文字列値
  | s=STR                                             { StrExp { s=s; pos=to_pos($startpos) } }
  | NIL                                               { NilExp { pos=to_pos($startpos) } }
  // 配列初期化  array int[8] of 0
  | ARRAY ty=type_exp LBRACKET len=exp RBRACKET OF e=exp    { ArrayExp { ty=ty; len=len; init=e; pos=to_pos($startpos) } }
  // レコード初期化 {x=10}
  | LBRACE fs=fields RBRACE                           { RecordExp { fields=fs; pos=to_pos($startpos) } }

// フィールド
fields:
  | fs=separated_list(COMMA, field) { fs }

field:
  | s=ID EQ e=exp { Field { id=toSymbol s ; e=e; pos=to_pos($startpos) } }

// 宣言
decs:
  // 変数宣言
  | d = var_dec                               { d }
  // 相互再帰ありの関数宣言
  | fs=separated_nonempty_list(AND, func_dec) { FunDecs fs }
  // 相互再帰ありの型宣言
  | ts=separated_nonempty_list(AND, type_dec) { TypeDecs ts }


// 変数宣言
var_dec:
  | VAR s=ID EQ e=exp                       { VarDec { id=toSymbol s; ty=None; e=e; pos=to_pos($startpos) } }
  | VAR s=ID COLON ty=type_exp EQ e=exp     { VarDec { id=toSymbol s; ty=(Some ty); e=e; pos=to_pos($startpos) } }

// 関数宣言  返値型ありor返値型なし
func_dec:
  | FUNCTION name=ID LPAREN params=type_fields RPAREN EQ body=exp                     { FunDec {id=toSymbol name; params=params; rty=None;     body=body; pos=to_pos($startpos)} }
  | FUNCTION name=ID LPAREN params=type_fields RPAREN COLON ty=type_exp EQ body=exp   { FunDec {id=toSymbol name; params=params; rty=Some ty;  body=body; pos=to_pos($startpos)} }

// 左辺値もしくは変数参照
left_value:
  // x
  | s=ID                                  { SimpleVar    { id=toSymbol s; pos=to_pos($startpos) } }
  // arr[1]
  | v=left_value DOT s=ID                 { FieldVar     { parent=v; id=toSymbol s; pos=to_pos($startpos) } }
  // record.member
  | v=left_value LBRACKET e=exp RBRACKET  { SubscriptVar { var=v; index=e; pos=to_pos($startpos) } }


// 型表現
type_exp:
  // 型名
  | t=ID                                      { NameTy {id=toSymbol t; pos=to_pos($startpos)} }
  // record型 record {x: int}
  | RECORD LBRACE params=type_fields RBRACE   { RecordTy { fields=params; pos=to_pos($startpos) } }
  // 配列型 [int]
  | LBRACKET ty=type_exp RBRACKET             { ArrayTy  { ty=ty; pos=to_pos($startpos) }}

// 型フィールド
type_fields:
  | fs=separated_list(COMMA, type_field) { fs }

// 属性
type_field:
  | s=ID COLON ty=type_exp { Param { id=toSymbol s; ty=ty; pos=to_pos($startpos) } }

// 型の文法
// 型宣言
type_dec:
  | TYPE s=ID EQ ty=type_exp { TypeDec { id=toSymbol s; ty=ty; pos=to_pos($startpos)} }


// 二項演算
bin_op_exp:
  | e1=exp PLUS   e2=exp {OpExp {op=PlusOp; l=e1; r=e2; pos=to_pos($startpos)} }
  | e1=exp MINUS  e2=exp {OpExp {op=MinusOp; l=e1; r=e2; pos=to_pos($startpos)} }
  | e1=exp TIMES  e2=exp {OpExp {op=TimesOp; l=e1; r=e2; pos=to_pos($startpos)} }
  | e1=exp DIVIDE e2=exp {OpExp {op=DivOp; l=e1; r=e2; pos=to_pos($startpos)} }
  | e1=exp LT     e2=exp {OpExp {op=LtOp; l=e1; r=e2; pos=to_pos($startpos)} }
  | e1=exp GT     e2=exp {OpExp {op=GtOp; l=e1; r=e2; pos=to_pos($startpos)} }
  | e1=exp LEQ    e2=exp {OpExp {op=LeqOp; l=e1; r=e2; pos=to_pos($startpos)} }
  | e1=exp GEQ    e2=exp {OpExp {op=GeqOp; l=e1; r=e2; pos=to_pos($startpos)} }
  | e1=exp LAND   e2=exp {OpExp {op=AndOp; l=e1; r=e2; pos=to_pos($startpos)} }
  | e1=exp LOR    e2=exp {OpExp {op=OrOp; l=e1; r=e2; pos=to_pos($startpos)} }
  | e1=exp EQEQ   e2=exp {OpExp {op=EqOp; l=e1; r=e2; pos=to_pos($startpos)} }
  | e1=exp NEQ    e2=exp {OpExp {op=NeqOp; l=e1; r=e2; pos=to_pos($startpos)} }

// bin_op:
//   | PLUS   {}
//   | MINUS  {}
//   | TIMES  {}
//   | DIVIDE {}
//   | LT     {}
//   | GT     {}
//   | LEQ    {}
//   | GEQ    {}
//   | LAND   {}
//   | LOR    {}
//   | EQEQ   {}
//   | NEQ    {}
