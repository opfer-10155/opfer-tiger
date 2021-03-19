%{
  (*open Core.Pos
  let to_pos (p : Lexing.position) : pos = { lnum = p.pos_lnum; bol = p.pos_bol }*)
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
%token BREAK    // break
%token LET IN END // let ... in ... end
%token UMINUS  // unary ops


%left LOR
%left LAND
%nonassoc EQEQ NEQ LT GT GEQ LEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start <unit> program

%%

program:
  | exp EOF {}

// 式
exp:
  | LPAREN exp RPAREN {} // 括弧で閉じる
  | INT {} // 数値
  | STR {} // 文字列値
  | UMINUS INT {} // マイナス
  | NIL {} // NIL
  | ID LPAREN separated_list(COMMA, exp) RPAREN {} // 関数適用
  | ID  {} // 変数
  | exp bin_op exp {}  // 二項演算
  // | MINUS exp %prec UMINUS {} // 単項マイナス
  | IF exp THEN exp ELSE exp {} // if-else
  | IF exp THEN exp {}  // if
  | WHILE exp DO exp {} // while
  | FOR ID COLONEQ exp TO exp DO exp {} // for
  | BREAK {} // break
  | LPAREN exp SEMICOLON exp RPAREN {} // ;連結
  | LET decs IN exp END     {} // let
  | type_dec {}
// 宣言
decs:
  | list(dec) {}

dec:
  | var_dec {}
  | func_dec {}

// 変数宣言
var_dec:
  | VAR left_value COLONEQ exp {}
  | VAR left_value COLON type_id COLONEQ exp {}

// 関数宣言
func_dec:
  | FUNCTION ID LPAREN type_field RPAREN EQ exp {}
  | FUNCTION ID LPAREN type_field RPAREN COLON type_id EQ exp {}

// 左辺値
left_value:
  | ID {}
  | left_value DOT ID {}
  | left_value LBRACKET exp RBRACKET {}

// 型の文法
// 型宣言
type_dec:
  | TYPE type_id EQ type_exp {}

// 型表現
type_exp:
  | type_id  {} // 型名
  | LBRACE type_fields RBRACE {} // フィールド型
  | LBRACKET type_exp RBRACKET {} //配列型

// 型名
type_id:
  | ID {}

// 型フィールド
type_fields:
  | separated_list(COMMA, type_field) {}

// 属性
type_field:
  | ID COLON type_exp {}

%inline bin_op:
  | PLUS {}
  | MINUS {}
  | TIMES {}
  | DIVIDE {}
  | LT {}
  | GT {}
  | LEQ {}
  | GEQ {}
  | LAND {}
  | LOR {}
  | EQEQ {}
  | NEQ {}

