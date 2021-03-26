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
%token EQ               // =
%token VAR TYPE FUNCTION    //  var type function
%token PLUS MINUS TIMES DIVIDE  // + - * /
%token EQEQ NEQ LT GT LEQ GEQ LAND LOR // == != < > <= >= & |
%token IF THEN ELSE  // if else
%token WHILE DO   // while do
%token FOR TO     // for to
%token BREAK    // break
%token OF   // of
%token LET IN END // let ... in ... end
%token UMINUS  // unary ops
%token RECORD // record


%nonassoc DO THEN
%nonassoc ELSE OF
%nonassoc EQEQ NEQ
%nonassoc GEQ LEQ LT GT
%left SEMICOLON
%left LOR LAND
%left PLUS MINUS
%left TIMES DIVIDE
%left UMINUS

%start <unit> program

%%

program:
  | exp EOF {}

// 式
exp:
  | value {}
  | ID  {} // 変数
  | LPAREN exp RPAREN {} // 括弧で閉じる
  | ID LPAREN separated_list(COMMA, exp) RPAREN {} // 関数適用
  | bin_op_exp {}  // 二項演算
  // | exp bin_op exp {}  // 二項演算
  | MINUS exp %prec UMINUS {} // 単項マイナス
  | IF exp THEN exp ELSE exp {} // if-else
  | IF exp THEN exp {}  // if
  | WHILE exp DO exp {} // while 1 do ...
  | FOR ID EQ exp TO exp DO exp {} // for x = 1 to 10 do ...
  | BREAK {} // break
  | exp SEMICOLON exp {} // a = 10; a + 1
  | LET decs IN exp END     {} // let var x = 1 in ... end

// values
value:
  | INT {} // 数値
  | STR {} // 文字列値
  | NIL {} // NIL
  | type_exp LBRACKET exp RBRACKET OF exp {} // int[8] of 0
  | LBRACE fields RBRACE {} // {x : 10} record

// フィールド
fields:
  | separated_list(COMMA, field) {}

field:
  | ID EQ exp {}

// 型表現
type_exp:
  | type_id {} // 型名
  | RECORD LBRACE type_fields RBRACE {} // record型
  | LBRACKET type_exp RBRACKET {} //配列型

// 型フィールド
type_fields:
  | separated_list(COMMA, type_field) {}

// 属性
type_field:
  | ID COLON type_exp {}

// 宣言
decs:
  | list(dec) {}

dec:
  | var_dec {}
  | func_dec {}
  | type_dec {}

// 変数宣言
var_dec:
  | VAR left_value EQ exp {}
  | VAR left_value COLON type_exp EQ exp {}

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

// 型名
type_id:
  | ID {}

// 二項演算
bin_op_exp:
  | exp PLUS   exp {}
  | exp MINUS  exp {}
  | exp TIMES  exp {}
  | exp DIVIDE exp {}
  | exp LT     exp {}
  | exp GT     exp {}
  | exp LEQ    exp {}
  | exp GEQ    exp {}
  | exp LAND   exp {}
  | exp LOR    exp {}
  | exp EQEQ   exp {}
  | exp NEQ    exp {}

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
