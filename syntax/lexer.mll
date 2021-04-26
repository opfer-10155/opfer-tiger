{
  open Error
  type token = [%import: Parser.token] [@@deriving show, eq]

  let append_char str ch =
    str ^ (String.make 1 (Char.chr ch))

  let str_incr_linenum str lexbuf =
    String.iter (function '\n' -> Lexing.new_line lexbuf | _ -> ()) str

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let id = alpha+ (alpha | digit | '_')*
let num = '0' | ['1'-'9'] digit*
let ws = ['\t' ' ' '\n']
let space = [' ' '\t']
let newline = ['\n' '\r']

rule token = parse
  | space+    { token lexbuf }
  | newline   { Lexing.new_line lexbuf; token lexbuf }
  | "nil"     { NIL }
  | num as n  { INT (int_of_string n) }
  | '"'       { string (Buffer.create 0) lexbuf }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "while"   { WHILE }
  | "do"      { DO }
  | "for"     { FOR }
  | "to"      { TO }
  | "break"   { BREAK }
  | "let"     { LET }
  | "in"      { IN }
  | "end"     { END }
  | "var"     { VAR }
  | "function" { FUNCTION }
  | "type"    { TYPE }
  | "of"      { OF }
  | ":="      { COLONEQ }
  | "["       { LBRACKET }
  | "]"       { RBRACKET }
  | "+"       { PLUS }
  | "-"       { MINUS }
  | "*"       { TIMES }
  | "/"       { DIVIDE }
  | "="       { EQ }
  | "=="      { EQEQ }
  | "!="      { NEQ }
  | "<"       { LT }
  | ">"       { GT }
  | "<="      { LEQ }
  | ">="      { GEQ }
  | "&"       { LAND }
  | "|"       { LOR }
  | "("       { LPAREN }
  | ")"       { RPAREN }
  | "{"       { LBRACE }
  | "}"       { RBRACE }
  | ","       { COMMA }
  | ":"       { COLON }
  | ";"       { SEMICOLON }
  | '.'       { DOT }
  | "record"  { RECORD }
  | "and"     { AND }
  | "array"   { ARRAY }
  | id as i   { ID (i) }
  | eof       { EOF }
  | _ as s    {illegal_character (lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p) s }
  (* | "/*" *)

and string buf = parse
  | '"'       { STR (Buffer.contents buf) }
  | _ as s { Buffer.add_char buf s; string buf lexbuf }
  
