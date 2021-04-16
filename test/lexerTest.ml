(* 字句解析器のテスト *)
open Syntax.Lib
open Syntax.Lexer
open OUnit2
open List

let equal_tokens_list l1 l2 =
  if length l1 != length l2 then false
  else fold_left (fun b -> fun (t1 , t2) -> b && (equal_token t1 t2))
                  true (combine l1 l2)

let show_tokens_list tokens = String.concat ", " (map show_token tokens)

(* test for single token *)
let test_single_token name (expected : token) (input : string) =
  name >:: fun _ ->
    assert_equal [expected ; EOF] (lex_from_string input)
                ~cmp:equal_tokens_list ~printer:show_tokens_list


(* test for tokens list *)
let test_tokens_list name expected input =
  name >:: fun _ ->
    assert_equal expected (lex_from_string input)
                ~cmp:equal_tokens_list ~printer:show_tokens_list


let simple_test =
  "simple_test"
  >::: [
        test_single_token "variable" (ID "a") "a";
        test_single_token "NIL" NIL "nil";
        test_single_token "string" (STR "hoge") "\"hoge\"";
        test_single_token "while" WHILE "while";
        test_single_token "for" FOR "for";
        test_single_token "to" TO "to";
        test_single_token "break" BREAK "break";
        test_single_token "let" LET "let";
        test_single_token "in" IN "in";
        test_single_token "end" END "end";
        test_single_token "function" FUNCTION "function";
        test_single_token "var" VAR "var";
        test_single_token "type" TYPE "type";
        (* test_single_token "array" ARRAY "array"; *)
        test_single_token "if" IF "if";
        test_single_token "then" THEN "then";
        test_single_token "else" ELSE "else";
        test_single_token "do" DO "do";
        test_single_token "of" OF "of";
        test_single_token "of" NIL "nil";
        test_single_token "," COMMA ",";
        test_single_token ":" COLON ":";
        test_single_token ";" SEMICOLON ";";
        test_single_token "(" LPAREN "(";
        test_single_token ")" RPAREN ")";
        test_single_token "[" LBRACKET "[";
        test_single_token "]" RBRACKET "]";
        test_single_token "{" LBRACE "{";
        test_single_token "}" RBRACE "}";
        test_single_token "." DOT ".";
        test_single_token "+" PLUS "+";
        test_single_token "-" MINUS "-";
        test_single_token "*" TIMES "*";
        test_single_token "/" DIVIDE "/";
        test_single_token "==" EQEQ "==";
        test_single_token "!=" NEQ "!=";
        test_single_token "<" LT "<";
        test_single_token "<=" LEQ "<=";
        test_single_token ">" GT ">";
        test_single_token ">=" GEQ ">=";
        test_single_token "&" LAND "&";
        test_single_token "|" LOR "|";
        (* test_single_token ":=" COLONEQ ":="; *)
        test_single_token "=" EQ "=";
  ]
(*
TODO : comment test "/* x */ 1 /* x */" to be failed
*)

let strict_test =
  "strict_test"
  >:::[
    test_single_token "ID or IF ?" (ID "ifs") "ifs";
    test_single_token "number is correct?" (INT 0) "0";
    test_single_token "number is correct?" (INT 1) "1";
  ]


let () =
  run_test_tt_main simple_test;
  run_test_tt_main strict_test;