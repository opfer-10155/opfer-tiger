open Syntax
open Syntax.Lib
open OUnit2
open Core.Pos

let lexer_test name expected input =
  name >:: fun _ ->
    assert_equal expected (lex_from_string input)



