open Syntax.Lib
open OUnit2

let test_parser name expected str =
  name >:: fun _ ->
    assert_equal expected (parse_from_string str)

let read_file file =
  let ic = open_in file in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let simple_test =
"simple_test"
>::: [
  test_parser "variable" () "x";
  test_parser "(exp)" () "(x)";
  test_parser "application" () "func(arg1, arg2)";
  test_parser "bin_op" () "1 + x";
  test_parser "unary" () "-1";
  test_parser "if-else" () "if x then y else z";
  test_parser "if-then" () "if x then y";
  test_parser "while" () "while x do x";
  test_parser "for" () "for i = 1 to 10 do print(i)";
  test_parser "break" () "break";
  test_parser "sequence" () "print(1); print(x)";
  test_parser "let" () "let var x = 0 in print(x) end";
]

(* exp:
  | value {}
  | type_dec {} *)

let multi_test =
  "multi_test"
  >::: [
    test_parser "unary minus" () "-1 + 2";
    test_parser "if-conflict" () "if x1 then if x1 then y1 else z1";
  ]

(* let sample_program_test =
  let prg = read_file "./tiger_sample/sample.tiger" in
  "sample_program_test"
  >::: [
    test_parser "sample_program" () prg;
  ] *)


let () =
  run_test_tt_main simple_test;
  run_test_tt_main multi_test;

