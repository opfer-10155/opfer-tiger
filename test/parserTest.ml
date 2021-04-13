open Syntax.Lib
open OUnit2
open Semantic.Ast
open Semantic.Position

let test_parser name expected str =
  name >:: fun _ ->
    assert_equal expected (parse_from_string str)
    ~cmp:equal_exp ~printer:show_exp

let read_file file =
  let ic = open_in file in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let sym_x = ("x" , 1)
let sym_y = ("y" , 1)
let sym_z = ("z" , 1)
let sym_f = ("f" , 1)

let x = VarExp{var=SimpleVar{id=sym_x; pos=dummy_pos}; pos=dummy_pos}
let y = VarExp{var=SimpleVar{id=sym_y; pos=dummy_pos}; pos=dummy_pos}
let z = VarExp{var=SimpleVar{id=sym_z; pos=dummy_pos}; pos=dummy_pos}
let binOp op x y = OpExp{op=op; l=x; r=y; pos=dummy_pos}
let int0 = IntExp{i=0; pos=dummy_pos}
let int1 = IntExp{i=1; pos=dummy_pos}
let varx = SimpleVar{id=sym_x; pos=dummy_pos}
let negative1 = binOp MinusOp int0 int1

let simple_test =
"simple_test"
>::: [
  test_parser "variable" (x) "x";
  test_parser "(exp)" (x) "(x)";
  test_parser "application" (CallExp{id=sym_f; args=[x; y]; pos=dummy_pos }) "func(x, y)";
  test_parser "bin_op" (binOp PlusOp int1 x) "1 + x";
  test_parser "unary" negative1 "-1";
  test_parser "if-else" (IfExp{c=x; t1=y; t2=Some z; pos=dummy_pos}) "if x then y else z";
  test_parser "if-then" (IfExp{c=x; t1=y; t2=None; pos=dummy_pos}) "if x then y";
  test_parser "while" (WhileExp{c=x; body=x; pos=dummy_pos}) "while x do x";
  test_parser "for" (ForExp{id=sym_f; low=int0; high=int1; body=x; pos=dummy_pos}) "for x = 0 to 1 do x";
  test_parser "break" (BreakExp{pos=dummy_pos}) "break";
  test_parser "sequence" (SeqExp{l=x; r=y}) "x; y";
  test_parser "let" (LetExp{
    decs=VarDec{id=sym_x; ty=None; e=int0; pos=dummy_pos};
    body=x;
    pos=dummy_pos
  }) "let var x = 0 in x end";
]

(* exp:
  | value {}
  | type_dec {} *)

let multi_test =
  "multi_test"
  >::: [
    test_parser "unary minus" (binOp PlusOp negative1 int0) "-1 + 0";
    test_parser "if-conflict" (IfExp{c=x; t1=IfExp{c=y; t1=z; t2=Some int1; pos=dummy_pos}; t2=None; pos=dummy_pos}) "if x then if y then z else 1";
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

