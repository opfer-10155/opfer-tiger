(* 型検査器のテスト *)
open Syntax.Lib
open Semantic.Position
open Semantic.Symbol
open Semantic.Ast
open Semantic.Type
open Semantic.Tycheck
open Semantic.Env
open OUnit2
open List
open SymTab

let parse = parse_from_string

let rec equal_type t1 t2 =
  match (t1 , t2) with
  | (ArrTy (ty1, _) , ArrTy (ty2, _)) -> equal_type ty1 ty2
  | (RecTy (f1, _) , RecTy (f2, _)) ->
      fold_left
        (fun flag ((id1 , ty1) , (id2 , ty2)) -> flag && (equal_type ty1 ty2) && id1 == id2)
        true
        (List.combine f1 f2)
  | _ -> t1 == t2

(* 型検査器の単体テスト *)
let test_type name ty exp =
  name >:: fun _ ->
  assert_equal ty (tycheck base_tenv (enter (toSymbol "a") IntTy base_ctx) exp)
              ~cmp:equal_type ~printer: show_ty

(* parserが正常に機能しているという前提で、リテラルからテストできる *)
let test_type_from_literal name ty literal =
  name >:: fun _ -> assert_equal ty (tycheck base_tenv base_ctx (parse literal))
                    ~cmp:equal_type ~printer: show_ty


(* ASTエイリアス *)
let nil  = NilExp {pos=dummy_pos}
let zero = IntExp {i=0; pos=dummy_pos}
let one  = IntExp {i=1; pos=dummy_pos}
let str  = StrExp {s="a"; pos=dummy_pos}

let sym_x        = toSymbol "x"
let sym_a        = toSymbol "a"

let sym_int      = toSymbol "int"
let sym_printint = toSymbol "printint"

let var_x = SimpleVar {id=sym_x; pos=dummy_pos}
let var_a = SimpleVar {id=sym_a; pos=dummy_pos}

let x = VarExp {var=var_x; pos=dummy_pos}

let call_printint = CallExp{id=sym_printint; args=[zero]; pos=dummy_pos}

let int_ty  = NameTy{id=sym_int; pos=dummy_pos}
let x_field = Field {id=sym_x; e=zero; pos=dummy_pos}
let dec_x   = VarDec {id=sym_x; ty=None; e=zero; pos=dummy_pos}


(* テスト *)
let simple_test =
  "simple_test"
  >::: [
    test_type "Int"          IntTy                zero;
    test_type "String"       StrTy                str;
    test_type "Nil"          NilTy                nil;
    test_type "printint(0)"  UnitTy               call_printint;
    test_type "0 + 0 : Int"  IntTy                (OpExp{op=PlusOp; l=zero; r=zero; pos=dummy_pos});
    test_type "'a'; 0 : Int" IntTy                (SeqExp{l=str; r=zero});
    test_type "a :=0 : Unit" UnitTy               (AssignExp{var=var_a; e=zero; pos=dummy_pos});
    test_type "if c a a:Str" StrTy                (IfExp {c=zero; t1=str; t2=Some str; pos=dummy_pos});
    test_type "if c a: Unit" UnitTy               (IfExp {c=zero; t1=call_printint; t2=None; pos=dummy_pos});
    test_type "while : Unit" UnitTy               (WhileExp {c=zero; body=str; pos=dummy_pos});
    test_type "for : Unit"   UnitTy               (ForExp {id=sym_x; low=zero; high=one; body=x; pos=dummy_pos});
    test_type "break : Unit" UnitTy               (BreakExp {pos=dummy_pos});
    test_type "[0]: Arr Int" (ArrTy(IntTy , ref())) (ArrayExp{ty=int_ty; len=one; init=zero; pos=dummy_pos});
    test_type "{x=0}:Record" (RecTy( [ (sym_x , IntTy) ], ref() )) (RecordExp {fields=[x_field]; pos=dummy_pos});
    test_type "let x=0 in x" IntTy                (LetExp {decs=dec_x; body=x; pos=dummy_pos});
    test_type_from_literal  "type anotation" IntTy
      "let
        function hoge() = printint(0)
        type rec = {aa : {bb : int}}
        var x = 0
      in x end"
  ]

(* let strict_test =
  "strict_test"
  >:::[
  ] *)

  (* let test_should_failure *)

  (* mutual recursive test *)

let () =
  (* run_test_tt_main table_test; *)
  run_test_tt_main simple_test;
  (* run_test_tt_main strict_test; *)