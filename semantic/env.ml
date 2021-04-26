open Symbol
module SymTab = SymTable
open Type

type enventry =
  | VarEntry of {
      ty : ty;
      const : bool;
      (* access: Trans.access *)
    }
  | FunEntry of {
      formals : ty list; 
      result : ty;
      (* label: Temp.label;
      level: Trans.level *)
    }

let standard_types = [ ("int", IntTy); ("string", StrTy) ]

let standard_functions =
  [
    ("printint", [ IntTy ], UnitTy);
    ("exit", [ IntTy ], UnitTy);
    ("getchar" , [], StrTy);
    ("chr" , [IntTy] , StrTy);
    ("ord" , [StrTy] , IntTy);
    ("print" , [StrTy] , UnitTy)
  ]

type venv = enventry SymTab.table

type tyenv = ty SymTab.table  (* 型名 -> 型 *)

type ctx = ty SymTab.table    (* 変数名 -> 型 *)


let makeFunEntry name params result =
  (name, FunEntry { formals = params; result })

let base_venv =
  List.fold_left
    (fun env (name, formals, result) ->
      let name, entry = makeFunEntry name formals result in
      SymTab.enter (toSymbol name) entry env)
    SymTab.empty standard_functions

let base_tenv =
  List.fold_left
    (fun env (name, t) -> SymTab.enter (toSymbol name) t env)
    SymTab.empty standard_types

let base_ctx =
  List.fold_left
    (fun ctx (name, formals, result) ->
      let funty = FuncTy (formals, result) in
      SymTab.enter (toSymbol name) funty ctx)
    SymTab.empty standard_functions
