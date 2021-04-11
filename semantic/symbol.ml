(* 記号型 名前文字列と対応する番号のペア *)
type symbol = (string * int[@equal fun (a, _) (b, _) -> a = b])
[@@deriving show, eq]

(* 次の新しい記号に対して割り当てる番号 *)
let nextsym = ref 0

let size_hint = 128

(* 記号表 stringを{i mod size_hint}*)
let hashtable = Hashtbl.create size_hint

let name (s, _) = s

(* メッセージ用文字列を得る? *)
let string_of_symbol (sym : symbol) : string =
  match sym with
    (name, i) -> name ^ "$" ^ string_of_int i

(* 名前文字列を記号へ *)
let toSymbol (name : string) =
  (* try
    let i = Hashtbl.find hashtable name in
    (name, i)
  with Not_found ->
    let i = !nextsym in
    nextsym := i + 1;
    Hashtbl.add hashtable name i;
    (name, i) *)
  match (Hashtbl.find_opt hashtable name) with
    | Some i -> (name, i)
    | None ->
        let i = !nextsym in
        nextsym := i + 1;
        Hashtbl.add hashtable name i;
        (name, i)

