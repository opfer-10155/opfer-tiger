open Position
open Type
exception Error of string

let error (loc : Lexing.position * Lexing.position) fmt =
  let (startp, endp) = loc in
  let lnum = startp.pos_lnum in
  let start_pos = startp.pos_cnum - startp.pos_bol + 1 in
  let end_pos_sub = endp.pos_cnum - endp.pos_bol + 1 in
  let chars_len = end_pos_sub - start_pos in
  let end_pos = 
    if chars_len = 0
      then end_pos_sub
      else end_pos_sub - 1
  in
  let posinfo =
    if chars_len = 1
      then Format.sprintf "line %d, character %d: " lnum start_pos
      else Format.sprintf "line %d, characters %d-%d: " lnum start_pos end_pos
  in
  Format.ksprintf (fun msg -> raise (Error (posinfo ^ msg))) fmt

(* 例外 *)
let type_mismatch pos exp expected found =
  (* This expression has type exp but an expression was expected of type Type.ty *)
  error pos "type mismatch: The expression %s type is expected %s, but found %s"
    (Ast.show_exp exp) (show_ty expected) (show_ty found)

let undefined pos kind id =
  error pos "Undefined %s: %s" (kind) (Symbol.name id)

let not_record_argument pos parent id =
  error pos "%s is not a member of record %s"
    (Symbol.name id) (Symbol.name (Ast.var_toSymbol parent))
let not_record pos parent ty =
  error pos "%s is not Record type but it is %s type"
    (Symbol.name (Ast.var_toSymbol parent)) (show_ty ty)

let not_array pos var ty =
  error pos "%s is not Array type but it is %s type"
    (Symbol.name (Ast.var_toSymbol var)) (show_ty ty)

let not_a_function pos id ty =
  error pos "%s is not a function but its type is %s"
    (Symbol.name id) (show_ty ty)

let circulation_found pos id =
  error pos "Circulation found at type %s" (Symbol.name id)

(* let misdefined loc kind id =
  Error.error loc "%s is not a %s" (S.name id) kind

let cannot_be_nil loc id =
  Error.error loc "cannot initialize untyped variable %s with nil" (S.name id)

let break_is_not_in_loop loc =
  Error.error loc "breal exp isn't in loop exp" *)



