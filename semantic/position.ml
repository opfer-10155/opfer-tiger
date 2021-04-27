(* type for a location in source code *)
type position = [%import: Lexing.position]
[@@deriving show]

let equal_position _ _ = true

(* (start_pos , end_pos) *)
type location = position * position
[@@deriving show]

let equal_location _ _ = true


(* alias *)
type pos = location
[@@deriving show, eq]

(* initializing *)

let dummy_loc =
  (Lexing.dummy_pos, Lexing.dummy_pos)

(* alias *)
let dummy_pos = dummy_loc

let curr_loc lexbuf =
  (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)

(* Printing *)

let pp_position ppf pos =
  Format.fprintf ppf "%s:%i.%i"
                 pos.pos_fname
                 pos.pos_lnum
                 (pos.pos_cnum - pos.pos_bol)

let pp_location ppf (left, right) =
  if left.pos_fname = right.pos_fname then
    Format.fprintf ppf "%s:%i.%i-%i.%i"
                   left.pos_fname
                   left.pos_lnum
                   (left.pos_cnum - left.pos_bol)
                   right.pos_lnum
                   (right.pos_cnum - right.pos_bol)
  else
    Format.fprintf ppf "%a-%a"
                   pp_position left
                   pp_position right
