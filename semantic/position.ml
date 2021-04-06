open Lexing

type pos = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}
[@@deriving show]

let equal_pos _ _ = true

let to_pos (p:position) : pos = {
  pos_fname=p.pos_fname;
  pos_lnum=p.pos_lnum;
  pos_bol=p.pos_bol;
  pos_cnum=p.pos_cnum;
}

let dummy_pos = {
  pos_fname="dummy";
  pos_lnum=0;
  pos_bol=0;
  pos_cnum=0;
}
