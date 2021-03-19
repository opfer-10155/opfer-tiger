open Lexer

(* convert tokens buffer to tokens list *)
let rec buf_to_list buf =
  match Lexer.token buf with
  | EOF -> [ EOF ]
  | tok -> tok :: buf_to_list buf


(* parse string into token list *)
let lex_from_string str =
  let lexbuf = Lexing.from_string str in
  buf_to_list lexbuf


let show_token_list (toks : token list) = String.concat ", " (List.map Lexer.show_token toks)
