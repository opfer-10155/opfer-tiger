open Parser
open ParserWithErrorMsg

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


let parse_from_string str =
  let lexbuf = Lexing.from_string str in
  Parser.program Lexer.token lexbuf

(*
* parse_from_file
* エラーメッセージを出すために動作を2stepに分ける
* step1:
*   通常のパーサーで動作させる
*   正常に終了して構文木を返せば、そのまま終了する
*   字句解析器がエラーを吐くと、そのメッセージを出力し異常終了する
*   構文解析器がエラーを吐いた場合、step2へ移行する。
* step2:
*   通常のパーサーから意味動作を排除(すべてunitに変換)したものでもう一度動作させる
*   今度は現在の状態をトラッキングしながらパースしていると思われる。(step1でそうしなかったのは時間がかかるから？)
*   エラーが発生した状態に応じて.messageファイルにあるメッセージを吐いて終了。
*)
let parse_from_file filename =
    match step1 filename with
    | (Some(exp) , _) -> exp
    | (_ , raw_text) -> step2 filename raw_text

