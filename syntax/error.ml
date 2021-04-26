(*
 * Lexing bufferを受け取り、状況に応じたエラーメッセージを出力する
 * buf : Lexing.lexbuf ocamllexから生成されるlexing buffer
 * proc : string       エラーを起こした処理の名称 (Lexer, Parser)
 * error : string      エラー内容
 *)
let gen_error_message (buf : Lexing.lexbuf) (proc : string) (error : string) =
  let pos = buf.lex_curr_p in
  let lnum = pos.pos_lnum in
  let start = buf.lex_start_pos in
  let curr = buf.lex_curr_pos in
  let last_char =
    (* 仮の条件式（lexbuf.lex_eof_reachedが機能しない） *)
    if (start == curr) then
      "eof"
    else (Lexing.lexeme buf)
  in
  let message = Printf.sprintf
    "line %d, characters %d-%d: '%s' %s - %s"
    (lnum)          (* 処理に失敗した行番号 *)
    (start)         (* 処理に失敗した文字の開始位置 *)
    (curr)          (* 処理に失敗した文字位置 *)
    (last_char)     (* 処理に失敗した文字 *)
    (proc)          (* 処理に失敗した処理名 *)
    (error)         (* 処理の失敗の理由 *)
  in
    message

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

let fatal fmt =
  Format.ksprintf failwith fmt

let illegal_character (lexbuf: Lexing.lexbuf) char =
  error (lexbuf.lex_start_p, lexbuf.lex_curr_p)  "illegal character '%c'" char

let unterminated_string  (lexbuf: Lexing.lexbuf) =
  error (lexbuf.lex_start_p, lexbuf.lex_curr_p) "unterminated string"

let illegal_escape  (lexbuf: Lexing.lexbuf) sequence =
  error (lexbuf.lex_start_p, lexbuf.lex_curr_p) "illegal escape sequence: %s" sequence


