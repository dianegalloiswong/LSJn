
{

  open Lexing
  open Parser_bin
  
  exception Lexing_error of char

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}



let chiffre = ['0'-'9']
let entier = chiffre+
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha | '_') (alpha | chiffre | '_')*


let space = [' ' '\t']

rule debut = parse
  | '\"'    { token lexbuf }
  | '\n'    { newline lexbuf; debut lexbuf }
  | _       { debut lexbuf }

and fin = parse
  | eof     { EOF }
  | '\n'    { newline lexbuf; fin lexbuf }
  | _       { fin lexbuf }

and token = parse
(*  | '\"'    { fin lexbuf }*)
  | '\n'    { newline lexbuf; token lexbuf }
  | space+  { token lexbuf }

  | "LDA"     { LDA }
  | "LDC"     { LDC }
  | "LDN"     { LDN }
  | "LDV"     { LDV }
  | "SAVE"     { SAVE }
  | "STV"     { STV }
  | "RST"     { RST }
  | "UJP"     { UJP }
  | "SKIP"     { SKIP }
  | "PAIR"     { PAIR }
  | "SPLIT"     { SPLIT }
  | "SWAP"     { SWAP }
  | "EQN"     { ISNULL }
  | "EQI"     { ISINT }
  | "SUCC"     { SUCC }
  | "PRED"     { PRED }
  | "LESS"     { LEQ }
  | "HALT"     { HALT }

  | ';'     { SEMICOLON }

  | entier as s  { INT (int_of_string s) }

  | eof     { EOF }
  | _ as c  { raise (Lexing_error c) }





