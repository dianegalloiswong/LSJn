
{

  open Lexing
  open Parser
  
  exception Lexing_error of char

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

  let comment_depth = ref 0

}



let chiffre = ['0'-'9']
let entier = chiffre+
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha | '_') (alpha | chiffre | '_')*


let space = [' ' '\t']




rule comment = parse
  | "(*" { incr comment_depth; comment lexbuf }
  | "*)" { if !comment_depth=0 then token lexbuf else (decr comment_depth; comment lexbuf) }
  | '\n' { newline lexbuf ; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { EOF }


and token = parse
  | '\n'    { newline lexbuf; token lexbuf }
  | space+  { token lexbuf }
  | "(*"    { comment lexbuf }


  | "match"     { MATCH }
  | "with"     { WITH }
  | "=>"     { ARROW }
  | "let"     { LET }
  | "="     { EQ }
  | "in"     { IN }
  | "call"     { CALL }
  | "isnull"     { ISNULL }
  | "isint"     { ISINT }
  | "isnode"     { ISNODE }
  | "if"     { IF }
  | "then"     { THEN }
  | "else"     { ELSE }
 

  | '<'     { LT }
  | '>'     { GT }
  | "<="     { LEQ }
 (* | ">="     { GEQ } *)

  | '('     { LPAREN }
  | ')'     { RPAREN }
  | ','     { COMMA }

  | "&&"    { AND }
  | "+1"    { PLUSUN }

  | "null"    { NULL }
  | ident as id  { IDENT id }
  | entier as s  { INT (int_of_string s) }

  | eof     { EOF }
  | _ as c  { raise (Lexing_error c) }





