
{
  open Lexing
  open Parser_ILTP
  
  exception Lexing_error of char

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}



let chiffre = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha | '_') (alpha | chiffre | '_')*


let space = [' ' '\t']




rule comment = parse
  | '\n' { newline lexbuf ; token lexbuf }
  | _    { comment lexbuf }
  | eof  { EOF }


and token = parse
  | '\n'    { newline lexbuf; token lexbuf }
  | space+  { token lexbuf }
  | "fof"  { FOF }
  | "conjecture"  { CONJ }
  | "axiom"  { AX }
  | ident as id  { IDENT id }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '~'     { NON }
  | '|'     { OU }
  | '&'     { ET }
  | "=>"    { IMP }
  | "<=>"   { EQ }
  | "$false"  { FAUX }
  | "$true"  { VRAI }
  | '.'     { POINT }
  | ','     { VIR }

  | "% Status   : Theorem"              { Th }
  | "% Status (intuit.) : Theorem"      { ITh }
  | "% Status (intuit.) : Non-Theorem"  { INTh }

  | '%'    { comment lexbuf }
  | eof     { EOF }
  | _ as c  { raise (Lexing_error c) }

