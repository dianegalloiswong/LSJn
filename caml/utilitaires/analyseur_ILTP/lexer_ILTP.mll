
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



(*

(* Analyseur lexical pour Mini C++ *)



{
  open Lexing
  open Parser
  
  exception Lexing_error of char
  exception CommentaireNonTermine


  let kwd_tbl = ["class", CLASS ; "else", ELSE ; "false", FALSE ;"for", FOR; "if", IF; "int", INT;
		"new", NEW; "NULL", NULL; "public", PUBLIC; "return", RETURN; "this", THIS; "true", TRUE ;
		"virtual", VIRTUAL; "void", VOID; "while", WHILE]

  
  let () = List.iter (fun (s,t)-> Keywords.add_tbl s t) kwd_tbl 
  
  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
  
  let rec oct_to_dec s = 
    let n = String.length s in
    if n = 0 then 0 else
    8*(oct_to_dec (String.sub s 0 (n-1))) + (int_of_string (String.sub s (n-1) 1))
   
  let int_of_string2 s = 
    let n = String.length s in
    if ((String.length s) < 2) then int_of_string s
    else if (s.[0] = '0')&&(s.[1] <> 'x') then oct_to_dec (String.sub s 1 (n-1))
    else int_of_string s

    	
let hexachar a b =                                     
  let hex c = match int_of_char c with
    |n when (n>47)&&(n<58) -> n-48
    |n when (n>64)&&(n<71) -> n-55
    |n when (n>96)&&(n<103)-> n-87
    |_-> assert false
  in
char_of_int ((16*hex a)+ hex b)

let rec backslash i n s =
  if (i>(n-2)) then s
  else if (String.sub s i 2 = "\\\\") then let s1 = (String.sub s 0
  i)^(String.sub s (i+1) (n-(1+i))) in backslash (i+1) (n-1) s1
  else backslash (i+1) n s 

let rec hexa i n s = 
  if (i>(n-4)) then s
   else if String.sub s i 2 <> "\\x" then hexa (i+2) n s 
   else let c = (hexachar s.[i+2] s.[i+3]) in
	let s1 = (String.sub s 0 (i+1))^(String.sub s (i+4) (n-(i+4)) ) in
	s1.[i]<-c;
	hexa (i+1) (n-3) s1


let rec format_string s =
  let s = hexa 0 (String.length s) s in backslash 0 (String.length s) s 


}



let chiffre = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha | '_') (alpha | chiffre | '_')*

let chiffre_octal = ['0'-'7']
let chiffre_hexa = ['0'-'9' 'a'-'f' 'A'-'F']
let entier = ( '0' | ['1'-'9'] chiffre* | '0' chiffre_octal+ | "0x" chiffre_hexa+ )
let caractere =
  (
    [' '-'!' '#'-'[' ']'-'\127']         (*32-33 35-91 92-127*)
  | ('\\' ('\\' | '\"' | 'n' | 't'))
  | ("\\x" chiffre_hexa chiffre_hexa)
  )
(*let chaine = '\"' caractere* '\"'*)

let space = [' ' '\t']




rule comment_etoile = parse
  | "*/" { token lexbuf }
  | '\n' { newline lexbuf ; comment_etoile lexbuf }
  | _    { comment_etoile lexbuf }
  | eof  { raise CommentaireNonTermine}


and comment_ligne = parse
  | '\n' { newline lexbuf ; token lexbuf }
  | _    { comment_ligne lexbuf }
  | eof  { EOF }


and token = parse
  | "#include <iostream>"  { IOSTREAM }
  | "std::cout"  { COUT }
  | "/*"    { comment_etoile lexbuf }
  | "//"    { comment_ligne lexbuf }
  | '\n'    { newline lexbuf; token lexbuf }
  | space+  { token lexbuf }
  | ident as id  { try Keywords.id_or_kwd id with Not_found -> IDENT id }
  | '\"' (caractere* as s) '\"'  { CHAINE (format_string s) }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { TIMES }
  | '/'     { DIV }
  | '%'     { MOD }
  | "=="    { EQTEST }
  | "++"    { INCR }
  | "--"    { DECR }
  | '='     { EQ }
  | "!="    { NEQ }
  | "<<"    { LTLT }
  | '<'     { LT }
  | "<="    { LE }
  | '>'     { GT }
  | ">="    { GE }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '{'     { LACCO }
  | '}'     { RACCO }
  | '.'     { DOT }
  | ','     { COMMA }
  | ':'     { COLON }
  | ";"     { SEMICOLON }
  | "&&"    { AND }
  | '&'     { ADDRESS }
  | "||"    { OR }
  | "->"    { ARROW }
  | '!'	    { NOT }
  | entier as s  { CST (int_of_string2 s) }
  | eof     { EOF }
  | _ as c  { raise (Lexing_error c) }

*)
