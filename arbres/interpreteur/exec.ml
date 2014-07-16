
let localisation nom pos =
  let l = pos.Lexing.pos_lnum in
  let c = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
  Format.eprintf "File \"%s\", line %d, characters %d-%d:@." nom l (c-1) c

let parse nom =
  let f = open_in nom in 
  let buf = Lexing.from_channel f in
  try
    let prog = Parser.prog Lexer.token buf in
    close_in f;
    prog
  with
    | Lexer.Lexing_error c -> 
      localisation nom (Lexing.lexeme_start_p buf);
      Format.eprintf "Erreur dans l'analyse lexicale: %c@." c;
      raise Exit
    | Parser.Error -> 
      localisation nom (Lexing.lexeme_start_p buf);
      Format.eprintf "Erreur dans l'analyse syntaxique@.";
      raise Exit
    | _ ->
      Format.eprintf "Erreur dans Exec.parse@.";
      raise Exit




let main nom =
  let prog = parse nom in
  let t = Interp.interp_prog prog in
  Print.tree t;
  Format.printf "@."
