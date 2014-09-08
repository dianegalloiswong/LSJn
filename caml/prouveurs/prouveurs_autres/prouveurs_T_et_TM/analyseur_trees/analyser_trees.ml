let localisation nom pos =
  let l = pos.Lexing.pos_lnum in
  let c = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  Format.eprintf "File \"%s\", line %d, characters %d-%d:@." nom l c (c+1)


let main nom =
  let f = open_in nom in 
  let buf = Lexing.from_channel f in
  try
    let prog0 = Parser_trees.prog0 Lexer_trees.token buf in
    close_in f;
    prog0,nom
  with
    | Lexer_trees.Lexing_error c -> 
      localisation nom (Lexing.lexeme_start_p buf);
      Format.eprintf "Erreur dans l'analyse lexicale: %c@." c;
      raise Exit
    | Parser_trees.Error -> 
      localisation nom (Lexing.lexeme_start_p buf);
      Format.eprintf "Erreur dans l'analyse syntaxique@.";
      raise Exit
    | exn ->
      Format.eprintf "Erreur dans Analyser_trees.main@.";
      raise exn

