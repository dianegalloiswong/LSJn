let localisation nom pos =
  let l = pos.Lexing.pos_lnum in
  let c = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  Format.eprintf "File \"%s\", line %d, characters %d-%d:@." nom l c (c+1)


let analyser nom =
  let f = open_in nom in 
  let buf = Lexing.from_channel f in
  try
    let prog = Parser_bin.prog Lexer_bin.token buf in
    close_in f;
    prog
  with
    | Lexer_bin.Lexing_error c -> 
      localisation nom (Lexing.lexeme_start_p buf);
      Format.eprintf "Erreur dans l'analyse lexicale: %c@." c;
      raise Exit
    | Parser_bin.Error -> 
      localisation nom (Lexing.lexeme_start_p buf);
      Format.eprintf "Erreur dans l'analyse syntaxique@.";
      raise Exit
    | exn ->
      Format.eprintf "Erreur dans Exec_bin.analyser@.";
      raise exn


let (*main*) () =
  let prog = analyser Path.bin in
  Machine_abstraite.main prog
