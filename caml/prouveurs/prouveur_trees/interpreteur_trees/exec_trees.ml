
let localisation nom pos =
  let l = pos.Lexing.pos_lnum in
  let c = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  Format.eprintf "File \"%s\", line %d, characters %d-%d:@." nom l c (c+1)

let loc2 nom (pos1, pos2) = 
	let l1 = pos1.Lexing.pos_lnum in
	let l2 = pos2.Lexing.pos_lnum in
	let c1 = pos1.Lexing.pos_cnum - pos1.Lexing.pos_bol in
	let c2 = pos2.Lexing.pos_cnum - pos2.Lexing.pos_bol in
	if l1=l2 
	then Format.eprintf "File \"%s\", line %d, characters %d-%d:\n" nom l1 c1 c2
	else (localisation nom pos1; localisation nom pos2)

let parse nom =
  let f = open_in nom in 
  let buf = Lexing.from_channel f in
  try
    let prog = Parser_trees.prog Lexer_trees.token buf in
    close_in f;
    prog
  with
    | Lexer_trees.Lexing_error c -> 
      localisation nom (Lexing.lexeme_start_p buf);
      Format.eprintf "Erreur dans l'analyse lexicale: %c@." c;
      raise Exit
    | Parser_trees.Error -> 
      localisation nom (Lexing.lexeme_start_p buf);
      Format.eprintf "Erreur dans l'analyse syntaxique@.";
      raise Exit
    | _ ->
      Format.eprintf "Erreur dans Exec.parse@.";
      raise Exit




let main nom =
  let prog = parse nom in
  let t = try Interp_trees.interp_prog prog 
    with Interp_trees.Error (s,pos,treeopt) ->
      loc2 nom pos;
      Format.eprintf "%s@." s;
      (match treeopt with
	| None -> ()
	| Some t -> Format.printf "(evaluated as "; Print.tree t; Format.printf ")@."
      );
      raise Exit
  in
  t
