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


let main prog =
  let t = try Interp_trees.main prog 
    with Interp_trees.Error (s,pos,treeopt) ->
      loc2 (snd prog) pos;
      Format.eprintf "%s@." s;
      (match treeopt with
	| None -> ()
	| Some t -> Format.printf "(evaluated as "; Print.tree t; Format.printf ")@."
      );
      raise Exit
  in
  t
