(* main : prend (sous forme de string) une liste de fichiers ou répertoires, et retourne la liste des fichiers récursivement dedans dans l'ordre alphabétique *)



let rec files nom acc =
  match (Unix.stat nom).Unix.st_kind with
      | Unix.S_REG (*when (pointp nom)*) (*?*) -> 
	nom::acc
      | Unix.S_DIR -> 
	let dh = Unix.opendir nom in
	let rec loop acc =
	  try
	    let s = Unix.readdir dh in
	    let acc = if s.[0] <> '.' then files (nom^"/"^s) acc else acc in
	    loop acc
	  with End_of_file -> acc
	in loop acc
      | _ -> acc

let main f_or_d_list =
  let file_list = List.fold_left (fun l nom -> files nom l) [] f_or_d_list in
  List.sort compare file_list
(*
let ltest = main ["Problems";"testsILTP";"README"]
let () = List.iter (fun s -> Format.printf "%s@." s) ltest
*)
