
let sys_compile() = "ocamlc -w -26 -o "^Path.code_caml_executable()^" "^Path.code_caml()
let sys_exec() = "./"^Path.code_caml_executable()^" > "^Path.code_caml_sortie()(*^" 2> "^code_caml_errors*)

let compile () =  
  if Options.affiche_temps_etapes() then Format.printf "compilation par ocamlc : %!";
  ignore (Time.time Unix.system (sys_compile()))

let exec () =
  if Options.affiche_temps_etapes() then Format.printf "exécution : %!";
  ignore (Time.time Unix.system (sys_exec()))

let lecture_sortie () =
  let fd = Unix.openfile (Path.code_caml_sortie()) [Unix.O_RDONLY] 0o640 in
  let buff = "a" in
  ignore (Unix.read fd buff 0 1);
  Unix.close fd;
  if buff="1" then true else if buff="0" then false else
  if buff="E" then (Format.eprintf "erreur : le résultat de l'exécution de %s n'est pas un booléen@." (Path.code_caml_executable()); raise Exit)
  else (Format.eprintf "erreur dans Use_ocamlc.lecture_sortie@."; raise Exit)

let main () =
  compile ();
  exec ();
  lecture_sortie ()
