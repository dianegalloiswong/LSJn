open Path
(*
let compile_et_execute = "ocamlc -w -26 -o "^code_caml_executable^" "^code_caml
  ^"; ./"^code_caml_executable^" > "^code_caml_sortie^" 2> "^code_caml_errors
*)
let sys_compile() = "ocamlc -w -26 -o "^code_caml_executable()^" "^code_caml()
let sys_exec() = "./"^code_caml_executable()^" > "^code_caml_sortie()(*^" 2> "^code_caml_errors*)

let main prog =
  Format.printf "compilation trees vers caml : %!";
  Time.time Compile_trees_vers_caml.main prog;
  Format.printf "compilation par ocamlc : %!";
  ignore (Time.time Unix.system (sys_compile()));
  Format.printf "exécution : %!";
  ignore (Time.time Unix.system (sys_exec()));
  let fd = Unix.openfile (code_caml_sortie()) [Unix.O_RDONLY] 0o640 in
  let buff = "a" in
  ignore (Unix.read fd buff 0 1);
  Unix.close fd;
  if buff="1" then true else if buff="0" then false else
  if buff="E" then (Format.eprintf "erreur : le résultat de l'exécution de %s n'est pas un booléen@." (code_caml_executable()); raise Exit)
  else (Format.eprintf "erreur dans exec_trees_via_ocamlc.ml@."; raise Exit)
