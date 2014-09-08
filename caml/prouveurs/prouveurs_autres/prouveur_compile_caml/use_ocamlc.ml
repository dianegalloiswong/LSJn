
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

  let buff = "aa" in
  ignore (Unix.read fd buff 0 2);

  let b =
    if buff="1\n" then true else if buff="0\n" then false else
    if buff="E\n" then (Format.eprintf "erreur : le résultat de l'exécution de %s n'est pas un booléen@." (Path.code_caml_executable()); raise Exit)
    else (Format.eprintf "erreur dans Use_ocamlc.lecture_sortie@."; raise Exit)
  in

  let buff = String.make 16 'a' in
  let rec read_until i =
    ignore (Unix.read fd buff i 1);
    if buff.[i] = '\n' then i
    else read_until (i+1)
  in
  let i = read_until 0 in
  let appels = int_of_string (String.sub buff 0 i) in
  Time.appels := appels;

  Unix.close fd;

  b


let main () =
  compile ();
  exec ();
  lecture_sortie ()
