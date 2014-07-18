open Ast_trees



let compile () =
  let f = Quelques_formules.f8 in
  Compile.main f

let exec_code () =
  let t = Exec_trees.main Compile.code in
  Print.tree t;
  Format.printf "@."


let ref_compile = ref false
let ref_exec_code = ref false


let options = [
  "-compile", Arg.Set ref_compile, ": génère code.ml";
  "-exec-code", Arg.Set ref_exec_code, ": exécute code.ml";
]

let () = Arg.parse options (fun _ -> ()) ""

let () = if not (!ref_compile || !ref_exec_code) then (ref_compile:=true; ref_exec_code:=true)

let () = 
  if !ref_compile then compile ();
  if !ref_exec_code then exec_code ()
