let lc = 20 (* largeur colonne, doit être pair *)
let lc_nom = 40

let ff = ref Format.std_formatter
let file_results = "results.txt"

let categories = [|
  [| "nom de la formule \\ prouveur utilise"; ""; ""; ""; ""; ""; |];
  [| "simple";
     "temps total";
     "";
     "";
     "";
     "nombre d'appels";
  |];
  [| "caml compile";
     "temps total";
     "compil. vers caml";
     "compil. par ocamlc";
     "exec. .out";
     "nombre d'appels";
  |];
  [| "trees";
     "temps total";
     "compil. vers trees";
     "";
     "exec. trees";
     "nombre d'appels";
  |];
  [| "trees machine";
     "temps total";
     "compil. vers trees";
     "compil. vers machine";
     "exec. machine";
     "";
  |];
|]


let esp n = if n<0 then Format.eprintf "Test_all.esp : String.make %d@." n;String.make n ' '

let resize s = s^(esp (lc-(String.length s)))
let resize_nom s = s^(esp (lc_nom-(String.length s)))

let pos_point s =
  let rec aux i =
    if i = String.length s then -1
    else if s.[i] = '.' then i
    else aux (i+1)
  in aux 0
let resize_float s =
let res =
  let len = String.length s in
  let i = pos_point s in
  if i = -1 then
    (esp (lc/2-len))^s^(esp (lc/2))
  else
    (esp (lc/2-i))^s^(esp (lc/2-(len-i)))
in
if String.length res <> lc then
  Format.eprintf "Test_all.resize_float: \"%s\" donne \"%s\" de longeur %d@." s res (String.length res);
(*res.[0]<-'d';res.[lc-1]<-'f';*)
res


let print_categories () =
  for j=0 to 5 do
    Format.fprintf !ff "%s" (resize_nom categories.(0).(j));
    for i=1 to 4 do
      Format.fprintf !ff "%s" (resize categories.(i).(j));
    done;
    Format.fprintf !ff "@.";
  done;
  Format.fprintf !ff "@."




let fd_ref = ref None
let init () =
  let fd = Unix.openfile file_results [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_TRUNC] 0o640 in
  let out = Unix.out_channel_of_descr fd in
  ff := Format.formatter_of_out_channel out;
  fd_ref := Some fd;
  print_categories ()
let close () =
  match !fd_ref with None -> assert false | Some fd ->
    Unix.close fd





let tab = Array.make 4 [||]
let () = for i=0 to 3 do tab.(i) <- Array.make 5 "" done
let nom = ref ""

let print_res_formule () =
  for j=0 to 4 do
    if j=0 then Format.fprintf !ff "%s" (resize_nom !nom)
    else Format.fprintf !ff "%s" (esp lc_nom);
    Format.fprintf !ff "%!";
    for i=0 to 3 do
      Format.fprintf !ff "%s" (resize_float tab.(i).(j));
      Format.fprintf !ff "%!";
    done;
    Format.fprintf !ff "@.";
  done;
  Format.fprintf !ff "@."


let case = ref (0,0)
let prochaine = function
  | 3,0 -> 0,0
  | i,0 -> i,4
  | i,4 -> i+1,1
  | i,3 -> i,0
  | 2,1 -> 2,3
  | i,j -> i,j+1

let remplir s =
  let i,j = !case in
  tab.(i).(j) <- s;
  case := prochaine (i,j)
let remplir_float x =
  remplir (Format.sprintf "%f" x)
let remplir_int n =
  remplir (Format.sprintf "%d" n)

(*
let () = print_categories ()

let () =
  nom := "blou_formule";
  for i=0 to 14 do remplir ("blou."^(string_of_int i)) done;
  print_tab ()
*)
