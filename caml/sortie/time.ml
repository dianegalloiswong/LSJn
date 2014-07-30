
let temps_total = ref 0.
let start_actuel = ref 0.
let appels = ref 0 (* à prouvable *)

let echoues = ref 0
let non_traites = ref 0

let toplevel = ref true



(* pour ne pas faire les problèmes plus difficiles que ceux déjà échoués *)
let courant = ref ("",0)
let trop_longs = ref []
let parse nom =
  let s1,s2 =
    let len = String.length nom in
    let rec separe_plus i =
      if i = len then
	raise Exit
      else if nom.[i]='+' then
	String.sub nom 0 i, String.sub nom (i+1) (len-i-1)
      else separe_plus (i+1)
    in separe_plus 0
  in
  if s2 = "1.p" then
    (s1,0)
  else
    try (s1,int_of_string (String.sub s2 2 3)) with _ -> assert false
let rec mem_inf (s,n) = function
  | [] -> false
  | (s',i)::_ when s'=s && i<=n -> true
  | _::t -> mem_inf (s,n) t
let faire_fichier nom =
  if Options.stop_on() then
    try
      let descr = parse nom in
      if mem_inf descr !trop_longs then (incr non_traites; false)
      else (courant:=descr; true)
    with Exit -> true
  else true





exception Timeout


let sigalrm_handler = Sys.Signal_handle (fun _ -> raise Timeout)
let timeout t_max f x =
  let t_max = (int_of_float t_max)+1 in
  let old_behavior = Sys.signal Sys.sigalrm sigalrm_handler in
  let reset_sigalrm () = Sys.set_signal Sys.sigalrm old_behavior 
  in ignore (Unix.alarm t_max) ;
  try  let res = f x in reset_sigalrm () ; res  
  with exc -> reset_sigalrm () ; raise exc
(*http://www.pps.univ-paris-diderot.fr/Livres/ora/DA-OCAML/book-ora169.html*)



let time f x = if Options.time_on() then 
  begin
  let toplevel_loc = !toplevel in toplevel:=false;
  let start = Unix.gettimeofday () in
  start_actuel := start;
  appels := 0;
  try
    let res = if Options.stop_on() then timeout !Options.temps_max f x else f x in
    let stop = Unix.gettimeofday () in
    let temps = (stop -. start) in
    if toplevel_loc then temps_total := !temps_total +. temps;
    if Options.affiche_temps_un() then
      begin
      if toplevel_loc && !appels > 0 then
	Format.printf "%fs, %d appels à prouvable@." temps !appels
      else
	Format.printf "%fs@." temps
      end;
    toplevel:=toplevel_loc;
    res
  with Timeout -> 
    let stop = Unix.gettimeofday () in
    let temps = (stop -. start) in
    Format.printf "timeout (temps mis : %fs, max. autorise : %fs)" temps !Options.temps_max;
    if !appels > 0 then Format.printf ", %d appels à prouvable vus" !appels;
    Format.printf "@.";
    if toplevel_loc then (incr echoues;
			  trop_longs := !courant :: !trop_longs);
    toplevel:=toplevel_loc;
    raise Timeout
  end
else f x

let verif_timeout () =
  incr appels;
  if Options.stop_on() && (Unix.gettimeofday () -. !start_actuel > !Options.temps_max) then
    raise Timeout
