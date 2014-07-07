
let temps_total = ref 0.
let start_actuel = ref 0.
let appels = ref 0 (* à prouvable *)

exception Temps_ecoule


let time f x =
if Options.time_on() then
    let start = Unix.gettimeofday () in
    start_actuel := start;
    appels := 0;
 try
    let res = f x in
    let stop = Unix.gettimeofday () in
    let temps = (stop -. start) in
    temps_total := !temps_total +. temps;
    if Options.print_temps_un() then Format.printf "%fs, %d appels à prouvable@." temps !appels;
    res
 with Temps_ecoule -> Format.printf "temps écoulé (%fs), %d appels à prouvable vus@." !Options.temps_max !appels; raise Temps_ecoule
else f x



let verif_timeout () =
  incr appels;
  if Options.stop_on() && (Unix.gettimeofday () -. !start_actuel > !Options.temps_max) then
    raise Temps_ecoule





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
    let descr = parse nom in
    if mem_inf descr !trop_longs then false
    else (courant:=descr; true)
  else true

let verif_timeout () = 
  if Options.stop_on() then
    try verif_timeout () with Temps_ecoule ->
      trop_longs := !courant:: !trop_longs;
      raise Temps_ecoule
  else
    verif_timeout ()
