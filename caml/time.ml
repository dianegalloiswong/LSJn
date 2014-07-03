let temps_total = ref 0.
let start_actuel = ref 0.
let appels = ref 0 (* à prouvable *)

let time f x =
if Options.time_on() then
    let start = Unix.gettimeofday () in
    start_actuel := start;
    appels := 0;
    let res = f x in
    let stop = Unix.gettimeofday () in
    let temps = (stop -. start) in
    temps_total := !temps_total +. temps;
    if Options.print_temps_un() then Format.printf "%fs, %d appels à prouvable@." temps !appels;
    res
else f x

exception Temps_ecoule

let verif () =
  incr appels;
  if Options.stop_on() && (Unix.gettimeofday () -. !start_actuel > !Options.temps_max) then
    raise Temps_ecoule;
