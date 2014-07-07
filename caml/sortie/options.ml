let indexation = ref false
let preuves = ref false
let cmods = ref false
let compare = ref true (*!*)
let compare_seul = ref false
(*let classique = ref false*)

let rien_afficher = ref false

let notime = ref false
let time_on () = not !notime && not !indexation
let temps_max = ref 0.
let stop_on () = !temps_max <> 0.



let print_formule () = not !compare_seul && not !rien_afficher
let print_rep () = not !compare_seul && not !rien_afficher
let print_fichier () = not !rien_afficher
let print_temps_un () = (not !notime) && (not !rien_afficher)
let print_ax_et_conj () = (* not !compare_seul *) false
let print_temps_total () = time_on ()

