let preuves = ref false
let cmods = ref false
let compare = ref false
let compare_seul = ref false
(*let classique = ref false*)


let rien_afficher = ref false

let print_formule () = not !compare_seul && not !rien_afficher
let print_rep () = not !compare_seul && not !rien_afficher
let print_fichier () = not !rien_afficher
let print_temps_un () = not !rien_afficher

let notime = ref false
let time_on () = not !notime
let temps_max = ref 0.
let stop_on () = !temps_max <> 0.
