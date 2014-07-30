let indexation = ref false
let affiche_formule_ref = ref false
let affiche_rep_ref = ref false
let rien_afficher = ref false

let trees = ref false
let trees_via_caml = ref false
let trees_machine = ref false
let prouveur_trees () = !trees || !trees_via_caml || !trees_machine
let compile_caml = ref false

let preuves = ref false
let cmods = ref false



let notime = ref false
let time_on () = not !notime && not !indexation
let temps_max = ref 0.
let stop_on () = !temps_max <> 0.



let affiche_formule () = !affiche_formule_ref || !indexation
let affiche_nom_formule () = not !rien_afficher
let affiche_rep () = !affiche_rep_ref
let affiche_nom_fichier () = (*not !rien_afficher*) false
let affiche_temps_un () = (not !notime) && (not !rien_afficher)
let affiche_temps_etapes () = (not !notime) && (not !rien_afficher)
let affiche_ax_et_conj () = false
let affiche_temps_total () = time_on ()


let irr = ref false

let compile_test = ref false


