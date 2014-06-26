type variable = string
type connecteur = Et | Ou | Imp
type formule = FFaux | FVar of variable | F of connecteur*formule*formule

let non f = F (Imp,f,FFaux)


(* sous_formules *)
type case = CFaux | CVar of variable | C of (connecteur*int*int)

(* choix_formules *)
type quoi_faire = QF_aucun | QF_imp | QF_etR | QF_ouL | QF_ouR | QF_etL | QF_fauxL

(* seq *)
type sous_formule = int
type couple = int*sous_formule



(* variables globales *)
let sf = ref [|CFaux|]
let classe = ref [|0|]
let priorite = ref [|0|]


(* affichage *)
let details = ref false
let aff_preuves = ref false
let aff_cmods = ref false

type logique = IL | CL
