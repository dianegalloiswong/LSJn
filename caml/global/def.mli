type variable = string
type connecteur = Et | Ou | Imp
type formule = FFaux | FVar of variable | F of connecteur*formule*formule


(* choix formule *)
type quoi_faire = QF_aucun | QF_imp | QF_etR | QF_ouL | QF_ouR | QF_etL | QF_fauxL

(* seq *)
type sous_formule = int
type couple = int*sous_formule
