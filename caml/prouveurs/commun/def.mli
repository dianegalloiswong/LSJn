type variable = string
type connecteur = Et | Ou | Imp
type formule = FFaux | FVar of variable | F of connecteur*formule*formule







