type attendu = IL of bool | CL of bool

type fact = 
  | Conj of string * Def.formule
  | Ax of string * Def.formule
  | Autre of string*string*Def.formule

type facts = fact list

type fichier = (attendu list)*(fact list)
