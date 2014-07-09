type regle = R_Irr | R_etL | R_etR of int | R_ouL of int | R_ouR | R_impL of int | R_impR | R_Succ
type t = R of regle*int*(t list)
