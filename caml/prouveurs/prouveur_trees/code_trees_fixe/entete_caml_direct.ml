(*entete_caml_direct*)

(*equal ?*)

let pred n = n-1

let rec rm x = function
  | [] -> assert false
  | h::t when h=x -> t
  | h::t -> h::(rm x t)
