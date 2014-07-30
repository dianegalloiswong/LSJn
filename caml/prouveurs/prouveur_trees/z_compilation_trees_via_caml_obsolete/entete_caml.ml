(* fichier entete_caml.ml *)
type tree = Null | Int of int | Node of tree*tree

let of_bool b = if b then Int 1 else Int 0
let to_bool = function Int 0 -> false | Int _ -> true | _ -> assert false
let to_int = function Int n -> n | _ -> assert false

let isnull = function Null -> of_bool true | _ -> of_bool false
let isint = function Int _ -> of_bool true | _ -> of_bool false
let isnode = function Node _ -> of_bool true | _ -> of_bool false

let rec inexistant (t:tree) = t
(* pour pouvoir imprimer des "and" ensuite *)
