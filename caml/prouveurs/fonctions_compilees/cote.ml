open Def
open Global_ref

type cote = L | R
let autre = function L -> R | R -> L
let cote = ref [||]


let remplir () =
  let m = Array.length !sf -1 in
  let tab = Array.make (m+1) L in
  let rec aux i c = 
    tab.(i) <- c;
    match !sf.(i) with
      | CFaux | CVar _ -> ()
      | C (conn,i1,i2) ->
	aux i1 (if conn=Imp then autre c else c);
	aux i2 c
  in aux m R;
  cote := tab



