open Def
open Global_ref
open Fonctions_de_sf

type cote = L | R
let autre = function L -> R | R -> L
let cote = ref [||]


let remplir_cote () =
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



let remplir_fonctions () =
  let m = Array.length !sf -1 in
  for h=1 to m do
    match !sf.(h) with
      | CFaux | CVar _ -> ()
      | C (conn,a,b) ->
	let f = match conn,!cote.(h) with
	  | Et,L -> etL
	  | Et,R -> etR
	  | Ou,L -> ouL
	  | Ou,R -> ouR
	  | Imp,L -> impL
	  | Imp,R -> impR
	in f h a b
  done

