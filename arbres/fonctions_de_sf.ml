open Def
open Global_ref

open Ast


let fonctions = ref []
let prefixe h = "fonction_sf"^(string_of_int h)^"_"
let prem j h = (prefixe h)^"prem"^(string_of_int j)
let rev j h = (prefixe h)^"rev"^(string_of_int j)

let rec ajout_fonctions_aux h k = function
  | [] -> ()
  | body::tl ->
    let nom = match k with
      0->prem 1 |1->rev 1 |2->prem 2 |3->rev 2 |4->prem 3 |5->rev 3 |_->assert false
    in fonctions := (nom h,"arg",body):: !fonctions;
    ajout_fonctions_aux h (k+1) tl
let ajout_fonctions h l = ajout_fonctions_aux h 0 l



let eletin_seq func var a =
  eletin "seq" (ecall func (enode (enode (enode (evar var) (eint a)) (eint !classe.(a))) (evar "seq")))
(* func : rm_g ou add_d ou etc., var : "i" ou "n"... *)

let body_of_list l =
  ematch (evar "arg") "i" "seq" (
    List.fold_right (fun f x -> f x) l (evar "seq")
  )



let etL_ouR is_etL h a b =
  let add = if is_etL then "add_g" else "add_d" in
  let rm = if is_etL then "rm_g" else "rm_d" in
  let body_prem = body_of_list [
    eletin_seq rm "i" h;
    eletin_seq add "i" a;
    eletin_seq add "i" b;
  ] in
  let body_rev = body_of_list [
    eletin_seq rm "i" b;
    eletin_seq rm "i" a;
    eletin_seq add "i" h;
  ] in
  ajout_fonctions h [body_prem;body_rev]

let etL = etL_ouR true
let ouR = etL_ouR false


let ouL_etR is_ouL h a b =
  let add = if is_ouL then "add_g" else "add_d" in
  let rm = if is_ouL then "rm_g" else "rm_d" in
  let body_prem1 = body_of_list [
    eletin_seq rm "i" h;
    eletin_seq add "i" a;
  ] in
  let body_rev1 = body_of_list [
    eletin_seq rm "i" a;
    eletin_seq add "i" h;
  ] in
  let body_prem2 = body_of_list [
    eletin_seq rm "i" h;
    eletin_seq add "i" b;
  ] in
  let body_rev2 = body_of_list [
    eletin_seq rm "i" b;
    eletin_seq add "i" h;
  ] in
  ajout_fonctions h [body_prem1;body_rev1;body_prem2;body_rev2]

let ouL = ouL_etR true
let etR = ouL_etR false




let eletin_n_of_seq = eletin "n" (ecall "n_of_seq" (evar"seq"))
let eletin_seq_incr_n = eletin "seq" (ecall "incr_n" (evar"seq"))
let eletin_seq_decr_n = eletin "seq" (ecall "decr_n" (evar"seq"))


let impL h a b =

  let body_prem1 = body_of_list [
    eletin_seq "rm_g" "i" h;
    eletin_seq "add_g" "i" b;
  ] in
  let body_rev1 = body_of_list [
    eletin_seq "rm_g" "i" b;
    eletin_seq "add_g" "i" h;
  ] in

  let list_prem2 = [
    eletin_n_of_seq;
    eletin "nplus1" (esucc (evar"n"));
    eletin_seq "rm_g" "i" h;
    eletin_seq "add_g" "nplus1" b;
    eletin_seq "add_d" "n" a;
  ] in
  let body_prem2 = body_of_list list_prem2 in
  let body_prem3 = body_of_list (eletin_seq_incr_n::list_prem2) in

  let list_rev2 = [
    eletin_n_of_seq;
    eletin "nplus1" (esucc (evar"n"));
    eletin_seq "rm_d" "n" a;
    eletin_seq "rm_g" "nplus1" b;
    eletin_seq "add_g" "i" h;
  ] in
  let body_rev2 = body_of_list list_rev2 in
  let body_rev3 = body_of_list (list_rev2@[eletin_seq_decr_n]) in

  ajout_fonctions h [body_prem1;body_rev1;body_prem2;body_rev2;body_prem3;body_rev3]



let impR h a b =

  let list_prem1 = [ (* i=n *)
    eletin_seq "rm_d" "i" h;
    eletin_seq "add_d" "i" b;
    eletin_seq "add_g" "i" a;
  ] in
  let body_prem1 = body_of_list list_prem1 in
  let list_rev1 = [
    eletin_seq "rm_g" "i" a;
    eletin_seq "rm_d" "i" b;
    eletin_seq "add_d" "i" h;
  ] in
  let body_rev1 = body_of_list list_rev1 in

  let list_prem2 = eletin_seq_incr_n :: (eletin "i" (esucc(evar "i"))) :: list_prem1 in
  let body_prem2 = body_of_list list_prem2 in
  let list_rev2 = ( (eletin "i" (esucc(evar "i"))) :: list_prem1 ) @ [eletin_seq_decr_n] in
  let body_rev2 = body_of_list list_rev2 in

  ajout_fonctions h [body_prem1;body_rev1;body_prem2;body_rev2]















(*
etL h, prem :
(h = a->b, ca = !classe.(a), ...)

let sf$h_prem arg = match arg with <i,s> =>
  let s = call rm_g < <<i,$h>,$ch> , s > in
  let s = call add_g < <<i,$a>,$ca> , s > in
  let s = call add_g < <<i,$b>,$cb> , s > in
  s

*)

(*
let etL h a b =

  let body =
    ematch (evar "arg") "i" "seq" (
      eletin_seq "rm_g" "i" h (
      eletin_seq "add_g" "i" a (
      eletin_seq "add_g" "i" b (
	(evar "seq")
      )))
    )
  in

  ("prem"^(string_of_int h), "arg", body)
*)
(*
      eletin "seq" (ecall "rm_g" (enode (enode (enode (evar "i") (eint h)) (eint ch)) (evar "seq"))) (
      eletin "seq" (ecall "add_g" (enode (enode (enode (evar "i") (eint a)) (eint ca)) (evar "seq"))) (
      eletin "seq" (ecall "add_g" (enode (enode (enode (evar "i") (eint b)) (eint cb)) (evar "seq"))) (
*)
