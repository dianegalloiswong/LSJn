(* fichier expr *)
in
(*let l = <4,<7,<2,<8,<4,<<3,2>,<<3,3>,<1,<null,null>>>>>>>>> in*)
(*call sort l*)
(*call nth <5,l>*)
(*call length l*)

let seq = call empty_seq null in
let seq = call ajout_formule_initiale seq in
call prouvable seq
