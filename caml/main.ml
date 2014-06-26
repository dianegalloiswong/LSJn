open Def


let p = FVar "p"
let q = FVar "q"
let a = FVar "a"
let b = FVar "b"
let c = FVar "c"

let f0 = FFaux
let f1 = non f0



let f2 = F(Imp,p,p)
let f2bis = F(Imp,FFaux,p)

let f3 = F(Ou,p,non p)
let f4 = non (non f3)

let f5 = F(Imp, F(Imp, F(Ou,p,F(Imp,p,q)), q), q)

let f6 = F(Imp, F(Imp, F(Imp,non(non p),p), F(Ou,non p,p) ), F(Ou,non(non p),non p) )



let f7 = F(Ou, F(Imp,a,b), F(Imp,b,a))
(*
let f8 = Tiroirs.eq_boucle 14
let f9 = Tiroirs.eq_boucle 15
let f10 = Tiroirs.tiroirs 4 3
let f11 = Tiroirs.tiroirs 4 4
*)

let f8 = Tiroirs.eq_boucle 2
let f9 = Tiroirs.eq_boucle 5
let f10 = Tiroirs.tiroirs 3 2
let f11 = Tiroirs.tiroirs 2 2


let l = [f0;f1;f2;f2bis;f3;f4;f5;f6;f7;f8;f9;f10;f11]

let l1 = [f0;f1;f2;f2bis;f3;f7]

let f = Tiroirs.tiroirs 5 4
let f' = Tiroirs.eq_boucle 26
let l2 = [f']


let ltest = l



let () = List.iter (fun f ->LSJn.test f) ltest; print_newline()




let l_att = [f0,false,false;
	     f1,true,true;
	     f2,true,true;
	     f2bis,true,true;
	     f3,false,true;
	     f4,true,true;
	     f5,true,true;
	     f6,false,true;
	     f7,false,true;
	     f8,false,false;
	     f9,false,true;
	     f10,true,true;
	     f11,false,false]


(*let () = List.iter (fun f ->LSJn.test_attendu f) l_att; Format.printf "@."*)
