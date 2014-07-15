open Def



let p = FVar "p"
let q = FVar "q"
let a = FVar "a"
let b = FVar "b"
let c = FVar "c"

let non f = F (Imp,f,FFaux)

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
let f8 = Eq_boucle.main 2
let f9 = Eq_boucle.main 5
let f10 = Tiroirs.main 3 2
let f11 = Tiroirs.main 2 2


let l = [f0;f1;f2;f2bis;f3;f4;f5;f6;f7;f8;f9;f10;f11]

let l_att = [(*f0,*)false,false;
	     (*f1,*)true,true;
	     (*f2,*)true,true;
	     (*f2bis,*)true,true;
	     (*f3,*)false,true;
	     (*f4,*)true,true;
	     (*f5,*)true,true;
	     (*f6,*)false,true;
	     (*f7,*)false,true;
	     (*f8,*)false,false;
	     (*f9,*)false,true;
	     (*f10,*)true,true;
	     (*f11,*)false,false
            ]
let l_att = List.map (fun x -> Some x) l_att

let l1 = [f0;f1;f2;f2bis;f3;f4;f7]

let l1_att = [(*f0,*)false,false;
	      (*f1,*)true,true;
	      (*f2,*)true,true;
	      (*f2bis,*)true,true;
	      (*f3,*)false,true;  
	      (*f4,*)true,true;
	      (*f7,*)false,true;
	     ]
let l1_att = List.map (fun x -> Some x) l1_att
*)

(*
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
	     f11,false,false
	    ]
let l1_att = [f0,false,false;
	      f1,true,true;
	      f2,true,true;
	      f2bis,true,true;
	      f3,false,true;  
	      f4,true,true;
	      f7,false,true;
	     ]
*)
