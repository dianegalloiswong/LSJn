(*
fichier fonctions_sf

non (faux)
  sf   classe   description
   1      0      Faux
   2      0      Faux
   3      1      1 -> 2

*)
let sf3_prem1 arg = 
(match arg with <i,seq> => 
(let seq = (call rm_d < < < i , < 5 , 3 > > , 1 > , seq >) in 
(let seq = (call add_d < < < i , < 6 , 2 > > , 0 > , seq >) in 
(let seq = (call add_g < < < i , < 0 , 1 > > , 0 > , seq >) in 
seq))))

let sf3_rev1 arg = 
(match arg with <i,seq> => 
(let seq = (call rm_g < < < i , < 0 , 1 > > , 0 > , seq >) in 
(let seq = (call rm_d < < < i , < 6 , 2 > > , 0 > , seq >) in 
(let seq = (call add_d < < < i , < 5 , 3 > > , 1 > , seq >) in 
(let seq = (call rm_ax seq) in 
seq)))))

let sf3_prem2 arg = 
(match arg with <i,seq> => 
(let seq = (call incr_n seq) in 
(let i = (i +1) in 
(let seq = (call rm_d < < < i , < 5 , 3 > > , 1 > , seq >) in 
(let seq = (call add_d < < < i , < 6 , 2 > > , 0 > , seq >) in 
(let seq = (call add_g < < < i , < 0 , 1 > > , 0 > , seq >) in 
seq))))))

let sf3_rev2 arg = 
(match arg with <i,seq> => 
(let i = (i +1) in 
(let seq = (call rm_g < < < i , < 0 , 1 > > , 0 > , seq >) in 
(let seq = (call rm_d < < < i , < 6 , 2 > > , 0 > , seq >) in 
(let seq = (call add_d < < < i , < 5 , 3 > > , 1 > , seq >) in 
(let seq = (call rm_ax seq) in 
(let seq = (call decr_n seq) in 
seq)))))))

let ajout_formule_initiale seq = 
(call add_d < < < 0 , < 5 , 3 > > , 1 > , seq >)

let call_rev3 arg = 
(match arg with <k,x> => 
(if (k<2) then 
(if (k<1) then 
(call sf0_rev3 x) else 
(call sf1_rev3 x)) else 
(if (k<3) then 
(call sf2_rev3 x) else 
(call sf3_rev3 x))))

let call_prem3 arg = 
(match arg with <k,x> => 
(if (k<2) then 
(if (k<1) then 
(call sf0_prem3 x) else 
(call sf1_prem3 x)) else 
(if (k<3) then 
(call sf2_prem3 x) else 
(call sf3_prem3 x))))

let call_rev2 arg = 
(match arg with <k,x> => 
(if (k<2) then 
(if (k<1) then 
(call sf0_rev2 x) else 
(call sf1_rev2 x)) else 
(if (k<3) then 
(call sf2_rev2 x) else 
(call sf3_rev2 x))))

let call_prem2 arg = 
(match arg with <k,x> => 
(if (k<2) then 
(if (k<1) then 
(call sf0_prem2 x) else 
(call sf1_prem2 x)) else 
(if (k<3) then 
(call sf2_prem2 x) else 
(call sf3_prem2 x))))

let call_rev1 arg = 
(match arg with <k,x> => 
(if (k<2) then 
(if (k<1) then 
(call sf0_rev1 x) else 
(call sf1_rev1 x)) else 
(if (k<3) then 
(call sf2_rev1 x) else 
(call sf3_rev1 x))))

let call_prem1 arg = 
(match arg with <k,x> => 
(if (k<2) then 
(if (k<1) then 
(call sf0_prem1 x) else 
(call sf1_prem1 x)) else 
(if (k<3) then 
(call sf2_prem1 x) else 
(call sf3_prem1 x))))

