let appel_fk arg = 
(match arg with <k,x> => 
(if (k<5) then 
(if (k<2) then 
(if (k<1) then 
(call f0 x) else 
(call f1 x)) else 
(if (k<3) then 
(call f2 x) else 
(if (k<4) then 
(call f3 x) else 
(call f4 x)))) else 
(if (k<7) then 
(if (k<6) then 
(call f5 x) else 
(call f6 x)) else 
(if (k<8) then 
(call f7 x) else 
(if (k<9) then 
(call f8 x) else 
(call f9 x))))))
