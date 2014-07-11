open Arbres_ast



(* 


  'a liste : null | <'a,liste>

   sequent : <formules,reste>
   formules : <G,D>
   G/D : liste de <indice,sf>
   reste : <classes,infos>
   classes : <CG,CD>
   CG/CD : liste de <indice,cl>
   infos : <n,axiomes>
   n : indice du sequent
   axiomes : <fauxL,id>

 
*)

let arg = "arg"
let varx = "x"
let vary = "y"
let varz = "z"


(*
let rm arg =
  match arg with <x,l> =>
    match l with <h,t> =>
      if h=x then t else <h,call rm <x,t>>
*)
(*
let decl_rm = 
  let body =
    EMatch (EVar "arg","x","l",
      EMatch (EVar "l","h","t",
	EIf( EEq(EVar "x",EVar "h"),
	  EVar "t", 
	  ENode(EVar "h", ECall("rm",ENode(Evar "x", EVar "t")))
	)
      )
    )
  in ("rm", "arg", body)
*)
let decl_rm = 
  let body =
    ematch (evar "arg") "x" "l"
      (ematch (evar "l") "h" "t"
	(eif (eeq (evar "x") (evar "h"))
	  (evar "t")
	  (enode (evar "h") (ecall "rm" (enode (evar "x") (evar "t"))))
	)
      )
  in ("rm", "arg", body)	    	       


let decl_mem =
  let body =
    ematch (evar "arg") "x" "l" (
      eif (eisnull(evar "l")) efalse ((*else*)
	ematch (evar "l") "hd" "tl" (
	  eif (eeq (evar"x") (evar"hd")) etrue (
	    ecall "mem" (enode (evar"x") (evar"tl"))
	  )
	)
      )
    )
  in ("mem", "arg", body)






let add x l = enode x l
let rm x l = ecall "rm" (enode x l)



(*
rm : <x,l> -> liste l privée de l'élément x
mem : <x,l> -> booléen : x appartient à l
mem_inf <<n,x>,l> -> booléen : l contient un couple <i,x> avec i<=n

n_of_seq : seq -> n

add_g <c,seq> -> seq : ajoute le couple c à G de seq
add_d
rm_g
rm_d

add_cl_g <c,seq> -> seq : ajoute c=(i,cl) à CG, et si axiomes, met à jour
add_cl_d
rm_cl_g
rm_cl_d

add_g < <<i,a>,cl> , seq > ajoute (i,a) à G et (i,cl) à CG, et fauxL:=vrai si cl=0 et i<=n, et id:=vrai si i<=n et (n,cl) présent dans CD

rm_ax : seq -> seq où les axiomes sont mis à faux

incr_n
decr_n

*)
