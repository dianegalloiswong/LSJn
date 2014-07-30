(*type var = string
type func = string*)

open Ast_trees
(*
  | EVar of var
  | ENull
  | EInt of int
  | ENode of expr*expr
  | EMatch of expr*var*var*expr
  | ELetin of var*expr*expr
  | ECall of func*expr
  | EIsnull of expr
  | EIsint of expr
  | ELeq of expr*expr
  | EIf of expr*expr*expr
  | ESucc of expr
  | EPred of expr
*)
type decl_func = func*(var*expr)

open Machine_abstraite
(*
type instr =
  | LDA of int
  | LDC of int
  | LDN
  | LDV of var
  | SAVE of var
  | STV of var
  | RST of var

  | UJP
  | SKIP
  | PAIR
  | SPLIT
  | SWAP
  | ISNULL
  | ISINT
  | SUCC
  | PRED
  | LEQ
  | HALT
*)
let rec expr_compile lnk i e = 
  match e with
      | ENull                -> (1,[LDN]) 
      | EInt x               -> (1,[LDC x])
      | EVar x               -> (1,[LDV x])
      | ESucc e1             -> (match expr_compile lnk i e1 with
                                  | (le1,ce1) -> (le1+1,ce1@[SUCC]) 
                                )
      | EPred e1             -> (match expr_compile lnk i e1 with
                                  | (le1,ce1) -> (le1+1,ce1@[PRED])
                                )
      | ENode (e1,e2)        -> (match expr_compile lnk i e1 with 
                                  | (le1,ce1) ->
                                      let i2 = i+le1                 in
                                      (match expr_compile lnk i2 e2 with
                                         | (le2,ce2) -> (le1+le2+1,ce1@ce2@[PAIR])
                                      )
                                ) 
      | ECall (f,e1)         -> (match lnk f with
                                   | Some af -> 
                                     (match expr_compile lnk i e1 with
                                        | (le1,ce1) ->
                                      let i2 = i+le1+4                       in
                                      let ce = ce1@[LDA i2;SWAP;LDA af;UJP]  in
                                      let le = le1+ 4                   
                                      in  (le,ce)
                                     )
                                   | None ->  (1,[HALT])
                                )
      | ELetin (x,e1,e2)     -> (match expr_compile lnk i e1 with 
                                        | (le1,ce1) ->
                                      let i2 = i+le1+3                    in
                                      (match expr_compile lnk i2 e2 with
                                        | (le2,ce2) -> (le1+3+le2+2,ce1@[SAVE x;SWAP;STV x]@ce2@[SWAP;RST x])
                                      )
                                 )
      | EMatch (e1,x,y,e2)   -> (match expr_compile lnk i e1 with 
                                        | (le1,ce1) ->
                                      let i2 = i+le1+8                    in
                                      (match expr_compile lnk i2 e2 with
                                        | (le2,ce2) -> (le1+8+le2+4,ce1@[SAVE x;SWAP;SAVE y;SWAP;SPLIT;SWAP;STV x;STV y]@
                                                                    ce2@[SWAP;RST y;SWAP;RST x])
                                      )
                                 ) 
      | ELeq (e1,e2)         -> (match expr_compile lnk i e1 with 
                                        | (le1,ce1) ->
                                      let i2 = i+le1                     in
                                      (match expr_compile lnk i2 e2 with
                                        | (le2,ce2) -> (le1+le2+1,ce1@ce2@[LEQ])
                                      )
                                )
      | EIsnull e1           -> (match expr_compile lnk i e1 with
                                        | (le1,ce1) -> (le1+1,ce1@[ISNULL])
                                )
      | EIsint e1            -> (match expr_compile lnk i e1 with
                                        | (le1,ce1) -> (le1+1,ce1@[ISINT])
                                )
      | EIf (b,e1,e2)        -> (match expr_compile lnk i b with
                                        | (le0,ce0) ->
                                      let i1 = i+le0+3                   in
                                      (match expr_compile lnk i1 e1 with 
                                        | (le1,ce1) ->
                                      let i2 = i1+le1+2                  in
                                      (match expr_compile lnk i2 e2 with
                                        | (le2,ce2) -> 
                                      let i3 = i2+le2                    
                                      in  (le0+3+le1+2+le2,ce0@[SKIP;LDA i2;UJP]@ce1@[LDA i3;UJP]@ce2)
                                      ))
                                 )
      | _ -> assert false

let expr_length lnk e = fst (expr_compile lnk 0 e)

let recfun_compile lnk i (_,(pf,bf)) = 
    let (le,ce) = expr_compile lnk (i+3) bf in
    let code = [SAVE pf;SWAP;STV pf]@ce@[SWAP;RST pf;SWAP;UJP] in
    let lcode = 3 + le + 4        
    in  (lcode,code)

let recfun_length lnk (f,(pf,bf)) = (f,fst (recfun_compile lnk 0 (f,(pf,bf))))

let prg_compile lnk i e =
    match expr_compile lnk i e with
        | (le,ce) -> (i,ce@[LDA 0;UJP],le+2)
    



let list_assoc l f = try Some (List.assoc f l)  with Not_found -> None

let dummy_lnk prg f = 
  match list_assoc prg f with
    | Some _ -> Some 0
    | None   -> None

let rec linker_rec i (lc : (func * int) list) = 
    match lc with  
      | []        -> []  
      | (f,l)::lc -> (f,i)::linker_rec (i+l) lc
    

let new_linker lnk i prg = 
  let lc = List.map (recfun_length lnk) prg in
  let lr = linker_rec i lc
  in  list_assoc lr 

let linker i prg = new_linker (dummy_lnk prg) i prg

let rec cc_prg lnk i prg =
    match prg with 
      | []      -> []
      | rf::prg -> let (l,c) = recfun_compile lnk i rf
                   in  (i,c,l)::(cc_prg lnk (i+l) prg)
    

let main (prg,exp) =
  let lexp     = expr_length (dummy_lnk prg) exp  in
  let lnk      = linker (1+lexp+2) prg            in
  let code_exp = prg_compile lnk 1 exp            in
  let code_fun = cc_prg lnk (1+lexp+2) prg
  in  HALT::List.flatten (List.map (fun (_,x,_) -> x) (code_exp::code_fun))





(*
let prg = ("f",("x", EVar "x"))::[]
let exp = ECall ("f",ENull)

compile prg exp
*)
