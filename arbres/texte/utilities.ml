(*
utilities.ml

equal : <x,y> -> booléen x=y égalité structurelle
compare : <x,y> -> booléen x<=y. null < tous les entiers par ordre croissants < tous les couples par ordre lexicographique

pred : n -> n-1

length : l -> longueur de la liste l

rm : <x,l> -> liste l privée de l'élément x
mem : <x,l> -> booléen : x appartient à liste l
mem_inf <<n,x>,l> -> booléen : l contient un couple <i,x> avec i<=n (i,n entiers)

sort : l -> l triée (insertion)

filter : l -> l' liste des éléments de l qui vérifient condition (fonction inexistante, c'est juste un exemple)

nth : <n,l> -> le n-ième élément de l (commence à 0)
*)
let equal arg =
  match arg with <x,y> =>
  if isnull x && isnull y then 1 else
  if isint x && isint y && x=y then 1 else
  if isnode x && isnode y then
    match x with <x1,x2> => match y with <y1,y2> =>
    (call equal <x1,y1>) && (call equal <x2,y2>)
  else 0

let compare arg =
  match arg with <x,y> =>
  if isnull x then 1 else if isnull y then 0 else
  if isint x && isint y then x<=y else
  if isint x then 1 else if isint y then 0 else
  match x with <x1,x2> => match y with <y1,y2> =>
  if call equal <x1,y1> then call compare <x2,y2>
  else call compare <x1,y1>

let pred_aux arg =
  match arg with <n,arg2> =>
  match arg2 with <i,j> =>
  if j=n then i
  else call pred_aux <n,<i+1,j+1>>
let pred n =
  if n<=0 then 0 else
  call pred_aux <n,<0,1>>

(***)

let length l =
  if isnull l then 0 else
  match l with <h,t> =>
  call length t +1

let rm arg =
  match arg with <x,l> =>
  match l with <h,t> =>
  if call equal <h,x> then t else <h,call rm <x,t>>

let mem arg =
  match arg with <x,l> =>
  if isnull(l) then 0 else
    match l with <hd,tl> =>
    if call equal <hd,x> then 1 else call mem <x,tl>

let mem_inf arg =
  match arg with <arg1,l> =>
  if isnull(l) then 0 else
    match arg1 with <n,x> =>
    match l with <hd,tl> =>
    match hd with <i,y> =>
    if (i<=n && call equal <y,x>) then 1 else call mem_inf <arg1,tl>

let insert arg =
  match arg with <x,l> =>
  if isnull l then <x,null> else
  match l with <h,t> =>
  if call compare <x,h> then
    <x,l>
  else
    <h, call insert <x,t> >

let sort l =
  if isnull l then l else
  match l with <h,t> =>
  let t = call sort t in
  call insert <h,t>

(*
let filter l =
  if isnull l then l else
  match l with <h,t> =>
  let t = call filter t in
  if call condition h then <h,t> else t
*)

let nth arg =
  match arg with <n,l> =>
  if isnull l then null else
  match l with <h,t> =>
  if n<=0 then h else call nth <call pred n,t>
