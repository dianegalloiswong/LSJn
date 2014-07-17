(*
fichier all_imp

x : <i,<p,sf>> (les éléments de G et D)

is_imp_g : <n,x> -> i<=n et p=5
is_imp_d

filter_is_imp_g : <n,l> -> les éléments de l avec i<=n et p=5
filter_is_imp_d : <n,l> -> les éléments de l avec i=n et p=5

nb_imp_g
nb_imp_d

nth_imp_g : renvoie les éléments <i,sf> du n-ième implique de G
nth_imp_d

*)

let is_imp_g arg =
  match arg with <n,x> =>
  match x with <i,a> =>
  i<=n && (match a with <p,sf> => p=5)

let is_imp_d arg =
  match arg with <n,x> =>
  match x with <i,a> =>
  i=n && (match a with <p,sf> => p=5)

let filter_is_imp_g arg =
  match arg with <n,l> =>
  if isnull l then l else
  match l with <h,t> =>
  let t = call filter_is_imp_g <n,t> in
  if call is_imp_g <n,h> then <h,t> else t

let filter_is_imp_d arg =
  match arg with <n,l> =>
  if isnull l then l else
  match l with <h,t> =>
  let t = call filter_is_imp_d <n,t> in
  if call is_imp_d <n,h> then <h,t> else t

let all_imp_g seq =
  let n = call n_of_seq seq in
  let G = call G_of_seq seq in
  call filter_is_imp_g <n,G>

let all_imp_d seq =
  let n = call n_of_seq seq in
  let D = call D_of_seq seq in
  call filter_is_imp_d <n,D>

let nb_imp_g seq =
  call length (call all_imp_g seq)
let nb_imp_d seq =
  call length (call all_imp_d seq)

let nth_imp_g arg =
  match arg with <k,seq> =>
  let x = call nth <k, call all_imp_g seq> in
  match x with <i,a> =>
  match a with <p,sf> =>
  <i,sf>
let nth_imp_d arg =
  match arg with <k,seq> =>
  let x = call nth <k, call all_imp_d seq> in
  match x with <i,a> =>
  match a with <p,sf> =>
  <i,sf>
