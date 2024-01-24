(* 1. a) *)
let permutacije a b c =
  [(a, b, c); (a, c, b); (b, a, c); (b, c, a); (c, a, b); (c, b, a)]
(* 1. b) *)

let zip_opt sez1 sez2=
  let rec zip_aux acc sez1 sez2  = 
  match (sez1, sez2) with
  | (x::xs, []) ->  zip_aux (acc @ [(Some x, None)]) xs []
  | ([], x :: xs) -> zip_aux (acc @ [(None, Some x)]) [] xs
  | (x::xs, y::ys) -> zip_aux (acc @ [(Some x, Some y)]) xs ys
  | ([], []) -> acc in 
  zip_aux [] sez1 sez2 

(* 1. c) *)

let zip_default sez1 sez2 d1 d2=
  let rec zip_aux_d acc sez1 sez2 =
    match (sez1, sez2) with
    | (x::xs, []) ->  zip_aux_d (acc @ [(x, d2)]) xs []
    | ([], x :: xs) -> zip_aux_d (acc @ [(d1, x)]) [] xs
    | (x::xs, y::ys) -> zip_aux_d (acc @ [(x, y)]) xs ys
    | ([], []) -> acc in 
    zip_aux_d [] sez1 sez2

(* 1. d) *)

type response = Left | Middle | Right

let distribute f lst=
  let rec distribute_aux left middle right= function
  | [] -> (List.rev left, List.rev middle, List.rev right)
  | x:: xs -> 
    match f x with
    | Right -> distribute_aux left middle (x :: right) xs
    | Left -> distribute_aux (x :: left) middle right xs
    | Middle -> distribute_aux left (x :: middle) right xs
  in 
  distribute_aux [] [] [] lst
  
  
  
(* 1. e) *)

type ('a, 'b) sum = Left of 'a | Right of 'b

let iso1 f =
  let g_left x = f (Left x) in
  let g_right y = f (Right y) in
  (g_left, g_right)
  
let iso2 (g_left, g_right) = function
  | Left x -> g_left x
  | Right y -> g_right y