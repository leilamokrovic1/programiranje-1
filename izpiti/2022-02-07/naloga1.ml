(* 1. a) *)
let je_idempotent ((x, y), (z, w)) =
  x*x + y*z = x && x*y + w*y = y && z*x + w*z = z && z*y + w*w = w

(* 1. b) *)
let produkt = 
  let rec produkt_aux acc = function
  | [] -> acc
  | x:: xs when x = 0-> produkt_aux acc xs
  | x:: xs -> produkt_aux (acc * x) xs
in produkt_aux 1

(* 1. c) *)
let stalin_sort =
  let rec stalin acc max_seen= function
  | [] -> acc
  | x:: xs -> (if x > max_seen then stalin (acc @ [x]) x xs
  else stalin acc max_seen xs) in
  stalin [] 0

(* 1. d) *)


