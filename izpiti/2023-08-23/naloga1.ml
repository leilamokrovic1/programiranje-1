(* 1. a) *)

let vzporedna (x1, y1) (x2, y2) =
  if x1 * y2 = x2 * y1 then true
  else false 
 
(* 1. b) *)
let rec prekrizani sez1 sez2= 
match sez1, sez2 with
| [], [] -> []
| x1 :: x2 :: xs, y1:: y2:: ys -> (x1, y2) :: (x2, y1) :: (prekrizani xs ys)
| _, _ -> failwith "neustrezna seznama"

(* 1. c) *)

    
(* 1. d) *)
let rec repi sez =
  match sez with
 | [] -> [[]]
 | _ :: xs -> sez:: repi xs
(* 1. e) *)
type ('a , 'b) sum = Left of 'a | Right of 'b

 
 