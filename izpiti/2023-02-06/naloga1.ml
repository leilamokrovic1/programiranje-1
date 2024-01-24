(* 1. a) *)
let sum_two_smallest (a, b, c)=
if a > c && a > b 
  then b + c
else 
  if b > a && b > c 
  then a + c
else a + b

(* 1. b) *)
let flatten option1=
match option1 with
| Some (Some x) -> Some x
| Some None -> None
| None -> None
(* 1. c) *)
let dot_product v1 v2=
 let rec dot acc v1 v2=
 match (v1, v2) with
 | ([],[]) -> acc
 | ([], _) -> acc
 | (_, []) -> acc
 | (x :: xs, y:: ys) -> dot (acc + x * y) xs ys in 
 dot 0 v1 v2
(* 1. d) *)
let smalest_modulo list a=
match list with
| [] -> None
| x :: xs ->
  (let rec smalest list a acco accm= match list with 
  | [] -> Some accm
  | x :: xs -> if (x mod a) < acco then smalest xs a (x mod a) x
  else smalest xs a acco accm
in smalest list a a a)

(* 1. e) *)
(* let target_product list1=
  let n = List.length list1 in
  let minimal = ref None in
  for i = 0 to n-1 do
    for j = i to n-1 do
      let product = list1.(i) * list1.(j) in 
      if product = n then
        match !minimal with
        | None -> minimal := Some (min (list1.(i)) (list1.(j)), max list1.(i) list1.(j))
        | Some(min_val, max_val) ->minimal := Some (min min_val (min list1.(i) list1.(j)), max max_val (max list1.(i) list1.(j)))
      done
  done;
  !minimal *)






