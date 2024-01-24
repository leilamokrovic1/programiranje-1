type 'a list_tree = Leaf of 'a | Node of 'a list_tree list

(* 1. a) *)
let rec map f drevo= 
match drevo with
| Leaf x -> Leaf (f x)
| Node subtrees -> Node (List.map (map f) subtrees)

(* 1. b) *)
let count t=
 let rec count_aux acc t= match t with
 | Leaf _ -> acc + 1
 | Node subtrees -> 
  List.fold_left (fun acc' subtree-> count_aux acc' subtree) acc subtrees in 
 count_aux 0 t

(* 1. c) *)
let rec apply f_tree a_tree =
  match (f_tree, a_tree) with
  | (Leaf f, Leaf a) -> Leaf (f a)
  | (Node f_subtrees, Node a_subtrees) ->
    let combined = List.map2 apply f_subtrees a_subtrees in 
    Node(combined)
  | _ -> failwith "razlicne dolzine"

(* 1. d) *)
let rec combined f_tree g_tree= 
match (f_tree, g_tree) with
| (Leaf f, Leaf g) -> Leaf (fun x -> f (g(x)))
| (Node f_subtrees, Node g_subtrees) -> 
  let composed_fg = List.map2 combined f_subtrees g_subtrees in 
  Node (composed_fg)
| _ -> failwith "različni obliki"
(* 1. e) *)






let t1 = Node [ Node [ Leaf (fun x -> x) ]; Leaf (fun x -> x * 2) ]
let t2 = Node [ Leaf 1; Leaf 2 ]
let t3 = Node [ Node []; Leaf 2; Leaf 4 ]
let t4 = Node [ Node [ Leaf (fun x -> x) ]; Leaf (fun x -> x * 2) ]
let t5 = Node [ Node []; Leaf 2; Leaf 4 ]
