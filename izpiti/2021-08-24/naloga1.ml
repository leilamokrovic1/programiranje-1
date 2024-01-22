(* a *)
(*----------------------------------------------------------------------------*]
  Napišite predikat `je_urejena : int * int * int -> bool`, ki pove, ali je 
  podana trojica celih števil urejena strogo naraščajoče.
[*----------------------------------------------------------------------------*)
let je_urejena : int * int * int -> bool=
   fun (x, y, z) -> x < y && y < z
(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `poskusi_deljenje : float option -> float option -> float option`, 
  ki sprejme morebitni deljenec in morebitni delitelj ter vrne rezultat deljenja, 
  če se to da, ali pa `None`, če ne (kadar kakšnega argumenta ni ali pa bi prišlo 
  do deljenja z nič). 
  
    # poskusi_deljenje (Some 1.0) (Some 2.0);;
    - : float option = Some 0.5
    # poskusi_deljenje (Some 1.0) (Some 0.0);;
    - : float option = None
    # poskusi_deljenje None (Some 2.0);;
    - : float option = None
[*----------------------------------------------------------------------------*)
let poskusi_deljenje deljenec delitelj= 
match (deljenec, delitelj) with
| (Some x, Some y) -> ( match y with 
  | 0.0 -> None
  | _ -> Some (x/. y))
| _ -> None
(* c *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo `zavrti : 'a list -> int -> 'a list`, ki seznam zavrti 
  za dano število mest v levo (v vsaki rotaciji se prvi element prestavi na 
  konec seznama).
  
    # zavrti [1; 2; 3; 4; 5] 2;;
    - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)
let rec zavrti sez n = 
if n <= 0 then sez
else 
  match sez with
  | [] -> []
  | x :: xs -> zavrti (xs @ [x]) (n-1) 
(* d *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `razdeli : ('a -> int) -> 'a list -> ('a list *  'a list * 'a list)|`, 
  ki sprejme cenilno funkcijo in seznam elementov. Vrne naj trojico, kjer so na prvem 
  mestu vsi elementi za katere je cenilna funkcija negativna, na drugem vsi, kjer 
  je enaka 0, na tretjem pa vsi preostali elementi.
  Elementi naj v seznamih nastopajo v enakem vrstnem redu kot v prvotnem seznamu. 
  Za vse točke naj bo funkcija repno rekurzivna.
  
    # razdeli ((-) 3) [1; 2; 3; 4; 5; 6];;
    - : int list * int list * int list = ([4; 5; 6], [3], [1; 2])
[*----------------------------------------------------------------------------*)
let razdeli f seznam = 
   let rec razdeli_aux negativen nicelni ostali= function 
   | [] ->  negativen, nicelni, ostali
   | x :: xs when f x < 0 -> razdeli_aux (negativen @ [x])  nicelni ostali xs
   | x :: xs when f x = 0 -> razdeli_aux negativen (nicelni @ [x]) ostali xs
   | x :: xs ->  razdeli_aux negativen nicelni (ostali @ [x] ) xs in 
razdeli_aux [] [] [] seznam
   
