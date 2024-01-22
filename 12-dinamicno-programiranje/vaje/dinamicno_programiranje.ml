(* ========== Vaje 6: Dinamično programiranje  ========== *)


(*----------------------------------------------------------------------------*]
 Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
 samo za eno polje navzdol ali za eno polje na desno in na koncu mora prispeti
 v desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
 različne (ne-negativne) mase. Miška bi se rada kar se da nažrla, zato jo
 zanima, katero pot naj ubere.

 Funkcija [max_cheese cheese_matrix], ki dobi matriko [cheese_matrix] z masami
 sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
 optimalni poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_cheese test_matrix;;
 - : int = 13
[*----------------------------------------------------------------------------*)

let test_matrix = 
  [| [| 1 ; 2 ; 0 |];
     [| 2 ; 4 ; 5 |];
     [| 7 ; 0 ; 1 |] |]


let max_cheese cheese_matrix = 
   let dimy = Array.length cheese_matrix in 
   let dimx = Array.length cheese_matrix.(0) in
   let rec best_path y x=
      let current = cheese_matrix.(y).(x) in
      let best_right = if x+1 >= dimy then 0 else best_path y (x+1) in
      let best_down = if y +1 >= dimx then 0 else best_path (y+1) x in 
      current + max (best_right) (best_down) in 
      best_path 0 0

(*----------------------------------------------------------------------------*]
 Poleg količine sira, ki jo miška lahko poje, jo zanima tudi točna pot, ki naj
 jo ubere, da bo prišla do ustrezne pojedine.

 Funkcija [optimal_path] naj vrne optimalno pot, ki jo mora miška ubrati, da se
 čim bolj nažre. Ker je takih poti lahko več, lahko funkcija vrne poljubno.
 Pripravite tudi funkcijo [convert_path], ki pot pretvori v seznam tež sirčkov
 na poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # optimal_path test_matrix;;
 - : mouse_direction list = [Right; Down; Right; Down]
 # optimal_path test_matrix |> convert_path test_matrix;;
 - : int list = [1; 2; 4; 5; 1]
[*----------------------------------------------------------------------------*)

type mouse_direction = Down | Right

let optimal_path cheese_matrix = 
   let dimy = Array.length cheese_matrix in 
   let dimx = Array.length cheese_matrix.(0) in
   let rec best_path y x=
      let current = cheese_matrix.(y).(x) in
      let best_right, path_right =
      if x + 1 = dimy then (0, []) else best_path y (x+1) in
      let best_down, path_down =
      if y+1 = dimx then (0, []) else best_path (y+1) x in

      let best, step=
      if best_down >= best_right then (best_down, Down::path_down)
      else (best_right, Right :: path_right)
   in 
   (current + best, step)
in
best_path 0 0 |> snd

let convert_path cheese_matrix path= 
   let rec walk y x = function
   | [] -> []
   | dir :: xs -> 
      let r, d=
      match dir with Right -> (0, 1) | Down -> (1, 0)
   in
   cheese_matrix.(y).(x) :: walk (y + d) (x + r) xs in 
   walk 0 0 path
      

(*----------------------------------------------------------------------------*]
 Rešujemo problem sestavljanja alternirajoče obarvanih stolpov. Imamo štiri
 različne tipe gradnikov, dva modra in dva rdeča. Modri gradniki so višin 2 in
 3, rdeči pa višin 1 in 2.

 Funkcija [alternating_towers] za podano višino vrne število različnih stolpov
 dane višine, ki jih lahko zgradimo z našimi gradniki, kjer se barva gradnikov
 v stolpu izmenjuje (rdeč na modrem, moder na rdečem itd.). Začnemo z gradnikom
 poljubne barve.

 Namig: Uporabi medsebojno rekurzivni pomožni funkciji z ukazom [and].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # alternating_towers 10;;
 - : int = 35
[*----------------------------------------------------------------------------*)
let alternating_towers višina=
let rec redtop višina=
   if višina <= 0 then 0
   else if višina <= 2 then 1
   else bluetop (višina -1) + bluetop (višina-2)
and bluetop višina =
   if višina <= 1 then 0
   else if višina <= 3 then 1
   else redtop (višina -2) + redtop (višina -3)
in
redtop višina + bluetop višina
(*----------------------------------------------------------------------------*]
 Izračunali smo število stolpov, a naše vrle gradbince sedaj zanima točna
 konfiguracija. Da ne pride do napak pri sestavljanju, bomo stolpe predstavili
 kar kot vsotne tipe. 

 Stolp posamezne barve so temelji (Bottom), ali pa kot glava bloka pripadajoče
 barve in preostanek, ki je stolp nasprotne barve.

 Definirajte funkcijo [enumerate_towers], ki vrne seznam vseh stolpov podane
 dolžine. Stolpe lahko vrne v poljubnem vrstnem redu. Funkcija naj hitro (in
 brez) prekoračitve sklada deluje vsaj do višine 20.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # enumerate_towers 4;;
 - : tower list = 
    [Red (TopRed (Red2, TopBlue (Blue2, RedBottom)));
     Red (TopRed (Red1, TopBlue (Blue3, RedBottom)));
     Red (TopRed (Red1, TopBlue (Blue2, TopRed (Red1, BlueBottom))));
     Blue (TopBlue (Blue3, TopRed (Red1, BlueBottom)));
     Blue (TopBlue (Blue2, TopRed (Red2, BlueBottom)))]
[*----------------------------------------------------------------------------*)


type blue_block = Blue3 | Blue2
type red_block = Red2 | Red1

type red_tower = TopRed of red_block * blue_tower | RedBottom
and blue_tower = TopBlue of blue_block * red_tower | BlueBottom

type tower = Red of red_tower | Blue of blue_tower

let rec add_red red_block = List.map(fun t -> TopRed(red_block, t))
let rec add_blue blue_block = List.map(fun t -> TopBlue(blue_block, t))
let enumerate_towers višina = 
   let rec redtop višina=
   if višina <0 then []
   else if višina = 0 then [RedBottom]
   else
      add_red Red1 (bluetop (višina -1))
      @ add_red Red2 (bluetop (višina -2))
      and bluetop višina=
      if višina < 0 then []
      else if višina = 0 then [BlueBottom]
      else
         add_blue Blue2 (redtop (višina-2))
         @ add_blue Blue3 (redtop (višina -3))
      in List.map (fun t -> Red t )(redtop višina) @ List.map (fun t -> Blue t) (bluetop višina)

(*----------------------------------------------------------------------------*]
 Vdrli ste v tovarno čokolade in sedaj stojite pred stalažo kjer so ena ob
 drugi naložene najboljše slaščice. Želite si pojesti čim več sladkorja, a
 hkrati poskrbeti, da vas ob pregledu tovarne ne odkrijejo. Da vas pri rednem
 pregledu ne odkrijejo, mora biti razdalija med dvema zaporednima slaščicama,
 ki ju pojeste vsaj `k`.

 Napišite funkcijo [ham_ham], ki sprejme seznam naravnih števil dolžine `n`, ki
 predstavljajo količino sladkorja v slaščicah v stalaži in parameter `k`,
 najmanjšo razdalijo med dvema slaščicama, ki ju še lahko varno pojeste.
 Funkcija naj vrne seznam zastavic `bool`, kjer je `i`-ti prižgan natanko tedaj
 ko v optimalni požrtiji pojemo `i`-to slaščico.

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # ham_ham test_shelf 1;;
 - : bool list = [false; true; false; true; false; true; false; true; false]
 # ham_ham test_shelf 2;;
 - : bool list = [false; true; false; false; false; true; false; false; false]
[*----------------------------------------------------------------------------*)

let test_shelf = [1;2;-5;3;7;19;-30;1;0]
