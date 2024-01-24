type player = White | Black

type game_tree =
  | Winner of player
  | Tie
  | Decision of player * (float * game_tree) list

let primer =
  Decision
    ( White,
      [
        (0.3, Decision (Black, [ (0.5, Winner White); (0.5, Winner Black) ]));
        (0.7, Decision (Black, [ (0.5, Tie); (0.5, Winner Black) ]));
      ] )

(* 1. a) *)
let prestej_zmage tree =
  let rec prestej_aux acc = function
    | Winner White -> 1 + acc
    | Winner Black | Tie -> acc
    | Decision (_, moznosti) ->
      List.fold_left (fun acc (_, poddrevo) -> acc + prestej_aux 0 poddrevo) acc moznosti
  in
  prestej_aux 0 tree
   


(* 1. b) *)
type result = { white_wins : float; black_wins : float; ties : float }
let rec rezultat drevo = 
let zacetni_rez = {white_wins = 0.0 ; black_wins = 0.0; ties = 0.0} in
match drevo with
  | Winner White -> {zacetni_rez with white_wins= 1.0}
  | Winner Black -> {zacetni_rez with black_wins = 1.0}
  | Tie -> { zacetni_rez with ties = 1.0}
  | Decision (_, moznosti) -> 
    
(* 1. c) *)

(* 1. d) *)
