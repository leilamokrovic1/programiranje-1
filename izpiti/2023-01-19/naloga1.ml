(* 1. a) *)
let permutacije a b c =
  [(a, b, c); (a, c, b); (b, a, c); (b, c, a); (c, a, b); (c, b, a)]
(* 1. b) *)

(* 1. c) *)

(* 1. d) *)

type response = Left | Middle | Right

(* 1. e) *)

type ('a, 'b) sum = Left of 'a | Right of 'b
