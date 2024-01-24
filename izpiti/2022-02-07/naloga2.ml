type 'a operator = 'a -> 'a -> 'a

type 'a atom = Value of 'a | Operator of 'a operator

type 'a term = 'a atom list

type 'a result = Finished of 'a | Term of 'a term | Error

let plus = Operator ( + )

let krat = Operator ( * )

let deljeno = Operator ( / )
let deljeno_f = Operator(/.)
let krat_f = Operator( *.)
let minus = Operator (-)

let primer : int term = [ Value 3; Value 4; plus; Value 5; deljeno ]    

(* 2. a) *)


let primer1 : int term = [Value 1; Value 2; plus; Value 4; minus ; Value 5; krat]

let primer2 : float term = [Value 5.3; Value 4.6; deljeno_f; Value 1.7; krat_f]

let napacni_primer = [Value 3; Value 3; plus; plus; Value 3]

(* 2. b) *)

let korak = function
|[] -> Error
|[Value x] -> Finished x
| Value x :: Value y ::Operator op:: preostanek -> 
  let result = op x y in 
  Term (Value result :: preostanek)
| _ -> Error

(* 2. c) *)

let rec izvedi = function
|[] -> None
|[Value x] -> (Some x)
| Value x :: Value y :: Operator op :: preostanek ->
  let result = op x y in 
  izvedi ( Value result :: preostanek)
| _ -> None


(* 2. d) *)


(* 2. e) *)

let combine _ = failwith "TODO"
