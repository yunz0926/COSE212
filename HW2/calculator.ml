type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  
let rec calculatesigma : int -> exp -> int
= fun a exp ->
  match exp with
  |X -> a
  |INT(x) -> x
  |ADD(e1, e2) -> (calculatesigma a e1) + (calculatesigma a e2)
  |SUB(e1, e2) -> (calculatesigma a e1) - (calculatesigma a e2)
  |MUL(e1, e2) -> (calculatesigma a e1) * (calculatesigma a e2)
  |DIV(e1, e2) -> (calculatesigma a e1) / (calculatesigma a e2);;

let rec calculator : exp -> int
= fun exp -> 
  match exp with
  |INT(a) -> a
  |ADD(e1, e2) -> (calculator e1) + (calculator e2)
  |SUB(e1, e2) -> (calculator e1) - (calculator e2)
  |MUL(e1, e2) -> (calculator e1) * (calculator e2)
  |DIV(e1, e2) -> (calculator e1) / (calculator e2)
  |SIGMA(e1, e2, e3) -> if((calculator e1)<=(calculator e2)) then (calculatesigma (calculator e1) e3) + calculator (SIGMA (ADD(e1, INT(1)), e2, e3)) else 0;;
