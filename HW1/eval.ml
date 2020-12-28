type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp

and exp = 
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp
  
let rec calculate : exp -> int
= fun e -> match e with
  |Num n -> n
  |Plus (n1, n2) -> (calculate n1) + (calculate n2)
  |Minus (n1, n2) -> (calculate n1) - (calculate n2);;
  
let rec eval : formula -> bool
= fun f -> match f with
  |True -> true
  |False -> false
  |Not formula -> not (eval formula)
  |AndAlso (formula1, formula2) -> (eval formula1) && (eval formula2)
  |OrElse (formula1, formula2) -> (eval formula1) || (eval formula2)
  |Imply (formula1, formula2) -> if(eval formula1 && (not (eval formula2))) then false else true
  |Equal (exp1, exp2) -> if((calculate exp1) = (calculate exp2)) then true else false;;