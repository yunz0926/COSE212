type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) ->
  match exp with
  |Const(a) -> Const(0)
  |Var(var) -> if(var = x) then Const(1) else Var(var)
  |Power(var, a) -> if(var=x) then Times(Const(a)::[Power(var, a-1)]) else Power(var, a)
  |Times(hd::tl) -> (match hd with
                     |Const(a) -> Times(hd::[(diff (Times(tl), x))])
                     |_ -> diff (hd, x))
  |Sum(hd::tl) -> Sum((diff (hd, x))::[diff(Sum(tl), x)])
  |Sum([]) -> Const(0);;
  