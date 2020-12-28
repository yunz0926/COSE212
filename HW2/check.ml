type exp = 
  | V of var
  | P of var * exp
  | C of exp * exp
and var = string

let rec checkvar : exp * var -> bool
= fun (exp, var) -> match exp with
  |V(v) -> if (var = v) then true else false
  |P(_, e) -> checkvar (e, var)
  |C(e1, e2) -> checkvar(e1, var) || checkvar(e2, var);;

let rec check : exp -> bool
= fun exp -> match exp with
  |V(v) -> true
  |P(v, e) -> (checkvar(e, v)) && (check e)  
  |C(e1, e2) -> (check e1) && (check e2);;