let rec divide : int*int -> bool
= fun (n, m) ->
  if (m = 1) then true
  else if (n mod m = 0) then false
  else divide(n, m-1);;
  
let prime : int -> bool
= fun n -> 
  if(divide(n, n/2)) then true
  else false;;