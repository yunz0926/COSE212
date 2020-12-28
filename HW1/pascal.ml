let rec pascal : int * int -> int
= fun (n, m) -> 
  if (m = 0) then 1 
  else if (n = m) then 1
  else pascal(n-1, m-1) + pascal(n-1, m);;