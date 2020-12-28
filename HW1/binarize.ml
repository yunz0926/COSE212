let rec binarize : int -> int list
= fun n ->
  if (n / 2 = 1) then 1 :: (n mod 2) :: []
  else binarize(n/2)@(n mod 2)::[];;
