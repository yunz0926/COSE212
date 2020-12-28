type digit = ZERO | ONE
type bin = digit list

let rec binarytodecimal : bin-> int -> int
= fun a b ->
  match a with
  |[ZERO] -> b*2
  |[ONE] -> b*2 + 1
  |hd::tl -> if(hd = ONE) then (binarytodecimal tl (b*2 + 1)) else (binarytodecimal tl (b*2));;
  
let rec decimaltobinary : int -> bin
= fun a ->
  if(a/2 = 0) then (if(a mod 2 = 0) then [ZERO] else [ONE])
  else (decimaltobinary (a/2))@(if(a mod 2 = 0) then [ZERO] else [ONE]);;

let rec bmul : bin -> bin -> bin
= fun a b -> decimaltobinary ((binarytodecimal a 0) * (binarytodecimal b 0));;