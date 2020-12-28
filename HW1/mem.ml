type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n t -> match t with
  |Empty -> false
  |Node(n2, bt1, bt2) -> if(n = n2) then true else (mem n bt1)||(mem n bt2);;
  