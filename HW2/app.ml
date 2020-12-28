let rec duplicated : 'a -> 'a list -> bool
= fun element lst ->
  match lst with
  |[] -> false
  |hd::tl -> if(hd = element) then true else (duplicated element tl);; 

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  match l1 with 
  |[] -> l2
  |hd::tl -> if(duplicated hd l2) then app tl l2 else (app tl (l2@[hd]));; 