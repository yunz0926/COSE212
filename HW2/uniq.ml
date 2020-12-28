let rec duplicated : 'a -> 'a list -> 'a list
= fun element lst ->
  match lst with
  |[] -> []
  |hd::tl -> if(hd = element) then (duplicated element tl) else hd::(duplicated element tl);; 

let rec uniq : 'a list -> 'a list
= fun lst ->
  match lst with
  |[] -> []
  |hd::tl -> hd::(uniq(duplicated hd tl));;