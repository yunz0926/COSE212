type btree = 
  | Leaf of int
  | Left of btree
  | Right of btree
  | LeftRight of btree * btree

let rec mirror : btree -> btree
= fun tree -> 
  match tree with
  |Leaf(int) -> Leaf(int)
  |Left(bt) -> Right(mirror bt)
  |Right(bt) -> Left(mirror bt)
  |LeftRight(bt1, bt2) -> LeftRight(mirror bt2, mirror bt1);;