type 'a btree = Leaf | Node of 'a * 'a btree * 'a btree

let rec count_leaves tree =
  match tree with
  | Leaf -> 1
  | Node (_, left, right) -> count_leaves left + count_leaves right