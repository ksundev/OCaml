type 'a btree = Leaf | Node of 'a * 'a btree * 'a btree

let rec insert_btree key tree =
  match tree with
  | Leaf -> Node (key, Leaf, Leaf)
  | Node (k, left, right) ->
      if key < k then
        Node (k, insert_btree key left, right)
      else if key > k then
        Node (k, left, insert_btree key right)
      else
        tree