type 'a btree = Leaf | Node of 'a * 'a btree * 'a btree

let rec count_oddnode tree =
  match tree with
  | Leaf -> 0
  | Node (key, left, right) ->
      let count_left = count_oddnode left in
      let count_right = count_oddnode right in
      let count_key = if key mod 2 <> 0 then 1 else 0 in
      count_left + count_right + count_key
