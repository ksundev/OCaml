type 'a ntree = Leaf of 'a
              | Node of ('a ntree list);;
let rec flatten t =
  let rec flatten_worker acc t' =
    match t' with
    |Leaf x -> acc@[x]
    |Node children -> flatten_children acc children
  and flatten_children acc = function
    |[] -> acc
    |hd::tl ->
        let acc' = flatten_worker acc hd in
        flatten_children acc' tl
  in
  flatten_worker [] t;;