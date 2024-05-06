type 'a ntree = Leaf of 'a
              | Node of ('a ntree list);;

let findn t =
  let max a b =
    if a >= b then a else b
  in 
  let rec length l =
    let rec count acc = function
      |[] -> acc
      |_::tl -> count (acc + 1) tl
    in
    count 0 l
  in
  let rec countMaxChildren lst =
    match lst with
    |[] -> 0
    |Node hd::tl ->
        let countMaxTale = countMaxChildren tl in
        max (length hd) (countMaxTale)
    |Leaf _::tl -> countMaxChildren tl
  in
  match t with
  |Leaf _ -> 0
  |Node children -> countMaxChildren children;;