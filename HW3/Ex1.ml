let rec revrev t =
  let rec rev_inner l =
    match l with
    |[] -> []
    |hd::tl -> (rev_inner tl)@[hd]
  in
  match t with
  |[] -> []
  |hd::tl -> (revrev tl)@[(rev_inner hd)];;