let rec union l1 l2 =
  let rec isNotIn l a =
    match l with
    |[] -> true
    |hd::tl ->
        if hd = a then false else isNotIn tl a
  in
  match l1 with
  |[] -> l2
  |hd::tl ->
      if isNotIn l2 hd then hd::(union tl l2)
      else union tl l2;;