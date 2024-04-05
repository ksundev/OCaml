let rec lall lst p =
  match lst with
  | [] -> true
  | hd :: tl -> p hd && lall tl p