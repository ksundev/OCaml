let rec lany lst p =
  match lst with
  | [] -> false
  | hd :: tl -> p hd || lany tl p