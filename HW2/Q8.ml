let rec duplicate lst =
  match lst with
  | [] -> []
  | hd :: tl -> hd :: hd :: duplicate tl