let rec min lst =
  match lst with
  | [] -> 0
  | [x] -> x
  | hd :: tl -> min_worker hd tl
and min_worker min rest =
  match rest with
  | [] -> min
  | hd :: tl -> min_worker (if hd < min then hd else min) tl