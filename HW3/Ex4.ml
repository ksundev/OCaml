let dsort l =
  let rec insert n lst =
    match lst with
    |[] -> [n]
    |hd::tl ->
        if n >= hd then n::lst
        else hd::(insert n tl)
  in
  let rec sortworker l' acc =
    match l' with
    |[] -> acc
    |hd::tl -> sortworker tl (insert hd acc)
  in sortworker l [];;