let mapn f n l =
  let rec iter f n x =
    if n = 0 then x
    else iter f (n-1) (f x)
  in
  let rec map f n l' acc =
    match l' with
    |[] -> acc
    |hd::tl -> (map f n tl (acc@[iter f n hd]))
  in map f n l [];;