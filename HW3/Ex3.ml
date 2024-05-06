let rec alterSum l =
  let rec sumworker count sum l' =
    match l' with
    |[] -> sum
    |hd::tl ->
        if count = 0 then (sumworker (count+1) (sum+hd) tl)
        else if (count mod 2 = 1) then (sumworker (count+1) (sum+hd) tl)
        else sumworker (count+1) (sum-hd) tl
  in sumworker 0 0 l;;