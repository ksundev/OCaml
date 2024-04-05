let rec replicate lst n =
  if n <= 0 then
    []
  else
    match lst with
    | [] -> []
    | hd :: tl -> replicate_worker hd n @ replicate tl n
and replicate_worker element n =
  if n <= 0 then
    []
  else
    element :: replicate_worker element (n - 1)