let npower x n =
  if n < 0 then
    failwith "n have to be a non-negative int"
  else if n = 0 then
    1.0
  else
    let rec power_worker tmp x n =
      if n = 0 then tmp
      else power_worker (tmp /. float_of_int x) x (n-1) in
      power_worker 1.0 x n