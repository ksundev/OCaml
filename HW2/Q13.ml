let rec powerset = function
  | [] -> [[]]
  | hd :: tl ->
      let ps_of_tl = powerset tl 
    in
      let rec prepend hd = function
        | [] -> []
        | subset :: rest -> (hd :: subset) :: prepend hd rest
      in
      ps_of_tl @ (prepend hd ps_of_tl)
