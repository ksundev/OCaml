let rec powerset = function
  | [] -> [[]]
  | hd :: tl ->
      let ps_tl = powerset tl in
      let prepend_hd subset = hd :: subset in
      let subsets_with_hd = List.map prepend_hd ps_tl in
      ps_tl @ subsets_with_hd
