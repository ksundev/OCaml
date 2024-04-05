let rec cartesian lst1 lst2 =
  let rec cartesian_worker x lst2 =
    match lst2 with
    | [] -> []
    | hd :: tl -> (x, hd) :: cartesian_worker x tl
  in
  match lst1 with
  | [] -> []
  | hd :: tl -> cartesian_worker hd lst2 @ cartesian tl lst2