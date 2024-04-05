let rec is_member x lst =
  match lst with
  | [] -> false
  | hd :: tl -> if hd = x then true else is_member x tl

let deduplicate lst =
  let rec remove_duplicates seen = function
    | [] -> []
    | hd :: tl ->
        if is_member hd seen then
          remove_duplicates seen tl
        else
          hd :: remove_duplicates (hd :: seen) tl
  in
  remove_duplicates [] lst
