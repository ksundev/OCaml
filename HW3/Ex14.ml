type talkingto = (string * string) list

let party : talkingto = [("A", "B"); ("B", "A"); ("A", "D"); ("B", "C"); ("C", "E")]

let rec infected (party : talkingto) (infected_person : string) (target_person : string) : bool =
  
  let rec direct_infect infected target lst =
    match lst with
    |[] -> false
    |(p1, p2)::rest ->
        if p1=infected && p2=target
        then true
        else direct_infect infected target rest
  in 
  let rec indirect_infect infected target lst =
    match lst with
    |[] -> false
    |(p1, p2)::rest ->
        if p1=infected && direct_infect p2 target party then
          true
        else indirect_infect infected target rest
  in
  let rec all_possible_infections infected target lst =
    match lst with
    |[] -> false
    |(p1, p2)::rest ->
        if p1=infected && (direct_infect p2 target party || indirect_infect p2 target party) then
          true
        else all_possible_infections infected target rest
  in
  if direct_infect infected_person target_person party || indirect_infect infected_person target_person party then true
  else all_possible_infections infected_person target_person party;;
