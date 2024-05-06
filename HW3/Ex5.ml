type circuit = 
  | IN
  | AND of circuit * circuit
  | OR of circuit * circuit;;

let rec and_depth circ =
  let max a b = if a >= b then a else b
  in
  match circ with
  |IN -> 0
  |AND (c1, c2) -> 1 + max (and_depth c1) (and_depth c2)
  |OR (c1, c2) -> max (and_depth c1) (and_depth c2);;