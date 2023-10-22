(* 7. Flatten a nested list structure. (medium) *)
type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten = function
  | [] -> []
  | One x :: xs -> x :: flatten xs
  | Many x :: xs -> flatten x @ flatten xs


let%test "flatten - normal input" = flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]] = ["a"; "b"; "c"; "d"; "e"]
