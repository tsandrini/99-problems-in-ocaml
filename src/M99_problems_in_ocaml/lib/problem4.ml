(* 4. Find the number of elements of a list. (easy) *)
let rec length lst =
  match lst with
  | [] -> 0
  | (_:: []) -> 1
  | (_:: rest) -> 1 + (length rest)

let length' lst =
  List.fold_right (fun _ len -> len + 1) lst 0


let%test "length - normal input" = length ["a"; "b"; "c"] = 3
let%test "length - empty list" = length [] = 0

let%test "length' - normal input" = length' ["a"; "b"; "c"] = 3
let%test "length' - empty list" = length' [] = 0
