(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)
let rec last lst =
  match lst with
  | [] -> None
  | (fst :: []) -> Some fst
  | (_:: rest) -> last rest

let last' lst =
  let length = List.length lst in
  if length == 0 then None else Some (List.nth lst (length - 1));;

let rec last_sol = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last_sol t


let%test "last - normal input" = last ["a" ; "b" ; "c" ; "d"] = Some "d"
let%test "last - empty array" = last [] = None

let%test "last' - normal input" = last' ["a" ; "b" ; "c" ; "d"] = Some "d"
let%test "last' - empty array" = last' [] = None

let%test "last_sol - normal input" = last_sol ["a" ; "b" ; "c" ; "d"] = Some "d"
let%test "last_sol - empty array" = last_sol [] = None
