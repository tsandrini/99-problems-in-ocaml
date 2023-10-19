(* 2. Find the last but one (last and penultimate) elements of a list. (easy) *)
let rec last_two lst =
  match lst with
  | [] -> None
  | (fst :: snd :: []) -> Some (fst, snd)
  | (_:: rest) -> last_two rest

let last_two' lst =
  let length = List.length lst in
  if length < 2 then None else Some (List.nth lst (length - 2), List.nth lst (length - 1));;

let rec last_two_sol = function
  | [] | [_] -> None
  | [x; y] -> Some (x,y)
  | _ :: t -> last_two_sol t;;

let%test "last_two - normal input" = last_two ["a"; "b"; "c"; "d"] = Some ("c", "d")
let%test "last_two - partial wrong input" = last_two ["a"] = None
let%test "last_two - empty array" = last_two [] = None

let%test "last_two' - normal input" = last_two' ["a"; "b"; "c"; "d"] = Some ("c", "d")
let%test "last_two' - partial wrong input" = last_two' ["a"] = None
let%test "last_two' - empty array" = last_two' [] = None

let%test "last_two_sol - normal input" = last_two_sol ["a"; "b"; "c"; "d"] = Some ("c", "d")
let%test "last_two_sol - partial wrong input" = last_two_sol ["a"] = None
let%test "last_two_sol - empty array" = last_two_sol [] = None
