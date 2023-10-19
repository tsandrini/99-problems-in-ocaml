(* 3. Find the K'th element of a list. (easy) *)
let rec at idx lst =
  match lst with
  | [] -> None
  | (fst :: rest) -> if idx == 1 then Some fst else at (idx - 1) rest


let%test "at - normal input" = at 3 ["a"; "b"; "c"; "d"; "e"] = Some "c"
let%test "at - wrong input" = at 3 ["a"] = None
