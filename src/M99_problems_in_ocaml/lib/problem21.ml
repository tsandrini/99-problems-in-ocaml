(* 21. Insert an element at a given position into a list. (easy) *)

(* Start counting list elements with 0. If the position is larger or equal to *)
(*  the length of the list, insert the element at the end. (The behavior is *)
(*   unspecified if the position is negative.) *)

let insert_at elem at lst =
  let rec aux acc idx = function
    | [] when idx = at -> elem :: acc
    | [] -> acc
    | list when idx = at -> aux (elem :: acc) (idx + 1) list
    | hd :: tl -> aux (hd :: acc) (idx + 1) tl in
  aux [] 0 lst |> List.rev


let%test "insert_at - normal input" = insert_at "alfa" 1 ["a";"b";"c";"d"] = ["a"; "alfa"; "b"; "c"; "d"]
let%test "insert_at - input in middle" = insert_at "alfa" 3 ["a"; "b"; "c"; "d"] = ["a"; "b"; "c"; "alfa"; "d"]
let%test "insert_at - input at the end" = insert_at "alfa" 4 ["a"; "b"; "c"; "d"] = ["a"; "b"; "c"; "d"; "alfa"]
