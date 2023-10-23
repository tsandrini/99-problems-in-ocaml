(* 22. Create a list containing all integers within a given range. (easy) *)
(* If first argument is greater than second, produce a list in decreasing order. *)

let range start_pos end_pos =
  let parity = if start_pos < end_pos then 1 else (-1) in
  let rec aux acc = function
    | x when parity * x <= parity * end_pos -> aux (x :: acc) (x + parity)
    | _ -> acc in
  aux [] start_pos |> List.rev


let%test "range - positive increment" = range 4 9 = [4; 5; 6; 7; 8; 9]
let%test "range - negative increment" = range 9 4 = [9; 8; 7; 6; 5; 4]
