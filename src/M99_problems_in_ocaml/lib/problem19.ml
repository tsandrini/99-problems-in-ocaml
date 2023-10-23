(* 19. Rotate a list N places to the left. (medium) *)

let rotate lst shift_in =
  let n = List.length lst in
  let shift = if shift_in >= 0 then shift_in else n + shift_in in
  let rec aux acc idx = function
    | [] -> acc
    | hd :: tl when idx >= shift && (idx - shift) < n -> aux (hd :: acc) (idx + 1) tl
    | _ :: tl -> aux acc (idx + 1) tl in
  aux [] 0 (lst @ lst) |> List.rev

let%test "rotate - positive input" =
  rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
let%test "rotate - negative input" =
  rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2) = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
