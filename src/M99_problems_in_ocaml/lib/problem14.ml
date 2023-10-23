(* 14. Duplicate the elements of a list. (easy) *)

let rec duplicate = function
  | [] -> []
  | x :: tl -> x :: x :: duplicate tl

let duplicate' lst =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (hd :: hd :: acc) tl in
  aux [] lst |> List.rev

let duplicate'' lst =
  List.fold_left (fun acc x -> x :: x :: acc) [] lst

let%test "duplicate - normal input" = duplicate ["a"; "b"; "c"; "c"; "d"] = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
let%test "duplicate' - normal input" = duplicate' ["a"; "b"; "c"; "c"; "d"] = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
