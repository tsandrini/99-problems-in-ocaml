(* 15. Replicate the elements of a list a given number of times. (medium) *)

let replicate lst num =
  let rec replElem elem = function
    | 1 -> [elem]
    | c -> elem :: (replElem elem (c - 1)) in
  let rec aux = function
    | [] -> []
    | hd :: tl -> (replElem hd num) @ (aux tl) in
  aux lst

let replicate' lst num =
  let rec repeatPrepend elem acc = function
    | 1 -> elem :: acc
    | n -> repeatPrepend elem (elem :: acc) (n - 1) in
  let rec aux acc = function
    | []  -> acc
    | hd :: tl -> aux (repeatPrepend hd acc num) tl in
  aux [] lst |> List.rev

let replicate'' lst num =
  let rec repeatPrepend elem acc = function
    | 1 -> elem :: acc
    | n -> repeatPrepend elem (elem :: acc) (n - 1) in
  List.fold_left (fun acc elem -> repeatPrepend elem acc num) [] lst
  |> List.rev

let%test "replicate - normal input" =
  replicate ["a";"b";"c"] 3 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
let%test "replicate' - normal input" =
  replicate' ["a";"b";"c"] 3 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
let%test "replicate'' - normal input" =
  replicate'' ["a";"b";"c"] 3 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
