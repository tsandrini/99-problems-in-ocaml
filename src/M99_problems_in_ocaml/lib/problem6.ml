(* 6. Find out whether a list is a palindrome. (easy) *)
let ( <*> ) f g x = f x (g x)

let rev lst =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (hd :: acc) tl
  in
  aux [] lst

let is_palindrome lst = lst = rev lst

(*pointfree style*)
let is_palindrome_pf = (=) <*> rev

let%test "is_palindrome - true" = is_palindrome ["x"; "a"; "m"; "a"; "x"] = true
let%test "is_palindrome - true" = (not (is_palindrome ["a"; "b"])) = true

let%test "is_palindrome_pf - true" = is_palindrome_pf ["x"; "a"; "m"; "a"; "x"] = true
let%test "is_palindrome_pf - true" = (not (is_palindrome_pf ["a"; "b"])) = true
