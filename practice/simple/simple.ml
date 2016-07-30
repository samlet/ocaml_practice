(*#use "simple.ml";;*)

open Core.Std
open Printf

let multiply n list =
    let f x = n * x in
    List.map ~f:f list

(*A very elegant solution, but unfortunately it's not tail recursive. So it will consume some (linear) amount of stack space, and might even crash for a very long list.*)
let rec flatten = function
    [] -> []
  | l::r -> l @ flatten r

(*Using the standard FP accumulator trick to get tail recursive behavior (as noted by Thomas)*)
let flatten2 ll =
    let rec go acc = function
    | [] -> List.rev acc
    | l :: r -> go (List.rev_append l acc) r
in
    go [] ll


let () = 
	let double = multiply 2 in
	let triple = multiply 3 in
	List.map ~f:double [[1;2;3];[3;4;5]]
	|> List.map ~f:triple
	|> flatten2
	|> List.iter ~f:(printf "%d days\n")


