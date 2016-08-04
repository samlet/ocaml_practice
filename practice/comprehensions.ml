let my_list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];;

let is_even i =
    i mod 2 = 0 in
  List.filter ~f:is_even my_list;;

(*http://stackoverflow.com/questions/33808179/ocaml-list-mem-to-search-element-in-list-of-pair
	You can use List.exists, which takes a predicate function and returns true or false whether an element of the list matches or not:
*)
let list  = [(1,2);(2,3);(1,4);(5,0)];;
let exists_left list value = List.exists ~f:(fun (x, _) -> x = value) list;;
exists_left list 1;;
exists_left list 100;;

let () =
	print_string "Hello world!\n";;




