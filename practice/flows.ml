
let max a b =
    if a > b then a else b;;

(*结合递归函数，链表*)
let rec range a b =
    if a > b then []
    else a :: range (a+1) b;;

range 9 10

let f x y =
    x + if y > 0 then y else 0;;
let abs x =
    if x >= 0 then x else -x;;

(*
在 Pervasives 中的 string_of_float 函数也有一些复杂潜逃的if表达式：
*)    
let string_of_float f =
  let s = format_float "%.12g" f in
  let l = string_length s in
  let rec loop i =
    if i >= l then s ^ "."
    else if s.[i] = '.' || s.[i] = 'e' then s
    else loop (i+1) in
  loop 0

(*quit_loop 不是一个真正的变量，let 绑定只是让 quit_loop 成为 false 的简写。 这就是说while循环的条件语句总是会真的，因此这是一个死循环！*)
(*幸运的是OCaml是有引用的，所以我们可以写出想要的代码。在OCaml中是“解引用”的意思。你最好把 ! 读作“解引”*)
let quit_loop = ref false in
while not !quit_loop do
  print_string "Have you had enough yet? (y/n) ";
  let str = read_line () in
  if str.[0] = 'y' then
    quit_loop := true
done;;


(*如果你要迭代整个链表，是时候改变一下思维，不要老挂记着老朋友for循环了。OCaml有更好更快的方式 来迭代整个链表，这些函数都在List 模块里。在这个模块里有很多很好的函数，不过下面我只会讨论最 有用的一组。
*)
let my_list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];;
let f elem =
    Printf.printf "I'm looking at element %d now\n" elem in
    List.iter ~f:f my_list;;

List.map ~f:(( * ) 2) my_list;;


let () =
	print_string "Hello world!\n";;

