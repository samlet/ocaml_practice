(*模式匹配也可以出现在函数参数中。下面是一函数，用以计算两个点之间的距离，每个点用一对float表示。模式匹配语法可以让我们轻松获得需要的值。
*)
let distance (x1,y1) (x2,y2) =
    sqrt ((x1 -. x2) ** 2. +. (y1 -. y2) ** 2.)
  ;;

(*列表元素可以用模式匹配访问。列表模式基于这两个列表构造器：::和[]。*)
let my_favorite_language languages =
    match languages with
    | first :: the_rest -> first
    | [] -> "OCaml" (* A good default! *)
 ;;
my_favorite_language ["English";"Spanish";"French"];;
my_favorite_language [];;

(*递归函数，就是调用自身的函数，是OCaml以及所有函数式语言的重要技术。设计递归函数的典型方法是把逻辑分割成一些可以直接解决的 基本分支，和一些 归纳分支，归纳分支中把函数分割成更小的块，然后再调用自身来解决它们。
写递归列表的函数时，基本分支和归纳分支通常用模式匹配来分隔。下面是一个简单例子，一个求列表元素之和的函数。
*)
let rec sum l =
    match l with
    | [] -> 0                   (* base case *)
    | hd :: tl -> hd + sum tl   (* inductive case *)
  ;;
sum [1;2;3];;

(*更复杂的列表模式。下面是一个消除列表中连续重复的函数。
*)
let rec destutter list =
    match list with
    | [] -> []
    | [hd] -> [hd]
    | hd1 :: hd2 :: tl ->
      if hd1 = hd2 then destutter (hd2 :: tl)
      else hd1 :: destutter (hd2 :: tl)
  ;;
destutter ["hey";"hey";"hey";"man!"];;



type expr = Plus of expr * expr      (* means a + b *)
            | Minus of expr * expr     (* means a - b *)
            | Times of expr * expr     (* means a * b *)
            | Divide of expr * expr    (* means a / b *)
            | Product of expr list     (* means a * b * c * ... *)
            | Value of string          (* "x", "y", "n", etc. *);;

let factorize e =
    match e with
    | Plus (Times (e1, e2), Times (e3, e4)) when e1 = e3 ->
       Times (e1, Plus (e2, e4))
    | Plus (Times (e1, e2), Times (e3, e4)) when e2 = e4 ->
       Times (Plus (e1, e3), e4)
    | e -> e;;

factorize (Plus (Times (Value "n", Value "x"),
                   Times (Value "n", Value "y")));;
(*
- : expr = Times (Value "n", Plus (Value "x", Value "y"))
*)


