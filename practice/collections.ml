
[1; 2; 3];;
1 :: [2; 3];;

(*元组是值的有序集合，值的类型可以不同。你可以用逗号把值拼接起来创建元组。
*)
let a_tuple = (3,"three");;
let another_tuple = (3,"four",5.);;
(*你可以使用OCaml的模式匹配语法提取元组的元素，就像下面这样：
*)
let (x,y) = a_tuple;;
x + String.length y;;

(*使用列表你可以保存任意数量类型相同的元素
同一个列表中不能混合类型不同的值。
*)
let languages = ["OCaml";"Perl";"C"];;
List.length languages;;
List.map languages ~f:String.length;;
List.map ~f:String.length languages;;

(*除了使用方括号构造列表，你也可以使用::操作符向一个列表前面添加元素。
*)
"French" :: "Spanish" :: languages;;
(*这里我们创建了一个新的扩展列表，并没有改变开始的列表，如下所示：
# languages;;
*)

(*和其它语言不同，OCaml使用分号而不是逗号来分隔列表中的元素，逗号被用来分隔元组中的元素。如果你在列表中使用了逗号，代码也可以编译过，但和你的预期会大不相同。
# ["OCaml", "Perl", "C"];;
- : (string * string * string) list = [("OCaml", "Perl", "C")]
*)
(*即使没有括号包围，逗号也能创建元组。因此我们可以这样分配一个整数元组。
# 1,2,3;;
- : int * int * int = (1, 2, 3)
*)

(*方括号实际上是::的语法糖。因此下面的声明是等价的。注意[]用以表示空列表，::是右结合的。
# [1; 2; 3];;
- : int list = [1; 2; 3]
# 1 :: (2 :: (3 :: []));;
- : int list = [1; 2; 3]
# 1 :: 2 :: 3 :: [];;
- : int list = [1; 2; 3]
*)

(*@操作符可以用以连接两个列表。
# [1;2;3] @ [4;5;6];;
- : int list = [1; 2; 3; 4; 5; 6]
*)

type pair_of_ints = { a : int; b : int };;
{a=3; b=5};;

(*qualified unions 和枚举（enum）
*)
type foo =
    | Nothing
    | Int of int
    | Pair of int * int
    | String of string;;

(*
一个简单的C的枚举类型可以如下定义：
enum sign { positive, zero, negative };
而Ocaml的版本是：
*)
type sign = Positive | Zero | Negative

type binary_tree =
    | Leaf of int
    | Tree of binary_tree * binary_tree;;
(*下面是一些二叉树。试一下把他们画出来：
Leaf 3
Tree (Leaf 3, Leaf 4)
Tree (Tree (Leaf 3, Leaf 4), Leaf 5)
Tree (Tree (Leaf 3, Leaf 4), Tree (Tree (Leaf 3, Leaf 4), Leaf 5))
*)

(*多态变体可以如下实现：*)
# type 'a binary_tree =
    | Leaf of 'a
    | Tree of 'a binary_tree * 'a binary_tree;;

(*类型推导会自动为我们推导出叶节点的类型*)
Leaf "hello";;

(*链表就是参数化变体*)
type 'a equiv_list =
    | Nil
    | Cons of 'a * 'a equiv_list;;

Cons(1, Nil);;
Cons(1, Cons(2, Nil));;

(*如果必须对数据进行修改，OCaml的record可以定义可变域。这里，一个 元素可变的链表可以如下定义*)
type list = Nil | Cons of cell
  and cell = { mutable hd : int; tl : list };;


let () =
	print_string "Hello world!\n";;

