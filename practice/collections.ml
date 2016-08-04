open Core.Std

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

(*新的数据类型*)
(*point2d是一个记录类型，你可以把记录想成是一个元组，但每个字段都有命名，而不是按位置区分。*)
type point2d = { x : float; y : float };;
let p = { x = 3.; y = -4. };;
(*用模式匹配访问这些类型的内容：这里模式匹配把x_pos变量绑定到x字段的值，把y_pos变量绑定到y字段的值。*)
let magnitude { x = x_pos; y = y_pos } =
    sqrt (x_pos ** 2. +. y_pos ** 2.);;
magnitude p;;
(*我们可以一种 字段名双关(field punning)技术把上面的代码写得更精炼，字段名和其绑定的变量名在匹配中一定会一致，这样我们就不用两个都写了。使用这种技术，magnitude函数可以像下面这样重写。
*)
let magnitude { x; y } = sqrt (x ** 2. +. y ** 2.);;

(*也可以使用点号访问记录的字段：*)
let distance v1 v2 =
     magnitude { x = v1.x -. v2.x; y = v1.y -. v2.y };;

type pair_of_ints = { a : int; b : int };;
{a=3; b=5};;

(*可以在更大的类型中使用新创建的类型*)
type circle_desc  = { center: point2d; radius: float }
  type rect_desc    = { lower_left: point2d; width: float; height: float }
  type segment_desc = { endpoint1: point2d; endpoint2: point2d } ;;

(*现在想象一下你需要把这些类型的多个物体组合在一起作为一个多物体场景的描述。你需要一些统一的方法将这些物体用一种类型表示。变体类型是实现这种需求的一个方法：
*)
type scene_element =
    | Circle  of circle_desc
    | Rect    of rect_desc
    | Segment of segment_desc
  ;;

(*变体的不同情况用|分开（第一个|是可选的），并一种情况都有一个大写字母开头的标签以彼此区分，像Circle、Rect和Segment。
现在来看看我们如何写一个函数来测试一个点是否在一个sense_element列表的一些元素内部。
*)
(*调用List.exists时我们首次使用了 匿名函数。匿名函数使用fun关键字声明，不需要显式命名。这种函数在OCaml中很常用，特别是在使用List.exists这种迭代函数时。
*)
let is_inside_scene_element point scene_element =
     match scene_element with
     | Circle { center; radius } ->
       distance center point < radius
     | Rect { lower_left; width; height } ->
       point.x    > lower_left.x && point.x < lower_left.x +. width
       && point.y > lower_left.y && point.y < lower_left.y +. height
     | Segment { endpoint1; endpoint2 } -> false
  ;;
let is_inside_scene point scene =
     List.exists scene
       ~f:(fun el -> is_inside_scene_element point el)
   ;;
is_inside_scene {x=3.;y=7.}
    [ Circle {center = {x=4.;y= 4.}; radius = 0.5 } ];;
is_inside_scene {x=3.;y=7.}
    [ Circle {center = {x=4.;y= 4.}; radius = 5.0 } ];;


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

(*一个统计stdin读入的行频率计数的工具，最后输出计数最大的前10行。用了两个List.Assoc模块中的函数，该模块提供了操作关联列表的函数，关联列表即键/值对列表。这里，我们使用List.Assoc.find函数来在关联列表查找一个键值，用List.Assoc.add函数向关联列表中添加一对新的绑定*)
let assoc = [("one", 1); ("two",2); ("three",3)] ;;
List.Assoc.find assoc "two" ;;
List.Assoc.add assoc "four" 4 (* add a new key *) ;;
List.Assoc.add assoc "two"  4 (* overwrite an existing key *) ;;
(*List.Assoc.add不修改原列表，会分配一个包含所添加的键/值对的新列表。*)

let build_counts () =
  In_channel.fold_lines stdin ~init:[] ~f:(fun counts line ->
    let count =
      match List.Assoc.find counts line with
      | None -> 0
      | Some x -> x
    in
    List.Assoc.add counts line (count + 1)
  )

let () =
  build_counts ()
  |> List.sort ~cmp:(fun (_,x) (_,y) -> Int.descending x y)
  |> (fun l -> List.take l 10)
  |> List.iter ~f:(fun (line,count) -> printf "%3d: %s\n" count line)

(*Map创建一种映射关系。比方说，我们需要关联一组用户及其对应的密码，这里存在从用户到密码的 映射关系，可以通过Map模块通过函数式的方式相当迅速地来处理。在下面的例子中我将创建一个从 字符串到字符串的映射，当然这只是例子，映射的类型是很随意的。
*)
module MyUsers = Map.Make(String);;
let m = MyUsers.empty;;
(*m是一个全新的映射，因此前一个m已经被隐藏掉。这个m比前一个多了用户 "fred" 和他的密码 "sugarplums"。*)
let m = MyUsers.add "fred" "sugarplums" m;;
let m = MyUsers.add "tom" "ilovelucy" m;;
let m = MyUsers.add "mark" "ocamlrules" m;;
let m = MyUsers.add "pete" "linux" m;;

(*WARN*)
let print_users key password =
    print_string(key ^ " " ^ password ^ "\n");;
MyUsers.iter ~f:print_users m;;
(*WARN*)
MyUsers.find "fred" m;;

module SS = Set.Make(String);;
let s = SS.empty;;
(*不过我们也可以选择创建一个只包含一个元素的集合：*)
let s = SS.singleton "hello";;
(*加入元素*)
let s =
    List.fold_right SS.add ["hello"; "world"; "community"; "manager";
                            "stuff"; "blue"; "green"] s;;

(* 打印出每个字符串并且在最后换行 *)
let print_set s = 
     SS.iter print_endline s;;

(*我们可以通过remove函数移除某个元素。但是当我们想移除很多元素的时候，我们更应该使用filter。 下面我们将filter掉长度大于5的字符串：*)
let my_filter str =
    String.length str <= 5;;
let s2 = SS.filter my_filter s;;

let s2 = SS.filter (fun str -> String.length str <= 5) s;;
(*想看看某个元素是否在集合内，我们可以这么做：*)
SS.mem "hello" s2;;
(*Set模块提供了很多集合论的一些操作，如并，交，差等。比如说我们可以作原集合与字符串长度小于5的集合的差*)
print_set (SS.diff s s2);;

(*Hashtbl模块实现了一个高效的，可变的查询表*)
let my_hash = Hashtbl.create
Hashtbl.add my_hash "h" "hello";
  Hashtbl.add my_hash "h" "hi";
  Hashtbl.add my_hash "h" "hug";
  Hashtbl.add my_hash "h" "hard";
  Hashtbl.add my_hash "w" "wimp";
  Hashtbl.add my_hash "w" "world";
  Hashtbl.add my_hash "w" "wine";;

Hashtbl.find my_hash "h";;
Hashtbl.find_all my_hash "h";;
Hashtbl.remove my_hash "h";;
Hashtbl.replace my_hash "t" "try";
  Hashtbl.replace my_hash "t" "test";
  Hashtbl.find_all my_hash "t";;

(*想知道 my_hash中是否存在某个字母的时候*)
Hashtbl.mem my_hash "h";;


