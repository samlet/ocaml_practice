open Core.Std

let square x = x * x ;;
square 2;;
square (square 2);;

let ratio x y =
     Float.of_int x /. Float.of_int y
  ;;
ratio 4 7;;

(*下面的例子是一个接收3个参数的函数：一个测试函数和两个整数参数。这个函数返回可以通过测试函数的两个整数参数之和。*)
let sum_if_true test first second =
    (if test first then first else 0)
    + (if test second then second else 0)
  ;;
let even x =
    x mod 2 = 0 ;;
sum_if_true even 3 4;;
sum_if_true even 2 4;;

(*带类型标注版本的sum_if_true：*)
let sum_if_true (test : int -> bool) (x : int) (y : int) : int =
     (if test x then x else 0)
     + (if test y then y else 0)
  ;;


let rec range a b =
  if a > b then []
  else a :: range (a+1) b

(*test参数的类型是('a -> bool)，表示test是一个单参数函数，返回值是bool型，参数可以任何类型'a。但是，无论'a是什么类型,都要和其它两个参数以及first_if_true返回值类型相同。这种泛化叫作参数多态*)
let first_if_true test x y =
    if test x then x else y
  ;;
let long_string s = String.length s > 6;;
first_if_true long_string "short" "loooooong";;
let big_number x = x > 3;;
first_if_true big_number 4 3;;


(*
在函数式语言中, 函数（functions）是一等公民。
听上去不是很有用，让我们来看个例子。*)
(*
定义了一个嵌套函数double，它读入一个参数x后返回x * 2。然后map在给定的列表([1; 2; 3])的每个元素上调用double来生成结果：一个每个数都扩大一倍的新的列表。

map被称为高阶函数（higher-order function） (HOF)。高阶函数是指一个把其他函数作为参数之一的函数。
*)
# let double x = x * 2 in
  List.map double [ 1; 2; 3 ];;

(*闭包是那些带着它们被定义时的环境的函数。特别的，一个闭包可以引用它定义时存在的变量。让我们把上面那个函数变得更通用一些，以便我们可以对任何整数列表乘以一个任意值n:
*)
let multiply n list =
    let f x =
      n * x in
    List.map f list;;

(*
# multiply 2 [1; 2; 3];;
- : int list = [2; 4; 6]
# multiply 5 [1; 2; 3];;
- : int list = [5; 10; 15]
*)

(*部分函数应用（Partial function applications）和 currying（科里化）
*)
let plus a b =
    a + b;;
(*# plus 2;;
- : int -> int = <fun>
这不是一个错误。它告诉我们plus 2事实上也是一个函数。它以一个整数为参数并返回一个整数。*)

let f = plus 2;;
f 10;;
(*
# let f = plus 2;;
val f : int -> int = <fun>
# f 10;;
- : int = 12
# f 15;;
- : int = 17
# f 99;;
- : int = 101
*)


let multiply n list =
    let f x =
      n * x in
    List.map ~f:f list;;
let double = multiply 2;;
let triple = multiply 3;;
(*
它们确实是函数, 不信你看:
# double [1; 2; 3];;
- : int list = [2; 4; 6]
# triple [1; 2; 3];;
- : int list = [3; 6; 9]
*)

(*
你也可以不用中间函数f，而象这样来直接用部分应用（partial application）：
(( * ) n)是一个* (乘)函数的部分应用。 注意这里额外的空格，它使得OCaml不会认为( *是注释的开始。
*)
let multiply n = List.map ~f:(( * ) n);;
let double = multiply 2;;
let triple = multiply 3;;

let plus = ( + );;
plus 2 3;;

List.map ~f:(plus 2) [1; 2; 3];;
let list_of_functions = List.map ~f:plus [1; 2; 3];;


let () =
	print_string "Hello world!\n"

(*对于一个非懒惰的语言，参数和函数总是在使用前被求值，然后再传入到函数中。比如 下面的代码会引起除零错误：
*)
let give_me_a_three _ = 3;;
give_me_a_three (1/0);;
(*Exception: Division_by_zero.*)

(*OCaml是一个非懒惰的语言，但是Lazy模块允许你写懒惰的表达式，下面就是这样一个例子：
# let lazy_expr = lazy (1/0);;
*)
let lazy_expr = lazy (1/0);;
give_me_a_three lazy_expr;;
(*如果要求值，我们要用Lazy.force函数：*)
Lazy.force lazy_expr;;





