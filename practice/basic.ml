
open Printf

(* 输入两个浮点数后计算它们的平均值 *)
let average a b =
  (a +. b) /. 2.0

let positive_sum a b = 
  let a = max a 0
  and b = max b 0 in
  a + b

let my_data = [ "a"; "beautiful"; "day" ]

(*
	下面的两段代码返回同样的值（(a+b) + (a+b)^2^）
	后者可能快一些（但大多数编译器应该可以做到“消除公共子表达式”。），而且可读性更强。后者中的x只是a +. b的缩略名称.
*)
let f a b =
  (a +. b) +. (a +. b) ** 2.
  ;;

let f a b =
  let x = a +. b in
  x +. x ** 2.
  ;;

(*一个一般的pointer类型可以用来定义一个指针，而一个指针的值不是null就是某个内存地址：
# type 'a pointer = Null | Pointer of 'a ref;;
type 'a pointer = Null | Pointer of 'a ref
显式的解引用和指针赋值很容易就可以定义。我们定义一个前缀操作符 !^来解引用，和一个中缀操作符 ^:= 来赋值。
*)
type 'a pointer = Null | Pointer of 'a ref;;

let ( !^ ) = function
    | Null -> invalid_arg "Attempt to dereference the null pointer"
    | Pointer r -> !r;;

let ( ^:= ) p v =
    match p with
    | Null -> invalid_arg "Attempt to assign the null pointer"
    | Pointer r -> r := v;;

let new_pointer x = Pointer (ref x);;
(*我们可以定义一个指向整数的指针*)
let p = new_pointer 0;;
p ^:= 1;;
!^p;;

(* 现在我们可以如一般的指令式语言用指针定义链表：*)
(* The list type ``à la Pascal'' *)
type ilist = cell pointer
  and cell = {mutable hd : int; mutable tl : ilist};;
(* 然后我们定义链节的分配，链表的构造器和解构器*)
let new_cell () = {hd = 0; tl = Null};;
let cons x l =
    let c = new_cell () in
    c.hd <- x;
    c.tl <- l;
    (new_pointer c : ilist);;
let hd (l : ilist) = !^l.hd;;
let tl (l : ilist) = !^l.tl;;
(* Physical append *)
let append (l1 : ilist) (l2 : ilist) =
    let temp = ref l1 in
    while tl !temp <> Null do
      temp := tl !temp
    done;
    !^ !temp.tl <- l2;;

(* An example: *)
let l1 = cons 1 (cons 2 Null);;
let l2 = cons 3 Null;;
append l1 l2;;
(*这个操作引入了一个很糟糕的副作用：l1现在不止包括原来的元素，还包括了l2的。所以 从原来意义上的l1已经不存在了，可以认为append消费了第一个参数。换句话说，一个函数 调用的结果隐式地依赖于函数调用的历史。这个奇怪的行为引入了很多指针操作上的困难。*)



let () =
	let x = positive_sum 5 (-55) in
	printf "Hello world, %d!\n" x



