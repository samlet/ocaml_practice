
open Printf

(*可选参数就像一个调用者可提供也可不提供的标签参数。可选参数使用和标签参数一样的语法进行传递，并且，和标签参数一样，顺序可任意。
下面的例子是一个字符串拼接函数，使用了一个可选的分隔符。此函数使用^操作符拼接一对字符串。
*)
let concat ?sep x y =
     let sep = match sep with None -> "" | Some x -> x in
     x ^ sep ^ y
  ;;
concat "foo" "bar"             (* without the optional argument *);;
concat ~sep:":" "foo" "bar"    (* with the optional argument    *);;

(*提供默认值的语法，使我们可以把代码写得更简捷。*)
let concat ?(sep="") x y = x ^ sep ^ y ;;

(*在后台，一个使用可选参数的函数，当调用者没有提供此参数时，函数会接收到一个None，否则会接收到Some。但是Some和None都不是调用者显式传递的。
但有时候你确实想传递Some或None。OCaml允许你这样做，只要使用?代替~来标注参数即可。因此，下面两种给concat传递sep参数的方法是等价的。
*)
concat ~sep:":" "foo" "bar" (* provide the optional argument *);;
concat ?sep:(Some ":") "foo" "bar" (* pass an explicit [Some] *);;
(*下面两种不指定sep调用concat的方法也是等价的。*)
concat "foo" "bar" (* don't provide the optional argument *);;
concat ?sep:None "foo" "bar" (* explicitly pass `None` *);;

(*我们可以使用?语法直接把uppercase_concat的可选参数传给concat。*)
(*现在，如果有人不指定sep调用uppercase_concat时，这时显式的None会传递给concat，从而由concat来决定默认值。*)
let uppercase_concat ?sep a b = concat ?sep (String.uppercase a) b ;;
uppercase_concat "foo" "bar";;
uppercase_concat "foo" "bar" ~sep:":";;

(*关于标签和可选参数有一个微妙的方面就是类型系统是如何推导它们的。考虑下面这个例子，用以计算一个有两个实数参数的函数的数值导数。它接收一个delta参数来确定计算导数的窗口大小，值x和y用以给出计算导数的点，还有一个要计算导数的函数f。函数f本身接收两个标签参数x和y。注意你可以在变量名中使用撇号，所以x'和y'只是普通变量。*)
let numeric_deriv ~delta ~x ~y ~f =
    let x' = x +. delta in
    let y' = y +. delta in
    let base = f ~x ~y in
    let dx = (f ~x:x' ~y -. base) /. delta in
    let dy = (f ~x ~y:y' -. base) /. delta in
    (dx,dy)
  ;;

(*我们可以提供明确的类型信息，以使OCaml可以接受f以不同的参数顺序调用。因此，下面的代码会编译无误，因为给出了f的类型注解。*)
let numeric_deriv ~delta ~x ~y ~(f: x:float -> y:float -> float) =
    let x' = x +. delta in
    let y' = y +. delta in
    let base = f ~x ~y in
    let dx = (f ~y ~x:x' -. base) /. delta in
    let dy = (f ~x ~y:y' -. base) /. delta in
    (dx,dy)
  ;;


(*在log_entry中我们首次在函数体中用let定义变量。let和in一起可以在包括函数体在内的任何局部作用域中引入新的绑定。in标志了新变量可以在其中使用的作用域的开头。因此，我们可以这样写：
*)
let x = 7 in
  x + x
  ;;
(*我们也可以在一行上有多个let，每个都会在前面的基础上添加一个新变量。
*)
let x = 7 in
  let y = x * x in
  x + y
  ;;



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



