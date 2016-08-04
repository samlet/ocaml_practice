(*OCaml中最简单的可变数据结构就是数组。OCaml中的数组和其它语言（如C）中的非常相似：索引从0开始，访问和修改数组元素的时间复杂度是常数级的。数组比OCaml中包括列表在内的其它数据结构的内存利用都紧凑。*)
(*.(i)语法用来引用一个数组元素，<-语法用以修改。因为数组元素从0开始计数，所以.(2)是第三个元素。*)
let numbers = [| 1; 2; 3; 4 |];;
numbers.(2) <- 4;;

(*记录默认是不可变的，但是其中的一些字段可以显式声明成可变的。*)
type running_sum =
   { mutable sum: float;
     mutable sum_sq: float; (* sum of squares *)
     mutable samples: int;
   }
  ;;
let mean rsum = rsum.sum /. float rsum.samples
  let stdev rsum =
     sqrt (rsum.sum_sq /. float rsum.samples
           -. (rsum.sum /. float rsum.samples) ** 2.) ;;

(*create返回一个和空集相关的running_sum，update rsum x通过更新样本数、和以及平方和来修改rsum，以反映将x添加到样本集合中。
*)
let create () = { sum = 0.; sum_sq = 0.; samples = 0 }
  let update rsum x =
     rsum.samples <- rsum.samples + 1;
     rsum.sum     <- rsum.sum     +. x;
     rsum.sum_sq  <- rsum.sum_sq  +. x *. x
  ;;

let rsum = create ();;
List.iter [1.;3.;2.;-7.;4.;5.] ~f:(fun x -> update rsum x);;
mean rsum;;
stdev rsum;;

(*我们可以使用ref创建一个单独的可变值。ref类型是标准库中预定义的，并没有什么特别的，它只是一个普通的记录类型，拥有一个名为contents的单独的可变字段。
*)
let x = { contents = 0 };;
x.contents <- x.contents + 1;;
x;;

let x = ref 0  (* create a ref, i.e., { contents = 0 } *) ;;
!x             (* get the contents of a ref, i.e., x.contents *) ;;
x := !x + 1    (* assignment, i.e., x.contents <- ... *) ;;
!x ;;

(*这些操作符也没有什么神奇的。你完全可以用几行代码重新实现ref类型和所有这些操作符。
*)
(*ref前面的'a表示ref类型是多态的，和列表多态一样，指可以持有任何类型的值。!和:=周围的括号是必须的，因为它们是操作符，而不是普通函数。
*)
type 'a ref = { mutable contents : 'a }

  let ref x = { contents = x }
  let (!) r = r.contents
  let (:=) r x = r.contents <- x
  ;;

(*我们可以命令式求列表元素的和，使用一个ref来累加结果。*)
let sum list =
    let sum = ref 0 in
    List.iter list ~f:(fun x -> sum := !sum + x);
    !sum
  ;;

(*OCaml也支持传统命令式的控制流概念，如for和while循环。下面的例子中使用for循环来重排数组。我们使用Random模块作为随机源。Random从一个默认种子开始，但是你也可以调用Random.self_init来选择一个新的随机种子。
*)
let permute array =
    let length = Array.length array in
    for i = 0 to length - 2 do
       (* pick a j that is after i and before the end of the array *)
       let j = i + 1 + Random.int (length - i - 1) in
       (* Swap i and j *)
       let tmp = array.(i) in
       array.(i) <- array.(j);
       array.(j) <- tmp
    done
  ;;
let ar = Array.init 20 ~f:(fun i -> i);;
permute ar;;
ar;;

(*OCaml也支持while循环，下面的函数中会展示这一点，这个函数用以查找数组中第一个负数的位置。注意while（和for一样）也是一个关键字。
*)
let find_first_negative_entry array =
     let pos = ref 0 in
     while !pos < Array.length array && array.(!pos) >= 0 do
       pos := !pos + 1
     done;
     if !pos = Array.length array then None else Some !pos
  ;;
find_first_negative_entry [|1;2;0;3|];;
find_first_negative_entry [|1;-2;0;3|];;


