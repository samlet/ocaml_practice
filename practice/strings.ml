(*
	^操作符会连接两个字符串
*)

let string_of_float f =
  let s = format_float "%.12g" f in
  if String.contains s '.' || String.contains s 'e'
  then s
  else s ^ "."

(*定义一个表达式类型*)
type expr =
    | Plus of expr * expr        (* means a + b *)
    | Minus of expr * expr       (* means a - b *)
    | Times of expr * expr       (* means a * b *)
    | Divide of expr * expr      (* means a / b *)
    | Value of string            (* "x", "y", "n", etc. *);;

(* 我希望表示一个数学表达式 n * (x + y) 然后分解公因式 n * x + n * y *)
(* 需要一个函数来把 Times (Value "n", Plus (Value "x", Value "y")) 打印成 n * (x + y)。下面我将写两个函数，一个会把表达式转变成可读的字符串，而一个将其打印出来 （原因是我可以把字符串写到文件而不局限于标准输出）*)
let rec to_string e =
    match e with
    | Plus (left, right) ->
       "(" ^ to_string left ^ " + " ^ to_string right ^ ")"
    | Minus (left, right) ->
       "(" ^ to_string left ^ " - " ^ to_string right ^ ")"
    | Times (left, right) ->
       "(" ^ to_string left ^ " * " ^ to_string right ^ ")"
    | Divide (left, right) ->
       "(" ^ to_string left ^ " / " ^ to_string right ^ ")"
    | Value v -> v;;

let print_expr e =
    print_endline (to_string e);;

print_expr (Times (Value "n", Plus (Value "x", Value "y")));;
(* (n * (x + y))
 *)

let () =
	print_string "Hello world!\n";;

