(*read_and_accumulate是一个递归函数，用In_channel.input_line来按行读取标准输入，每次迭代都使用更新后的累加值sum调用自身。input_line返回一个option值，None表明输入流结束。
*)
open Core.Std

let rec read_and_accumulate accum =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> accum
  | Some x -> read_and_accumulate (accum +. Float.of_string x)

let () =
  printf "Total: %F\n" (read_and_accumulate 0.)

(*
	corebuild io_procs.native
*)

