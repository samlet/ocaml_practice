(*一个给定的模块也可以在一个文件中显式地定义，成为当前模块的一个子模块。让我们来看看example.ml文件：*)

module Hello = struct
  let message = "Hello"
  let hello () = print_endline message
end
let goodbye () = print_endline "Goodbye"
let hello_goodbye () =
  Hello.hello ();
  goodbye ()

(*从另一个文件中可以看出，很明显我们有两个层次的模块。我们可以这样写：*)
let () =
  Example.Hello.hello ();
  Example.goodbye ()

