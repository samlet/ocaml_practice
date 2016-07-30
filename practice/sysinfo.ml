(* sysinfo *)
(* #use "sysinfo.ml";; *)

#require "core_extended"

open Core.Std
open Core_extended.Std

let sh = Shell.sh_one_exn
let print_hostname = 	
	let hostname   = sh "hostname" in
	hostname

let () =
	printf "host name is %s \n" print_hostname

