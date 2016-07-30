(* shell scripts *)

(* 
#use "myfile.ml";;
*)

#require "core_extended";;

open Core_extended.Std

let my_host =
    let sh = Shell.sh_one_exn in
    { hostname   = sh "hostname";
      os_name    = sh "uname -s";
      cpu_arch   = sh "uname -p";
      timestamp  = Time.now ();
    }

let () =
	print_string my_host
