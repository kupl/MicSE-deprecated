(*****************************************************************************)
(*****************************************************************************)
(* Command Line Arguments                                                    *)
(*****************************************************************************)
(*****************************************************************************)

(* input file names *)
let ifnames : string list ref = ref []

(* output file names *)
let ofname  : string ref = ref ""

(* time limits *)
let ttlimit : float ref = ref 60.0  (* seconds *)
let z3limit : string ref = ref "5000"   (* milliseconds *)


(*****************************************************************************)
(*****************************************************************************)
(* Argument Setting Functions                                                *)
(*****************************************************************************)
(*****************************************************************************)

(* default behavior *)
let set_ifnames s = ifnames := !ifnames @ [s]

let cmdParmas = [
  ("-o", Arg.Set_string ofname, "set output filename");
  ("-time", Arg.Set_float ttlimit, "set total time limit in seconds (float)");
  ("-z3time", Arg.Set_string z3limit, "set z3 solver time limit in milliseconds (unsigned int)");
]


(*******************************************************************)
(*******************************************************************)
(* Toplevel Run                                                    *)
(*******************************************************************)
(*******************************************************************)

let _ =
  let usage_msg = Printf.sprintf "Usage: %s <input-file>" Sys.argv.(0) in
  Arg.parse cmdParmas set_ifnames usage_msg;
  let _ : unit = 
    if List.length !ifnames = 0
    then (print_endline usage_msg; failwith "No Arguments";)
    else ()
  in
  (* program behavior *)
  print_endline "PROGRAM ENDS"