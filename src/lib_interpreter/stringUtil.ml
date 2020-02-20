(* template for string_of_sequence like functions. *)
let string_of_seq_tpl : 
  (string -> string) -> 
  ('a -> 'b -> string) -> 
  string -> 
  'b ->
  ('a list -> string)
  =
  fun lastApplyFunc stringFunc delimeter extraArg ->
  let rec bodyConsFunc : 'a list -> string = begin
    function
    | [] -> ""
    | h :: [] -> stringFunc h extraArg
    | h :: tl -> stringFunc h extraArg ^ delimeter ^ bodyConsFunc tl
  end in
  fun alist -> lastApplyFunc (bodyConsFunc alist)
