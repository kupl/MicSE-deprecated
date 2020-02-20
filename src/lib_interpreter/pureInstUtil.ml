open PureInst
open Predefined

exception NegativeInteger of string

let inat_int : int -> InstNat.t = 
  fun n -> 
  if n >= 0
  then InstNat.of_int n
  else raise (NegativeInteger "pureInstUtil.ml : inat_int")

let d_int : int -> PureInst.data = fun n -> D_Int (Z.of_int n)

