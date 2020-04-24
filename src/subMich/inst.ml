(*****************************************************************************)
(*****************************************************************************)
(* Instructions & Datas                                                      *)
(*****************************************************************************)
(*****************************************************************************)

type t = {
  t_v     : inst_i;
  t_annot : annot;
  t_loc   : int;    (* Assume that the code is not too long (smart contract cannot has over 2^62 instructions) *)
}

and inst_i = 
  (* Control Flow *)
  | SEQ of t list
  | IF of t * t
  | IF_NONE of t * t
  | LOOP of t
  | DIP of t
  | FAILWITH
  | FAIL
  | EXEC
  (* Stack Operation *)
  | PUSH of typ * data
  | DUP
  | DROP
  (* Numeric Operation *)
  | EQ
  | ADD
  (* Boolean Operation *)
  | NOT
  | AND
  (* Pair Operation *)
  | PAIR
  | CAR
  | CDR
  (* Map Operation *)
  | EMPTY_MAP
  | UPDATE
  | GET
  | ITER of t

and data = {
  d_v     : data_i;
  d_annot : annot;
}

and data_i = 
  | D_int of Z.t
  | D_mtz of int64
  | D_true
  | D_false
  | D_pair of data * data
  | D_map of (data, data) BatMap.t

and typ = string
and annot = string


(*****************************************************************************)
(*****************************************************************************)
(* Utility                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

exception Utility_Undefined of string

let uuwith s = raise (Utility_Undefined s)

let i2di = fun x -> D_int (Z.of_int x)

let di2i = function
  | D_int x -> (Z.to_int x)
  | D_mtz x -> (Int64.to_int x)
  | _ -> uuwith "di2i"

let b2di = fun x -> if x then D_true else D_false

