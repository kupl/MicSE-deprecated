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

(* t type utility *)
val ii2t : inst_i -> t

(* data_i type conversion *)
val i2di : int    -> data_i
val di2i : data_i -> int
val b2di : bool   -> data_i
val di2b : data_i -> bool
val p2di : (data_i * data_i) -> data_i
val di2p : data_i -> (data_i * data_i)

(* data type conversion *)
val i2d  : int  -> data
val d2i  : data -> int
val b2d  : bool -> data
val d2b  : data -> bool
val p2d  : (data_i * data_i) -> data
val d2p  : data -> (data_i * data_i)

