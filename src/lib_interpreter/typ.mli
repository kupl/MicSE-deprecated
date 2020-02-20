
(*****************************************************************************)
(*****************************************************************************)
(* Types                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

type t =
  (* simple comparable types *)
  | SCT_int
  | SCT_nat
  | SCT_string
  | SCT_bytes
  | SCT_mutez
  | SCT_bool
  | SCT_key_hash
  | SCT_timestamp
  | SCT_address
  (* comparable types *)
  | CT_pair of t * t  (* pair : sct, ct *)
  (* others *)
  | T_key
  | T_unit
  | T_signature
  | T_option of t
  | T_list of t
  | T_set of t (* ct *)
  | T_operation
  | T_contract of t
  | T_pair of t * t
  | T_or of t * t
  | T_lambda of t * t
  | T_map of t * t  (* pair : ct, t *)
  | T_bigmap of t * t (* pair : ct, t *)
  | T_chain_id

(** classify simple-comparable, comparable, and plain types. *)
val isSimpleComparable : t -> bool
val isComparable : t -> bool
val isNonComparable : t -> bool

(** check whether CT_pair or T_set or T_map has valid constructees *)
val isValidTyp : t -> bool
  
(** generate pair type. if possible, it prefers to generate CT_pair *)
val genPairTyp : t -> t -> t

(** It considers that CT_pair and T_pair are the same type. It does not check "isValidTyp" of given type. *)
val isEqTyp : t -> t -> bool

(** string conversion *)
val to_string : t -> string
