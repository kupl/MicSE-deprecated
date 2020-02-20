
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
(** This does not guarantee that the argument is valid type. *)
let isSimpleComparable : t -> bool = function
  | SCT_int | SCT_nat | SCT_string | SCT_bytes | SCT_mutez | SCT_bool
  | SCT_key_hash | SCT_timestamp | SCT_address -> true
  | _ -> false
let isComparable : t -> bool = function
  | CT_pair _ -> true
  | _ as argt -> isSimpleComparable argt
let isNonComparable : t -> bool = fun argt -> not (isComparable argt)

(** check whether CT_pair or T_set or T_map or T_bigmap has valid constructees *)
let isValidTyp : t -> bool = function
  | CT_pair (t1, t2) -> isSimpleComparable t1 && isComparable t2
  | T_set t1 -> isComparable t1
  | T_map (t1, _) -> isComparable t1
  | T_bigmap (t1, _) -> isComparable t1
  | _ -> true

(** generate pair type. if possible, it prefers to generate CT_pair *)
let genPairTyp : t -> t -> t =
  fun t1 t2 ->
  if isSimpleComparable t1 && isComparable t2
  then CT_pair (t1, t2) else T_pair (t1, t2)

(** It considers that CT_pair and T_pair are the same type. It does not check "isValidTyp" of given type. *)
let rec isEqTyp : t -> t -> bool =
  fun t1 t2 ->
  match t1, t2 with
  | CT_pair (tt1, tt2), T_pair (tt3, tt4)   -> (isEqTyp tt1 tt3) && (isEqTyp tt2 tt4)
  | T_pair (tt1, tt2) , CT_pair (tt3, tt4)  -> (isEqTyp tt1 tt3) && (isEqTyp tt2 tt4)
  | CT_pair (tt1, tt2), CT_pair (tt3, tt4)  -> (isEqTyp tt1 tt3) && (isEqTyp tt2 tt4)
  | T_pair (tt1, tt2) , T_pair (tt3, tt4)   -> (isEqTyp tt1 tt3) && (isEqTyp tt2 tt4)
  | T_option tt1, T_option tt2 -> isEqTyp tt1 tt2
  | T_list tt1, T_list tt2 -> isEqTyp tt1 tt2
  | T_set tt1, T_set tt2 -> isEqTyp tt1 tt2
  | T_contract tt1, T_contract tt2 -> isEqTyp tt1 tt2
  | T_or (tt1, tt2), T_or (tt3, tt4) -> (isEqTyp tt1 tt3) && (isEqTyp tt2 tt4)
  | T_lambda (tt1, tt2), T_lambda (tt3, tt4) -> (isEqTyp tt1 tt3) && (isEqTyp tt2 tt4)
  | T_map (tt1, tt2), T_map (tt3, tt4) -> (isEqTyp tt1 tt3) && (isEqTyp tt2 tt4)
  | T_bigmap (tt1, tt2), T_bigmap (tt3, tt4) -> (isEqTyp tt1 tt3) && (isEqTyp tt2 tt4)
  | tt1, tt2 when tt1 = tt2 -> true
  | _ -> false

(** string conversion *)
let rec to_string : t -> string = function
  | SCT_int -> "SCT_int"
  | SCT_nat -> "SCT_nat"
  | SCT_string -> "SCT_string"
  | SCT_bytes -> "SCT_bytes"
  | SCT_mutez -> "SCT_mutez"
  | SCT_bool -> "SCT_bool"
  | SCT_key_hash -> "SCT_key_hash"
  | SCT_timestamp -> "SCT_timestamp"
  | SCT_address -> "SCT_address"
  | CT_pair (t1, t2) -> "CT_pair (" ^ to_string t1 ^ ", " ^ to_string t2 ^ ")"
  | T_key -> "T_key"
  | T_unit -> "T_unit"
  | T_signature -> "T_signature"
  | T_option t -> "T_option (" ^ to_string t ^ ")"
  | T_list t -> "T_list (" ^ to_string t ^ ")"
  | T_set t -> "T_set (" ^ to_string t ^ ")"
  | T_operation -> "T_operation"
  | T_contract t -> "T_contract (" ^ to_string t ^ ")"
  | T_pair (t1, t2) -> "T_pair (" ^ to_string t1 ^ ", " ^ to_string t2 ^ ")"
  | T_or (t1, t2) -> "T_or (" ^ to_string t1 ^ ", " ^ to_string t2 ^ ")"
  | T_lambda (t1, t2) -> "T_lambda (" ^ to_string t1 ^ ", " ^ to_string t2 ^ ")"
  | T_map (t1, t2) -> "T_map (" ^ to_string t1 ^ ", " ^ to_string t2 ^ ")"
  | T_bigmap (t1, t2) -> "T_bigmap (" ^ to_string t1 ^ ", " ^ to_string t2 ^ ")"
  | T_chain_id -> "T_chain_id"
