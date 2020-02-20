(*****************************************************************************)
(*****************************************************************************)
(* Pre-defined modules                                                       *)
(*****************************************************************************)
(*****************************************************************************)
module Timestamp : sig type t end

module Mutez : sig
  (* signature of Int64 module, some are modified for compile issues. *)
  type t = int64
  val zero : int64
  val one : int64
  val minus_one : int64
  val neg : int64 -> int64
  val add : int64 -> int64 -> int64
  val sub : int64 -> int64 -> int64
  val mul : int64 -> int64 -> int64
  val div : int64 -> int64 -> int64
  val rem : int64 -> int64 -> int64
  val succ : int64 -> int64
  val pred : int64 -> int64
  val abs : int64 -> int64
  val max_int : int64
  val min_int : int64
  val logand : int64 -> int64 -> int64
  val logor : int64 -> int64 -> int64
  val logxor : int64 -> int64 -> int64
  val lognot : int64 -> int64
  val shift_left : int64 -> int -> int64
  val shift_right : int64 -> int -> int64
  val shift_right_logical : int64 -> int -> int64
  val of_int : int -> int64
  val to_int : int64 -> int
  val of_float : float -> int64
  val to_float : int64 -> float
  val of_int32 : int32 -> int64
  val to_int32 : int64 -> int32
  val of_nativeint : nativeint -> int64
  val to_nativeint : int64 -> nativeint
  val of_string : string -> int64
  val of_string_opt: string -> int64 option
  val to_string : int64 -> string
  val bits_of_float : float -> int64
  val float_of_bits : int64 -> float
  val compare: t -> t -> int
  val equal: t -> t -> bool
end

module Contract : sig type t end

module Address : sig type t end

module Key : sig type t end

module KeyHash : sig type t end

module Signature : sig type t end

module Chain_id : sig type t end

module Operation : sig type t end

module InstNat : sig
  (* deal with natural number in instruction (not data). 
    Focused in evalution with plain integer values (for easy programming).
  *)
  type t
  val of_int : int -> t
  val to_string : t -> string
  val to_int : t -> int
  val to_Z : t -> Z.t
  val is_nat : t -> bool
  val zero : t

  val (+) : t -> int -> t
  val (-) : t -> int -> t
  val (<) : t -> int -> bool
  val (>) : t -> int -> bool
  val (<=) : t -> int -> bool
  val (>=) : t -> int -> bool
  val (=) : t -> int -> bool
end
