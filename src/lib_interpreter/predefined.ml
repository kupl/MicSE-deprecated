(*****************************************************************************)
(*****************************************************************************)
(* Pre-defined modules                                                       *)
(*****************************************************************************)
(*****************************************************************************)
module Timestamp = struct
  (* Dates in the real world *)
  type t = Z.t
end

module Mutez = struct
  include Int64
end

(* struct
  (* A specific type for manipulating tokens *)
  type t = int64
  let isValid v = (Int64.compare (-1L) v) < 0
  let maxT = Int64.max_int
  let compare = Int64.compare
end
*)

module Contract = struct
  (* A contract, with the type of its code, 'contract unit' for implicit accounts *)
  type t =
    | Implicit of string
    | Originated of string
end
module Address = struct
  (* An untyped address (implicit account or smart contract) *)
  type t = Contract.t * string
end
module Key = struct
  (* A public cryptographic key *)
  type t = string
end
module KeyHash = struct
  (* A hash of a public cryptographic key *)
  type t = string
end
module Signature = struct
  (* A cryptographic signature *)
  type t = string
end
module Chain_id = struct
  (* An identifier for a chain, used to distinguish the test and the main chains *)
  type t = string
end
module Operation = struct
  (* https://tezos.gitlab.io/whitedoc/michelson.html#inter-transaction-semantics *)
  type t = string
end

module InstNat = struct
  (* deal with natural number in instruction (not data). 
    Focused in evalution with plain integer values (for easy programming).
  *)
  (* Since it is unlikely to write very big number in the code,
    plain integer will be good enough for now. *)
  type t = int
  let of_int : int -> t = fun n -> n
  let to_string : t -> string = string_of_int
  let to_int : t -> int = fun n -> n
  let to_Z : t -> Z.t = Z.of_int
  let is_nat : t -> bool = fun n -> not (n < 0)
  let zero : t = 0
  
  let (+) : t -> int -> t = (+)
  let (-) : t -> int -> t = (-)
  let (<) : t -> int -> bool = (<)
  let (>) : t -> int -> bool = (>)
  let (<=) : t -> int -> bool = (<=)
  let (>=) : t -> int -> bool = (>=)
  let (=) : t -> int -> bool = (=)
end
