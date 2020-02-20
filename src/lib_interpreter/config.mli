(*****************************************************************************)
(*****************************************************************************)
(* Configuration                                                             *)
(*****************************************************************************)
(*****************************************************************************)
module type EXTEND = sig
  type t
  val init_value : t
end

(* Configuration means a single state in small-step semantics *)
module type S = sig
  module InstM : Inst.S
  module Extend : EXTEND
  type inst_t = InstM.t
  type data = InstM.data
  type stack_t = (Typ.t * InstM.data) list
  type tagtrace_t = InstM.Tag.t list
  type t = {
    code : inst_t;
    stack : stack_t;
    trace : tagtrace_t;
    extra : Extend.t;
  }

  type runtimeErrTyp =
  | LSL_overflow
  | LSR_overflow
  | Mutez_overflow
  | Mutez_underflow
  exception Not_implemented of t
  exception NotEnough_stack of t
  exception TypeError of t
  exception Exec_limit of t
  exception Failed_stackstate of t * string
  exception RuntimeErr of t * runtimeErrTyp
end

module Make (I : Inst.S) (E : EXTEND) : S with module InstM = I and module Extend = E
