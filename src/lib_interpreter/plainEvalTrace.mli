module IdTag : Inst.TAG with type t = int
module PlainInst : Inst.S with module Tag = IdTag
module RunExt : Config.EXTEND with type t = int

module PlainConf : Config.S with module InstM = PlainInst and module Extend = RunExt

(*
type runtimeErrTyp =
  | LSL_overflow
  | LSR_overflow
  | Mutez_overflow
  | Mutez_underflow

exception Not_implemented of PlainConf.t
exception NotEnough_stack of PlainConf.t
exception TypeError of PlainConf.t
exception Exec_limit of PlainConf.t
exception Failed_stackstate of PlainConf.t * string
exception RuntimeErr of PlainConf.t * runtimeErrTyp
*)

(* Convert pureInst into PlainConf *)
val inst_t_of_PureInst : PureInst.t -> int -> (PlainConf.inst_t * int)
val data_of_PureData : PureInst.data -> int -> (PlainConf.data * int)

(* make value to string *)
val string_of_inst_t : PlainConf.inst_t -> bool -> string
val string_of_inst : PlainConf.InstM.inst -> bool -> string
val string_of_data : PlainConf.data -> bool -> string
val string_of_stack_t : PlainConf.stack_t -> string -> bool -> string

(* Evaluation with execute-count limit *)
val eval_inst : int -> PlainConf.t -> PlainConf.t
