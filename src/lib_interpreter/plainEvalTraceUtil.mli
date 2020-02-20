open PlainEvalTrace

(* Get the string of the trace *)
val traceStr_of_conf : PlainConf.t -> string

(* pack only input and storage into PlainConf.stack_t *)
val packstack : Typ.t -> PlainConf.data -> Typ.t -> PlainConf.data -> PlainConf.stack_t

(* Pack the code, input and storage into PlainConf.t *)
val pack : PlainConf.inst_t -> Typ.t -> PlainConf.data -> Typ.t -> PlainConf.data -> PlainConf.t

(* Refer to Michelson Whitepaper, the code must return a stack containing a single pair
    whose first element is the list of internal operations that it wants to emit,
    and second element is the new contents of the storage space.
  * "checkExecValid" checks whether the given stack (in Config) is valid return-stack.
  * It does not typechecking.
  * T_operation is not comparable, so CT_pair cannot be the type in valid return-stack.
*)
val checkExecValid : Typ.t -> PlainConf.t -> bool

(* extract only storage part from Config. *)
val getStorage : PlainConf.t -> (Typ.t * PlainConf.data)
