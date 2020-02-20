open PlainEvalTrace

(* Get the string of the trace *)
let traceStr_of_conf : PlainConf.t -> string =
  fun conf ->
  let body = List.fold_left (fun acc x -> acc ^ (string_of_int x) ^ "; ") "" conf.trace in
  "[" ^ body ^ "]"

(* pack only input and storage into PlainConf.stack_t *)
let packstack : Typ.t -> PlainConf.data -> Typ.t -> PlainConf.data -> PlainConf.stack_t =
  fun it i st s -> [T_pair (it, st), D_Pair (i, s)]

(* Pack the code, input and storage into PlainConf.t *)
let pack : PlainConf.inst_t -> Typ.t -> PlainConf.data -> Typ.t -> PlainConf.data -> PlainConf.t =
  fun c it i st s ->
  {code = c; stack = packstack it i st s; trace = []; extra = 0;}

(* Refer to Michelson Whitepaper, the code must return a stack containing a single pair
    whose first element is the list of internal operations that it wants to emit,
    and second element is the new contents of the storage space.
  * "checkExecValid" checks whether the given stack (in Config) is valid return-stack.
  * It does not typechecking.
  * T_operation is not comparable, so CT_pair cannot be the type in valid return-stack.
*)
let checkExecValid : Typ.t -> PlainConf.t -> bool =
  fun storage_typ conf ->
  match conf.stack with
  | (T_pair (T_operation, t), D_Pair _) :: [] when t = storage_typ -> true
  | _ -> false

(* extract only storage part from Config. *)
let getStorage : PlainConf.t -> (Typ.t * PlainConf.data) =
  fun conf ->
  match conf.stack with
  | [T_pair (T_operation, t), D_Pair (_, d)] -> t, d
  | _ -> invalid_arg "plainEvalTraceUtil.ml : getStorage"
