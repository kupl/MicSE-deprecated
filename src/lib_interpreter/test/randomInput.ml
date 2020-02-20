open Micse_Interpreter.Predefined

module Typ = Micse_Interpreter.Typ
module PureInst = Micse_Interpreter.PureInst
(* Generate random input. Generated data's type is PureInst.data *)

exception Not_implemented

(* type t : random-generated data*)
type t = {
  data : PureInst.data;
  initialRandState : Random.State.t;
}

let hex2char = function
  | 0 -> '0'  | 1 -> '1'  | 2 -> '2'  | 3 -> '3'
  | 4 -> '4'  | 5 -> '5'  | 6 -> '6'  | 7 -> '7'
  | 8 -> '8'  | 9 -> '9'  | 10 -> 'a'  | 11 -> 'b'
  | 12 -> 'c'  | 13 -> 'd'  | 14 -> 'e'  | 15 -> 'f'
  | _ -> invalid_arg "randomInput.ml : hex2char"
let rec int2bytes accb n =
  if n <= 0 then accb 
  else begin 
    let c = hex2char (n mod 16) in
    let accbb = Bytes.extend accb 0 1 in
    let _ = Bytes.set accbb 0 c in
    int2bytes accbb (n / 16)
  end

let randint_ub = 1 lsl 30 - 1

let naive_randData (t : Typ.t) (state_opt : Random.State.t option) : t =
  let state = begin 
    match state_opt with
    | Some sopt -> sopt
    | None -> Random.State.make_self_init ()
  end in
  let _ = Random.set_state state in  
  let rec inner_randDataGen (ty : Typ.t) : PureInst.data =
    match ty with
    | SCT_int -> D_Int (let i = Random.int randint_ub in Z.of_int (i * (if Random.bool () then 1 else (-1))))
    | SCT_nat -> D_Int (Z.of_int (Random.int randint_ub))
    | SCT_string -> D_String (Bytes.to_string (let i = Random.int randint_ub in int2bytes Bytes.empty i))
    | SCT_bytes -> D_Bytes (let i = Random.int randint_ub in int2bytes Bytes.empty i)
    | SCT_mutez -> D_Mutez (Random.int64 Int64.max_int)
    | SCT_bool -> if Random.bool () then D_True else D_False
    | CT_pair (t1, t2) -> D_Pair (inner_randDataGen t1, inner_randDataGen t2)
    | T_option t -> if Random.bool () then D_Some (inner_randDataGen t) else D_None
    | T_unit -> D_Unit
    | T_pair (t1, t2) -> D_Pair (inner_randDataGen t1, inner_randDataGen t2)
    | T_or (t1, t2) -> if Random.bool () then D_Left (inner_randDataGen t1) else D_Right (inner_randDataGen t2)
    | SCT_key_hash                -> raise Not_implemented
    | SCT_timestamp               -> raise Not_implemented
    | SCT_address                 -> raise Not_implemented
    | T_key                       -> raise Not_implemented
    | T_signature                 -> raise Not_implemented
    | T_list t                    -> raise Not_implemented
    | T_operation                 -> raise Not_implemented
    | T_contract t                -> raise Not_implemented
    | T_lambda (t1, t2)           -> raise Not_implemented
    | T_set t                     -> raise Not_implemented
    | T_map (t1, t2)              -> raise Not_implemented
    | T_bigmap (t1, t2)           -> raise Not_implemented
    | T_chain_id                  -> raise Not_implemented
  in
  {data = (inner_randDataGen t); initialRandState = state;}