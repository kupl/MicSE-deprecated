module Expr   = Z3.Expr
module Solver = Z3.Solver

module Inst   = Micse_Interpreter.PlainEvalTrace.PlainInst

(* Solver for symbolic execution *)
let vname_id = ref (-1)
let new_vname () : string = incr vname_id; "v" ^ (string_of_int (!vname_id))


(*****************************************************************************)
(*****************************************************************************)
(*
  Symbolic Execution in MicSE

  Predefined:
    - Z3 Solver : Z3.Solver.solver  (* This has side-effects *)

  Input:
    Limits:
      - Total Time Limit    : float (* Upper bound of "Sys.time ()" *)
      - Bound Limit         : int   (* # of loop unrolling *)
    Stored:
      - Reached Points      : int BatSet.t  (* Set of program points *)
      [List of the following tuple type]
        - Assertion         : Z3.Expr.expr list
        - Result            : Z3.Solver.status
        - Trace             : int list      (* list of program points *)
    Current Status:
      - Current Instruction : Micse_Interpreter.Inst.S.t      (* Precisely, Micse_Interpreter.PlainEvalTrace.PlainInst.t *)
      - Current Assertion   : Z3.Expr.expr list
      - Current S-Stack     : (int * (int, string) BatMap.t)  (* symbolic stack, (top, (addr => variable)) *)
      - Current Bound       : (int, int) BatMap.t             (* (program point) -> (# of bound) *)
      - Current Trace       : int list                        (* list of program points *)
      - Snapshots           : (int * ((int, string) BatMap.t)) list   (* snapshot of 'Current S-Stack' to deal with 'DIP' instruction. *)

  Output:
    Stored:
      - Reached Points      : int BatSet.t  (* Set of program points *)
      [List of the following tuple type]
        - Assertion         : Z3.Expr expr list
        - Result            : Z3.Tactic.ApplyResult.apply_result
        - Trace             : int list  (* list of program points *)
    Current Status:
      [BatSet.t of the following tuple type]
        - Current Instruction : Micse_Interpreter.Inst.S.t    (* Precisely, Micse_Interpreter.PlainEvalTrace.PlainInst.t *)
        - Current Assertion : Z3.Expr.expr list
        - Current S-Stack   : (int * (int, string) BatMap.t)  (* symbolic stack, (top, (addr => variable)) *)
        - Current Bound     : (int, int) BatMap.t             (* (program point) -> (# of bound) *)
        - Current Trace     : int list                        (* list of program points *)
        - Snapshots         : (int * ((int, string) BatMap.t)) list   (* snapshot of 'Current S-Stack' to deal with 'DIP' instruction. *)

  About Unrolling (Bounded Verification):
    Unlike unrolling in C-like language which converts 'for' or 'while' statement into multiple 'if-then-else' sequences,
    Michelson has no appropriate instructions to unroll 'ITER' or 'MAP' instructions.
    So instead of syntactically modifies every loop instructions, 
    every symbolic execution path in MicSE writes down how many times it goes into the loop in "Current Bound".
    The "Current Bound" should be interpreted like
    "current symbolic execution path enters ITER instruction at program-point-'62' '3' times."
  
  About Stored Datas:
    This symbolic execution focuses on generate symbolic constraints to enter specific branch path and fail-reachable path.
    When the symbolic execuion path meets FAIL or branch instruction, 
    it sends the constraint to Z3 solver and store that result.
    For branch instructions, it will store two constraints, 
    one for true branch and another for false branch condition.

*)
(*****************************************************************************)
(*****************************************************************************)

type limits       = float * int
type stored_elem  = Expr.expr list * Solver.status * int list
type curstat_elem = Inst.t * Expr.expr list * (int, int) BatMap.t * int list

type stored       = (int BatSet.t) * (stored_elem list)
type curstatset   = curstat_elem BatSet.t


let rec singleStep : limits -> stored -> curstat_elem -> (stored * curstatset) = 
  (*
    <Arguments>
    ttlim   = Total Time Limit;
    blim    = Bound Limit;
    rpts    = Reached Points;
    storedl = List of Results <Assertion, Result, and Trace>;
    tval    = Current Instruction (Inst.t type);
    assrt   = Current Assertion;
    bound   = Current Bound;
    trace   = Current Trace;
  *)
  fun (ttlim, blim) (rpts, storedl) (tval, assrt, bound, trace) ->
  let limitvals = (ttlim, blim) in
  let storedvals = (rpts, storedl) in
  let csset = BatSet.singleton (tval, assrt, bound, trace) in

  (* check time limit *)
  if Sys.time () > ttlim
  then ((rpts, storedl), csset)
  else (
    match tval.inst with
    (*
    (* TODO LIST *)
    | SEQ tlist
    | DROP
    | DROP_N inat
    | DUP
    | SWAP
    | DIG inat
    | DUG inat
    | PUSH (ty, d)
    | SOME
    | NONE ty
    | UNIT
    | IF_NONE (t1, t2)
    | PAIR
    | CAR
    | CDR
    | LEFT ty
    | RIGHT ty
    | IF_LEFT (t1, t2)
    | NIL ty
    | CONS
    | IF_CONS (t1, t2)
    | SIZE
    | EMPTY_SET ty  (* comp_typ *)
    | EMPTY_MAP (ty1, ty2)  (* (comp_typ * typ) *)
    | EMPTY_BIG_MAP (ty1, ty2)  (* (comp_typ * typ) *)
    | MAP t
    | ITER t
    | MEM
    | GET
    | UPDATE
    | IF (t1, t2)
    | IFEQ (t1, t2)
    | IFNEQ (t1, t2)
    | IFLT (t1, t2)
    | IFGT (t1, t2)
    | IFLE (t1, t2)
    | IFGE (t1, t2)
    | LOOP t
    | LOOP_LEFT t 
    | LAMBDA of (ty1, ty2, t)
    | EXEC
    | DIP t
    | DIP_N (inat, t)
    | FAILWITH
    | FAIL
    | CAST ty
    | RENAME
    | CONCAT
    | SLICE
    | PACK
    | UNPAIR
    | UNPACK ty
    | ADD
    | SUB
    | MUL
    | EDIV
    | ABS
    | ISNAT
    | INT
    | NEG
    | LSL
    | LSR
    | OR
    | AND
    | XOR
    | NOT
    | COMPARE
    | EQ
    | NEQ
    | LT
    | GT
    | LE
    | GE
    | CMPEQ
    | CMPNEQ
    | CMPLT
    | CMPGT
    | CMPLE
    | CMPGE
    | SELF
    | CONTRACT ty
    | TRANSFER_TOKENS
    | SET_DELEGATE
    | CREATE_ACCOUNT
    | CREATE_CONTRACT t
    | IMPLICIT_ACCOUNT
    | NOW
    | AMOUNT
    | BALANCE
    | CHECK_SIGNATURE
    | BLAKE2B
    | SHA256
    | SHA512
    | HASH_KEY
    | STEPS_TO_QUOTA
    | SOURCE
    | SENDER
    | ADDRESS
    | CHAIN_ID
    | APPLY
    | Hidden ilist
    *)
    | _ -> Stdlib.failwith ("symex.ml : run : foldFunc : symbolic execution for this operation " ^ (Inst.Tag.to_string tval.tag) ^ " undefined.")
  ) (* function run: End of the else branch of "Sys.time () > ttlim" *)



