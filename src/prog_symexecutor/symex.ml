module Expr   = Z3.Expr
module Solver = Z3.Solver

module Inst   = Micse_Interpreter.PlainEvalTrace.PlainInst

(* Solver for symbolic execution *)


(*****************************************************************************)
(*****************************************************************************)
(*
  Symbolic Execution

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
      - Current Instruction : Micse_Interpreter.Inst.S.t    (* Precisely, Micse_Interpreter.PlainEvalTrace.PlainInst.t *)
      - Current Assertion   : Z3.Expr.expr list
      - Current Bound       : (int, int) BatMap.t   (* (program point) -> (# of bound) *)
      - Current Trace       : int list  (* list of program points *)

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
        - Current Bound     : (int, int) BatMap.t   (* (program point) -> (# of bound) *)
        - Current Trace     : int list  (* list of program points *)

*)
(*****************************************************************************)
(*****************************************************************************)

type limits       = float * int
type stored_elem  = Expr.expr list * Solver.status * int list
type curstat_elem = Inst.t * Expr.expr list * (int, int) BatMap.t * int list

type stored       = (int BatSet.t) * (stored_elem list)
type curstatset   = curstat_elem BatSet.t

let rec run : limits -> stored -> curstat_elem -> (stored * curstatset) = 
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
  let csset = BatSet.singleton (tval, assrt, bound, trace) in

  (* check time limit *)
  if Sys.time () > ttlim 
  then ((rpts, storedl), csset) 
  else (
    (* addConstr : perform symbolic execution (add constraint) *)
    let addConstr : (stored * curstatset) = 
      (match tval.inst with
      (*
      (* TODO LIST *)
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
      *)
      | _ -> Stdlib.failwith ("symex.ml : run : foldFunc : symbolic execution for this operation " ^ (Inst.Tag.to_string tval.tag) ^ " undefined.")
      )
    in
    let stored', curstatset' = addConstr in

    (* symbolic execution strategy : depth-first traversing with BatSet.fold *)
    if BatSet.is_empty curstatset' 
      then (stored', BatSet.empty)
      else (
        BatSet.fold 
          (fun cstat ((acc_rpts, acc_storedl), acc_cstatset) -> 
            let (f_rpts, f_storedl), f_cstatset = run limitvals (acc_rpts, acc_storedl) cstat in
            ((BatSet.union acc_rpts f_rpts, (f_storedl @ acc_storedl)), BatSet.union acc_cstatset f_cstatset)
          )
          curstatset' 
          (stored', BatSet.empty)
      )

  ) (* function run: End of the else branch of "Sys.time () > ttlim" *)


