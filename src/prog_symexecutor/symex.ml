open Z3

module Inst   = Micse_Interpreter.PlainEvalTrace.PlainInst
module SS     = SymStack

exception Violate_Precondition of string
let pcError s = raise (Violate_Precondition s)

(*
let lcond_singleton : 'a -> bool -> 'a list = 
  fun x cond -> if cond then [x] else []
*)

(*****************************************************************************)
(*****************************************************************************)
(* Run Z3 for symbolic execution                                             *)
(*****************************************************************************)
(*****************************************************************************)

let z3_result : context -> Expr.expr list -> Solver.status = 
  fun ctx exprs ->
  let s = Z3Util.gen_ctx () |> Solver.mk_simple_solver in
  ((Solver.add s exprs); Solver.check s [])

let not_unsat status : bool =
  status <> Z3.Solver.UNSATISFIABLE


(*****************************************************************************)
(*****************************************************************************)
(* Instruction Processing                                                    *)
(*****************************************************************************)
(*****************************************************************************)

(* Wrap Inst.t list into SEQ Instruction. Outermost SEQ will have (-1) tag. *)
let rec seq_wrap : Inst.t list -> Inst.t =
  fun x -> {inst=SEQ(x); tag=(-1);}

(* remove the redundant SEQ instructions. *)
let rec inst_flatten : Inst.t list -> Inst.t list = function
  | [] -> []
  | {inst = Inst.SEQ tlist; tag = _;} :: tail -> 
    (inst_flatten tlist) @ (inst_flatten tail)
  | h :: tail -> h :: (inst_flatten tail)

let inst_preprocess : Inst.t -> Inst.t = fun t -> match t.inst with
  | Inst.SEQ tlist -> {t with inst = Inst.SEQ (inst_flatten tlist);}
  | _ -> seq_wrap [t]


(*****************************************************************************)
(*****************************************************************************)
(*
  Symbolic Execution in MicSE

  Assumed Precondition:
    - Every Michelson program is wrapped with the outermost "SEQ" instruction.
      (In other words, "Current Instruction" is preprocessed by "inst_preprocess" function)

  Guaranteed Postcondition:
    - Every "Current Instructions" in [Current Status Output] are preprocessed by "inst_preprocess" function or have equivalent shape.

  Input:
    Z3:
      - Context             : Z3.context
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

  About singleStep:
    - If current instruction is Empty (i.e. SEQ []), solve current assertions using z3 and store the result and end.
    - If current instruction is FAIL, solve current assertions using z3 and store the result and end.
    - If current instruction is SEQ, IF, LOOP, DIP, ..., perform unrolling and end. It might return multiple current status.
    - Else, add assertion to current status.

*)
(*****************************************************************************)
(*****************************************************************************)

type symstack     = SymStack.t

type limits       = float * int
type stored_elem  = Expr.expr list * Solver.status * int list
type curstat_elem = Inst.t * Expr.expr list * symstack * (int, int) BatMap.t * int list * symstack list

type stored       = (int BatSet.t) * (stored_elem list)
type curstatset   = curstat_elem BatSet.t


(* utility for data processing *)
let update_retval : context -> curstat_elem -> (stored_elem list * curstatset) -> (stored_elem list * curstatset) =
  fun ctx (tval, assrt, sstack, bound, trace, sshot) (storedl, csset) ->
  let result   = z3_result ctx assrt in
  let storedl' = (assrt, result, trace) :: storedl in
  let csset'   = if not_unsat result then BatSet.add (tval, assrt, sstack, bound, trace, sshot) csset else csset in
  (storedl', csset')  


let rec singleStep : context -> limits -> stored -> curstat_elem -> (stored * curstatset) = 
  (*
    <Arguments>
    z3ctx   = Z3 context;
    ttlim   = Total Time Limit;
    blim    = Bound Limit;
    rpts    = Reached Points;
    storedl = List of Results <Assertion, Result, and Trace>;
    tval    = Current Instruction (Inst.t type);
    assrt   = Current Assertion;
    sstack  = Current S-Stack;
    bound   = Current Bound;
    trace   = Current Trace;
    sshot   = Snapshots;
  *)
  fun ctx (ttlim, blim) (rpts, storedl) (tval, assrt, sstack, bound, trace, sshot) ->
  let limitvals = (ttlim, blim) in
  let storedvals = (rpts, storedl) in
  let csset = BatSet.singleton (tval, assrt, sstack, bound, trace, sshot) in

  (* check time limit *)
  if Sys.time () > ttlim
  then ((rpts, storedl), csset)
  else (
    match tval.inst with
    | SEQ [] ->
      (* Execution ended. Solve current assertion and put the result into stored values. *)
      let r = z3_result ctx assrt in
      ((rpts, (assrt, r, trace) :: storedl), BatSet.empty)
    
    | SEQ (head :: tail) ->
      
      (******************************)
      (* preprocess for less typing *)
      (******************************)
      
      (* trace *)
      let trace'      = head.tag :: trace in
      let rpts'       = BatSet.add head.tag rpts in
      
      (* z3 sorts *)
      let bool_s      = (Boolean.mk_sort ctx) in

      (* z3 constant values *)
      let true_expr   = (Boolean.mk_true ctx) in 
      let false_expr  = (Boolean.mk_false ctx) in

      (* z3 symbols *)
      let stack_sym   = (Symbol.mk_string ctx "ssym") in


      (******************************)
      (* actual single step         *)
      (******************************)
      (match head.inst with
      (* Control Flow (except SEQ) *)
      | IF (t1, t2) ->
        
        (* common things *)
        let sstack'     = SS.pop sstack in

        (* curstat_elem of true branch *)
        let tinst       = (([t1] |> inst_flatten) @ tail) |> seq_wrap in (* (inst_flatten tail) is not needed because of precondition. *)
        let teq_assrt   = (Boolean.mk_eq ctx true_expr (SS.top sstack)) :: assrt in
        let tce         = (tinst, teq_assrt, sstack', bound, trace', sshot) in
        (* curstat_elem of false branch *)
        let finst       = (([t2] |> inst_flatten) @ tail) |> seq_wrap in
        let feq_assrt   = (Boolean.mk_eq ctx false_expr (SS.top sstack)) :: assrt in
        let fce : curstat_elem = (finst, feq_assrt, sstack', bound, trace', sshot) in
        
        (* gather *)
        let storedl', csset' = (
          (storedl, BatSet.empty)
          |> update_retval ctx tce
          |> update_retval ctx fce
        )
        in ((rpts', storedl'), csset')
        
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
      | Hidden slist
      *)
      | SEQ _ -> pcError "singleStep Precondition Violated."
      | _ -> pcError ("symbolic execution for operation " ^ (Inst.Tag.to_string tval.tag) ^ " is undefined.")
      )
    | _ -> pcError "Receive Non-SEQ Instruction."
  ) (* function run: End of the else branch of "Sys.time () > ttlim" *)


(* Breadth-First symbolic execution run with state-pruning strategy 'filterFunc' *)
let rec run : context -> limits -> ((stored * curstatset) -> curstatset) -> stored -> curstatset -> (stored * curstatset) = 
  fun z3ctx (ttlim, blim) filterFunc (rpts, storedl) csset ->
  (* check time limit *)

  let limitvals = (ttlim, blim) in
  let storedvals = (rpts, storedl) in

  if Sys.time () > ttlim then (storedvals, csset)
  else (
    let prunedset = filterFunc ((rpts, storedl), csset) in
    let foldFunc : curstat_elem -> (stored * curstatset) -> (stored * curstatset) = 
      fun celem (acc_stored, acc_css) ->
      let (s', c') = singleStep z3ctx limitvals acc_stored celem in
      (s', BatSet.union c' acc_css)
    in
    let stored', csset' = BatSet.fold foldFunc prunedset (storedvals, BatSet.empty) in
    if BatSet.is_empty csset' then (stored', csset') else run z3ctx limitvals filterFunc storedvals csset'
  )
  
