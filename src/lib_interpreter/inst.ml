(*****************************************************************************)
(*****************************************************************************)
(* Instructions & Datas                                                      *)
(*****************************************************************************)
(*****************************************************************************)
module type TAG = sig
  type t
  val to_string : t -> string
  val eq : t -> t -> bool
end

module type S = sig
  open Predefined
  module Tag : TAG
  type t = {
    inst : inst;
    tag : Tag.t;
  }
  and typ = Typ.t
  and inst =
    | SEQ of t list
    | DROP
    | DROP_N of InstNat.t
    | DUP
    | SWAP
    | DIG of InstNat.t
    | DUG of InstNat.t
    | PUSH of typ * data
    | SOME
    | NONE of typ
    | UNIT
    | IF_NONE of t * t
    | PAIR
    | CAR
    | CDR
    | LEFT of typ
    | RIGHT of typ 
    | IF_LEFT of t * t
    | NIL of typ 
    | CONS
    | IF_CONS of t * t 
    | SIZE
    | EMPTY_SET of typ  (* comp_typ *)
    | EMPTY_MAP of typ * typ  (* (comp_typ * typ) *)
    | EMPTY_BIG_MAP of typ * typ  (* (comp_typ * typ) *)
    | MAP of t 
    | ITER of t 
    | MEM
    | GET
    | UPDATE
    | IF of t * t 
    | IFEQ of t * t 
    | IFNEQ of t * t 
    | IFLT of t * t 
    | IFGT of t * t 
    | IFLE of t * t 
    | IFGE of t * t 
    | LOOP of t 
    | LOOP_LEFT of t 
    | LAMBDA of typ * typ * t 
    | EXEC
    | DIP of t 
    | DIP_N of InstNat.t * t
    | FAILWITH
    | FAIL
    | CAST of typ
    | RENAME
    | CONCAT
    | SLICE
    | PACK
    | UNPAIR
    | UNPACK of typ 
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
    | CONTRACT of typ 
    | TRANSFER_TOKENS
    | SET_DELEGATE
    | CREATE_ACCOUNT
    | CREATE_CONTRACT of t 
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
  and data =
    | D_Int of Z.t
    | D_String of String.t
    | D_Bytes of Bytes.t
    | D_Unit
    | D_True
    | D_False
    | D_Pair of data * data
    | D_Left of data
    | D_Right of data
    | D_Some of data
    | D_None
    | D_List of data list
    | D_Set of data BatSet.t
    | D_Map of (data, data) BatMap.t
    | D_Bigmap of (data, data) BatMap.t
    | D_Lambda of t
    | D_Mutez of Mutez.t
    | D_Contract of Contract.t
    | D_Address of Address.t
    | D_Key of Key.t
    | D_KeyHash of KeyHash.t
    | D_Signature of Signature.t
    | D_Chain_id of Chain_id.t
    | D_Operation of Operation.t

  (** get minimum stack size to run the instruction one step *)
  val minStackSize : inst -> InstNat.t
  
  (** check validity of EMPTY_SET, EMPTY_MAP, and EMPTY_BIG_MAP *)
  val isValidInst : inst -> bool
  
end

module Make (T : TAG) = struct
  open Predefined
  open Predefined.InstNat
  module Tag = T
  type t = {
    inst : inst;
    tag : Tag.t;
  }
  and typ = Typ.t
  and inst =
    | SEQ of t list
    | DROP
    | DROP_N of InstNat.t
    | DUP
    | SWAP
    | DIG of InstNat.t
    | DUG of InstNat.t
    | PUSH of typ * data
    | SOME
    | NONE of typ
    | UNIT
    | IF_NONE of t * t
    | PAIR
    | CAR
    | CDR
    | LEFT of typ
    | RIGHT of typ 
    | IF_LEFT of t * t
    | NIL of typ 
    | CONS
    | IF_CONS of t * t 
    | SIZE
    | EMPTY_SET of typ  (* comp_typ *)
    | EMPTY_MAP of typ * typ  (* (comp_typ * typ) *)
    | EMPTY_BIG_MAP of typ * typ  (* (comp_typ * typ) *)
    | MAP of t 
    | ITER of t 
    | MEM
    | GET
    | UPDATE
    | IF of t * t 
    | IFEQ of t * t 
    | IFNEQ of t * t 
    | IFLT of t * t 
    | IFGT of t * t 
    | IFLE of t * t 
    | IFGE of t * t 
    | LOOP of t 
    | LOOP_LEFT of t 
    | LAMBDA of typ * typ * t 
    | EXEC
    | DIP of t 
    | DIP_N of InstNat.t * t
    | FAILWITH
    | FAIL
    | CAST of typ
    | RENAME
    | CONCAT
    | SLICE
    | PACK
    | UNPAIR
    | UNPACK of typ 
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
    | CONTRACT of typ 
    | TRANSFER_TOKENS
    | SET_DELEGATE
    | CREATE_ACCOUNT
    | CREATE_CONTRACT of t 
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
  and data =
    | D_Int of Z.t
    | D_String of String.t
    | D_Bytes of Bytes.t
    | D_Unit
    | D_True
    | D_False
    | D_Pair of data * data
    | D_Left of data
    | D_Right of data
    | D_Some of data
    | D_None
    | D_List of data list
    | D_Set of data BatSet.t
    | D_Map of (data, data) BatMap.t
    | D_Bigmap of (data, data) BatMap.t
    | D_Lambda of t
    | D_Mutez of Mutez.t
    | D_Contract of Contract.t
    | D_Address of Address.t
    | D_Key of Key.t
    | D_KeyHash of KeyHash.t
    | D_Signature of Signature.t
    | D_Chain_id of Chain_id.t
    | D_Operation of Operation.t

  (** get minimum stack size to run the instruction one step *)
  let minStackSize : inst -> InstNat.t = 
    let ii : int -> InstNat.t = InstNat.of_int in
    function
    | SEQ _ -> ii 0
    | DROP -> ii 1
    | DROP_N n -> n
    | DUP -> ii 1
    | SWAP -> ii 2
    | DIG n -> n+1
    | DUG n -> n+1
    | PUSH _ -> ii 0
    | SOME -> ii 1
    | NONE _ -> ii 0
    | UNIT -> ii 0
    | IF_NONE _ -> ii 1
    | PAIR -> ii 2
    | CAR -> ii 1
    | CDR -> ii 1
    | LEFT _ -> ii 1
    | RIGHT _ -> ii 1
    | IF_LEFT _ -> ii 1
    | NIL _ -> ii 0
    | CONS -> ii 2
    | IF_CONS _ -> ii 1
    | SIZE -> ii 1
    | EMPTY_SET _ -> ii 0
    | EMPTY_MAP _ -> ii 0
    | EMPTY_BIG_MAP _ -> ii 0
    | MAP _ -> ii 1
    | ITER _ -> ii 1
    | MEM -> ii 2
    | GET -> ii 2
    | UPDATE -> ii 3
    | IF _ -> ii 1
    | IFEQ _ -> ii 1
    | IFNEQ _ -> ii 1
    | IFLT _ -> ii 1
    | IFGT _ -> ii 1
    | IFLE _ -> ii 1
    | IFGE _ -> ii 1
    | LOOP _ -> ii 1
    | LOOP_LEFT _ -> ii 1
    | LAMBDA _ -> ii 0
    | EXEC -> ii 2
    | DIP _ -> ii 1
    | DIP_N (n, _) -> n
    | FAILWITH -> ii 1
    | FAIL -> ii 0
    | CAST _ -> ii 1
    | RENAME -> ii 1
    | CONCAT -> ii 2
    | SLICE -> ii 1
    | PACK -> ii 1
    | UNPAIR -> ii 1
    | UNPACK _ -> ii 1
    | ADD -> ii 2
    | SUB -> ii 2
    | MUL -> ii 2
    | EDIV -> ii 2
    | ABS -> ii 1
    | ISNAT -> ii 1
    | INT -> ii 1
    | NEG -> ii 1
    | LSL -> ii 2
    | LSR -> ii 2
    | OR -> ii 2
    | AND -> ii 2
    | XOR -> ii 2
    | NOT -> ii 1
    | COMPARE -> ii 2
    | EQ -> ii 1
    | NEQ -> ii 1
    | LT -> ii 1
    | GT -> ii 1
    | LE -> ii 1
    | GE -> ii 1
    | CMPEQ -> ii 2
    | CMPNEQ -> ii 2
    | CMPLT -> ii 2
    | CMPGT -> ii 2
    | CMPLE -> ii 2
    | CMPGE -> ii 2
    | SELF -> ii 0
    | CONTRACT _ -> ii 1
    | TRANSFER_TOKENS -> ii 3
    | SET_DELEGATE -> ii 1
    | CREATE_ACCOUNT -> ii 4
    | CREATE_CONTRACT _ -> ii 3
    | IMPLICIT_ACCOUNT -> ii 1
    | NOW -> ii 0
    | AMOUNT -> ii 0
    | BALANCE -> ii 0
    | CHECK_SIGNATURE -> ii 3
    | BLAKE2B -> ii 1
    | SHA256 -> ii 1
    | SHA512 -> ii 1
    | HASH_KEY -> ii 1
    | STEPS_TO_QUOTA -> ii 0
    | SOURCE -> ii 0
    | SENDER -> ii 0
    | ADDRESS -> ii 1
    | CHAIN_ID -> ii 0
    | APPLY -> ii 2
    

  (** check validity of EMPTY_SET, EMPTY_MAP, and EMPTY_BIG_MAP *)
  let isValidInst : inst -> bool = function
    | EMPTY_SET t1 -> Typ.isComparable t1           (* comp_typ *)
    | EMPTY_MAP (t1, _) -> Typ.isComparable t1      (* (comp_typ * typ) *)
    | EMPTY_BIG_MAP (t1, _) -> Typ.isComparable t1  (* (comp_typ * typ) *)
    | _ -> true

end
