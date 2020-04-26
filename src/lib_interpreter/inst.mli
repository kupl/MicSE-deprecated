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
      | Hidden of int list (* not in Michelson, but used for MicSE inner representation without violate verification result. *)
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

  module Make (T : TAG) : S with module Tag = T
