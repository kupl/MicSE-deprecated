(*****************************************************************************)
(*****************************************************************************)
(* "Welcome" example code in smartpy.io                                      *)
(*****************************************************************************)
(*****************************************************************************)

module Typ = Micse_Interpreter.Typ
module PureInst = Micse_Interpreter.PureInst
open Micse_Interpreter.PureInstUtil

(*
[INITIAL STORAGE]
(Pair 12 123)

[CODE]
parameter int;
storage   (pair (int %myParameter1) (int %myParameter2));
code
  {
    DUP;        # pair(params, storage).pair(params, storage)
    CDR;        # storage.pair(params, storage)
    SWAP;       # pair(params, storage).storage
    CAR;        # params.storage
    # Entry point: myEntryPoint # params.storage
    # sp.verify(self.data.myParameter1 <= 123) # params.storage
    PUSH int 123; # int.params.storage
    DIG 2;      # storage.int.params
    DUP;        # storage.storage.int.params
    DUG 3;      # storage.int.params.storage
    CAR;        # int.int.params.storage
    COMPARE;    # int.params.storage
    LE;         # bool.params.storage
    IF
      {}
      {
        PUSH string "WrongCondition: self.data.myParameter1 <= 123"; # string.params.storage
        FAILWITH;   # FAILED
      }; # params.storage
    # self.data.myParameter1 += params # params.storage
    SWAP;       # storage.params
    DUP;        # storage.storage.params
    DUG 2;      # storage.params.storage
    SWAP;       # params.storage.storage
    DUP;        # params.params.storage.storage
    DUG 2;      # params.storage.params.storage
    SWAP;       # storage.params.params.storage
    DUP;        # storage.storage.params.params.storage
    DUG 2;      # storage.params.storage.params.storage
    CAR;        # int.params.storage.params.storage
    ADD;        # int.storage.params.storage
    SWAP;       # storage.int.params.storage
    CDR;        # int.int.params.storage
    SWAP;       # int.int.params.storage
    PAIR;       # pair int int.params.storage
    DUG 2;      # params.storage.pair int int
    DROP;       # storage.pair int int
    DROP;       # pair int int
    NIL operation; # list operation.pair int int
    PAIR;       # pair (list operation) (pair int int)
  } # pair (list operation) (pair int int);

[EXAMPLE-TRANSACTIONS (6 in a row)]
1.  Input: 12 
    Result: (24, 123)
2.  Input: 13
    Result: (37, 123)
3.  Input: 14
    Result: (51, 123)
4.  Input: 50
    Result: (101,123)
5.  Input: 50
    Result: (151, 123)
6.  Input: 50
    Result: FAILED - "WrongCondition: self.data.myParameter1 <= 123"

[MINIMALIZED CODE]
  { DUP; CDR; SWAP; CAR;
    PUSH int 123;
    DIG 2;
    DUP;
    DUG 3;
    CAR; COMPARE; LE;
    IF
      {}
      {
        PUSH string "WrongCondition: self.data.myParameter1 <= 123";
        FAILWITH;
      };
    SWAP; DUP; DUG 2;
    SWAP; DUP; DUG 2;
    SWAP; DUP; DUG 2;
    CAR; ADD; SWAP; CDR; SWAP; PAIR;
    DUG 2;
    DROP; DROP;
    NIL operation;
    PAIR;
  }
*)
module Ex1 = struct
  let storageTyp : Typ.t = T_pair (SCT_int, SCT_int)
  let inputTyp : Typ.t = SCT_int

  let storage : PureInst.data = D_Pair (D_Int (Z.of_int 12), D_Int (Z.of_int 123))
  
  let input1 = d_int 12
  let input2 = d_int 13
  let input3 = d_int 14
  let input4 = d_int 50
  let input5 = d_int 50
  let input6 = d_int 50 (* input6 will cause error if you execute input1 ~ input6 in a row. *)

  let code : PureInst.t = 
    SEQ [
      DUP; CDR; SWAP; CAR;
      PUSH (SCT_int, d_int 123);
      DIG (inat_int 2);
      DUP;
      DUG (inat_int 3);
      CAR; COMPARE; LE;
      IF(
        SEQ [],
        SEQ [ PUSH (SCT_string, D_String "WrongCondition: self.data.myParameter1 <= 123");
          FAILWITH;]
      );
      SWAP; DUP; DUG (inat_int 2);
      SWAP; DUP; DUG (inat_int 2);
      SWAP; DUP; DUG (inat_int 2);
      CAR; ADD; SWAP; CDR; SWAP; PAIR;
      DUG (inat_int 2);
      DROP; DROP;
      NIL T_operation; PAIR;        
    ]

end


(*****************************************************************************)
(*****************************************************************************)
(* TODO: interpret manager.tz                                                *)
(*****************************************************************************)
(*****************************************************************************)
(*
[INITIAL STORAGE (example)]
tz1hWTBRzt39itjRz3JAcTFz8sLSh3A27FF5

[CODE]
parameter 
  or(
    lambda %do (
      unit;
      list; [operation;]

    );
    unit %default
  )

storage   key_hash;

code
  DUP; CAR; DIP; {CDR; } 
  IF_LEFT; { 
    PUSH; { mutez; } AMOUNT; 
    COMPARE; EQ; 
    IF; 
      {UNIT; FAILWITH; }
      DIP; 
    { DUP; } SWAP; IMPLICIT_ACCOUNT; ADDRESS; 
    SENDER; COMPARE; EQ; 
    IF; 
      {UNIT; FAILWITH; } 
      UNIT; 
    EXEC; PAIR; DROP; NIL; { operation; } 
    PAIR; 
} 

[EXAMPLE-TRANSACTIONS]

[MINIMALIZED CODE]
  {
    
  }
*)
