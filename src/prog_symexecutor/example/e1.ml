open Micse_Interpreter

let powerturtle : PureInst.t = 
  IF (
    UPDATE,
    SEQ [
      DROP; DROP; DIP (PUSH (SCT_nat, PureInstUtil.d_int 0) );
      ITER ( SEQ [
        DUP;
        PUSH (SCT_nat, PureInstUtil.d_int 4);
        SUB;
        GT;
        IF (
          ADD, 
          DROP
        );
      ]);
      SWAP;
      LSL;
      EMPTY_MAP (SCT_nat, SCT_nat);
    ]
  )
  