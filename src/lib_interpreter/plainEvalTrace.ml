open Predefined

module IdTag = struct
  type t = int
  let to_string = string_of_int
  let eq = (=)
end

module PlainInst = Inst.Make (IdTag)

module RunExt = struct
  type t = int  (* # of executed instructions *)
  let init_value = 0
end

module PlainConf = Config.Make (PlainInst) (RunExt)
open PlainConf (* open module to write exception module-name less *)

(*****************************************************************************)
(*****************************************************************************)
(* PureInst Convert                                                          *)
(*****************************************************************************)
(*****************************************************************************)
(* Convert pureInst into PlainConf *)
let rec inst_t_of_PureInst : PureInst.t -> int -> (PlainConf.inst_t * int) =
  let makeT i n : PlainConf.inst_t * int = {inst = i; tag = n;}, (n+1) in
  let makeT2 i n m : PlainConf.inst_t * int = {inst = i; tag = n;}, m in
  fun ist n -> begin
    match ist with
    | PureInst.SEQ ilist ->
      let foldFunc (iilist, nn) pureinst = 
        let ii, nnn = inst_t_of_PureInst pureinst nn in
        (ii :: iilist, nnn) 
      in
      let iilist, nn = List.fold_left foldFunc ([], n+1) ilist in
      makeT2 (SEQ (List.rev iilist)) n nn
    | PureInst.DROP -> makeT DROP n
    | PureInst.DROP_N inat -> makeT (DROP_N inat) n 
    | PureInst.DUP -> makeT DUP n
    | PureInst.SWAP -> makeT SWAP n
    | PureInst.DIG inat -> makeT (DIG inat) n
    | PureInst.DUG inat -> makeT (DUG inat) n
    | PureInst.PUSH (t, d) -> 
      let dd, nn = data_of_PureData d (n+1) in
      makeT2 (PUSH (t, dd)) n nn
    | PureInst.SOME -> makeT SOME n
    | PureInst.NONE t -> makeT (NONE t) n
    | PureInst.UNIT -> makeT UNIT n
    | PureInst.IF_NONE (i1, i2) ->
      let ii1, n1 = inst_t_of_PureInst i1 (n+1) in
      let ii2, n2 = inst_t_of_PureInst i2 n1 in
      makeT2 (IF_NONE (ii1, ii2)) n n2
    | PureInst.PAIR -> makeT PAIR n
    | PureInst.CAR -> makeT CAR n
    | PureInst.CDR -> makeT CDR n
    | PureInst.LEFT t -> makeT (LEFT t) n
    | PureInst.RIGHT t -> makeT (RIGHT t) n
    | PureInst.IF_LEFT (i1, i2) ->
      let ii1, n1 = inst_t_of_PureInst i1 (n+1) in
      let ii2, n2 = inst_t_of_PureInst i2 n1 in
      makeT2 (IF_LEFT (ii1, ii2)) n n2
    | PureInst.NIL t -> makeT (NIL t) n
    | PureInst.CONS -> makeT CONS n
    | PureInst.IF_CONS (i1, i2) ->
      let ii1, n1 = inst_t_of_PureInst i1 (n+1) in
      let ii2, n2 = inst_t_of_PureInst i2 n1 in
      makeT2 (IF_CONS (ii1, ii2)) n n2
    | PureInst.SIZE -> makeT SIZE n
    | PureInst.EMPTY_SET t -> makeT (EMPTY_SET t) n
    | PureInst.EMPTY_MAP (t1, t2) -> makeT (EMPTY_MAP (t1, t2)) n
    | PureInst.EMPTY_BIG_MAP (t1, t2) -> makeT (EMPTY_BIG_MAP (t1, t2)) n
    | PureInst.MAP i ->
      let ii1, n1 = inst_t_of_PureInst i (n+1) in
      makeT2 (MAP ii1) n n1
    | PureInst.ITER i ->
      let ii1, n1 = inst_t_of_PureInst i (n+1) in
      makeT2 (ITER ii1) n n1
    | PureInst.MEM -> makeT MEM n
    | PureInst.GET -> makeT GET n
    | PureInst.UPDATE -> makeT UPDATE n
    | PureInst.IF (i1, i2) ->
      let ii1, n1 = inst_t_of_PureInst i1 (n+1) in
      let ii2, n2 = inst_t_of_PureInst i2 n1 in
      makeT2 (IF (ii1, ii2)) n n2
    | PureInst.IFEQ  (i1, i2) -> inst_t_of_PureInst (PureInst.SEQ [EQ; IF (i1, i2)]) n
    | PureInst.IFNEQ (i1, i2) -> inst_t_of_PureInst (PureInst.SEQ [NEQ; IF (i1, i2)]) n
    | PureInst.IFLT  (i1, i2) -> inst_t_of_PureInst (PureInst.SEQ [LT; IF (i1, i2)]) n
    | PureInst.IFGT  (i1, i2) -> inst_t_of_PureInst (PureInst.SEQ [GT; IF (i1, i2)]) n
    | PureInst.IFLE  (i1, i2) -> inst_t_of_PureInst (PureInst.SEQ [LE; IF (i1, i2)]) n
    | PureInst.IFGE  (i1, i2) -> inst_t_of_PureInst (PureInst.SEQ [GE; IF (i1, i2)]) n
    | PureInst.LOOP i ->
      let ii1, n1 = inst_t_of_PureInst i (n+1) in
      makeT2 (LOOP ii1) n n1
    | PureInst.LOOP_LEFT i ->
      let ii1, n1 = inst_t_of_PureInst i (n+1) in
      makeT2 (LOOP_LEFT ii1) n n1
    | PureInst.LAMBDA (t1, t2, i) ->
      let ii1, n1 = inst_t_of_PureInst i (n+1) in
      makeT2 (LAMBDA (t1, t2, ii1)) n n1
    | PureInst.EXEC -> makeT EXEC n
    | PureInst.DIP i ->
      let ii1, n1 = inst_t_of_PureInst i (n+1) in
      makeT2 (DIP ii1) n n1
    | PureInst.DIP_N (inat, i) ->
      let ii1, n1 = inst_t_of_PureInst i (n+1) in
      makeT2 (DIP_N (inat, ii1)) n n1
    | PureInst.FAILWITH -> makeT FAILWITH n
    | PureInst.FAIL -> inst_t_of_PureInst (PureInst.SEQ [UNIT; FAILWITH]) n
    | PureInst.CAST t -> makeT (CAST t) n
    | PureInst.RENAME -> makeT RENAME n
    | PureInst.CONCAT -> makeT CONCAT n
    | PureInst.SLICE -> makeT SLICE n
    | PureInst.PACK -> makeT PACK n
    | PureInst.UNPAIR -> makeT UNPAIR n
    | PureInst.UNPACK t -> makeT (UNPACK t) n
    | PureInst.ADD -> makeT ADD n
    | PureInst.SUB -> makeT SUB n
    | PureInst.MUL -> makeT MUL n
    | PureInst.EDIV -> makeT EDIV n
    | PureInst.ABS -> makeT ABS n
    | PureInst.ISNAT -> makeT ISNAT n
    | PureInst.INT -> makeT INT n
    | PureInst.NEG -> makeT NEG n
    | PureInst.LSL -> makeT LSL n
    | PureInst.LSR -> makeT LSR n
    | PureInst.OR -> makeT OR n
    | PureInst.AND -> makeT AND n
    | PureInst.XOR -> makeT XOR n
    | PureInst.NOT -> makeT NOT n
    | PureInst.COMPARE -> makeT COMPARE n
    | PureInst.EQ -> makeT EQ n
    | PureInst.NEQ -> makeT NEQ n
    | PureInst.LT -> makeT LT n
    | PureInst.GT -> makeT GT n
    | PureInst.LE -> makeT LE n
    | PureInst.GE -> makeT GE n
    | PureInst.CMPEQ  -> inst_t_of_PureInst (PureInst.SEQ [COMPARE; EQ]) n
    | PureInst.CMPNEQ -> inst_t_of_PureInst (PureInst.SEQ [COMPARE; NEQ]) n
    | PureInst.CMPLT  -> inst_t_of_PureInst (PureInst.SEQ [COMPARE; LT]) n
    | PureInst.CMPGT  -> inst_t_of_PureInst (PureInst.SEQ [COMPARE; GT]) n
    | PureInst.CMPLE  -> inst_t_of_PureInst (PureInst.SEQ [COMPARE; LE]) n
    | PureInst.CMPGE  -> inst_t_of_PureInst (PureInst.SEQ [COMPARE; GE]) n
    | PureInst.SELF -> makeT SELF n
    | PureInst.CONTRACT t -> makeT (CONTRACT t) n
    | PureInst.TRANSFER_TOKENS -> makeT TRANSFER_TOKENS n
    | PureInst.SET_DELEGATE -> makeT SET_DELEGATE n
    | PureInst.CREATE_ACCOUNT -> makeT CREATE_ACCOUNT n
    | PureInst.CREATE_CONTRACT i ->
      let ii1, n1 = inst_t_of_PureInst i (n+1) in
      makeT2 (CREATE_CONTRACT ii1) n n1
    | PureInst.IMPLICIT_ACCOUNT -> makeT IMPLICIT_ACCOUNT n
    | PureInst.NOW -> makeT NOW n
    | PureInst.AMOUNT -> makeT AMOUNT n
    | PureInst.BALANCE -> makeT BALANCE n
    | PureInst.CHECK_SIGNATURE -> makeT CHECK_SIGNATURE n
    | PureInst.BLAKE2B -> makeT BLAKE2B n
    | PureInst.SHA256 -> makeT SHA256 n
    | PureInst.SHA512 -> makeT SHA512 n
    | PureInst.HASH_KEY -> makeT HASH_KEY n
    | PureInst.STEPS_TO_QUOTA -> makeT STEPS_TO_QUOTA n
    | PureInst.SOURCE -> makeT SOURCE n
    | PureInst.SENDER -> makeT SENDER n
    | PureInst.ADDRESS -> makeT ADDRESS n
    | PureInst.CHAIN_ID -> makeT CHAIN_ID n
    | PureInst.APPLY -> makeT APPLY n
  end
and data_of_PureData : PureInst.data -> int -> (PlainConf.data * int) =
  (* Since "IdTag" value can be written only at D_Lambda, 
      every comparable (or simple-comparable) types does not contains "IdTag" value.
      So D_Set's elements or D_Map's keys are not major consideration in this function.
      However, nested PureInst.data in D_Set's elements should be converted into
      PlainConf.data too.
  *)
  fun d n -> begin
    match d with
    | PureInst.D_Lambda i -> let ii1, n1 = inst_t_of_PureInst i n in D_Lambda ii1, n1
    | PureInst.D_Left d1 -> let dd1, n1 = data_of_PureData d n in D_Left dd1, n1
    | PureInst.D_Right d1 -> let dd1, n1 = data_of_PureData d n in D_Right dd1, n1
    | PureInst.D_Some d1 -> let dd1, n1 = data_of_PureData d n in D_Some dd1, n1
    | PureInst.D_Set dset ->
      let foldFunc : PureInst.data -> (PlainConf.data BatSet.t * int) -> (PlainConf.data BatSet.t * int) =
        fun pd (bs, nn) ->
        let pdd, nnn = data_of_PureData pd nn in
        (BatSet.add pdd bs, nnn)
      in
      let bs, nn = BatSet.fold foldFunc dset (BatSet.empty, n) in
      D_Set bs, nn
    | PureInst.D_Map dmap ->
      let foldiFunc : 
        PureInst.data -> PureInst.data ->
        ((PlainConf.data, PlainConf.data) BatMap.t * int) ->
        ((PlainConf.data, PlainConf.data) BatMap.t * int)
        =
        fun pk pe (bm, nn) ->
        let ppk, nnn1 = data_of_PureData pk nn in
        let ppe, nnn2 = data_of_PureData pe nnn1 in
        BatMap.add ppk ppe bm, nnn2
      in
      let bm, nn = BatMap.foldi foldiFunc dmap (BatMap.empty, n) in
      D_Map bm, nn
    | PureInst.D_Bigmap dmap ->
      (* For map-kind *)
      let foldiFunc : 
        PureInst.data -> PureInst.data ->
        ((PlainConf.data, PlainConf.data) BatMap.t * int) ->
        ((PlainConf.data, PlainConf.data) BatMap.t * int)
        =
        fun pk pe (bm, nn) ->
        let ppk, nnn1 = data_of_PureData pk nn in
        let ppe, nnn2 = data_of_PureData pe nnn1 in
        BatMap.add ppk ppe bm, nnn2
      in
      let bm, nn = BatMap.foldi foldiFunc dmap (BatMap.empty, n) in
      D_Bigmap bm, nn
    | PureInst.D_List dlist ->
      let foldFunc : PureInst.data -> (PlainConf.data list * int) -> (PlainConf.data list * int) =
        fun pd (bl, nn) ->
        let ppd, nnn = data_of_PureData pd nn in
        ppd :: bl, nnn
      in
      let bl, nn = List.fold_right foldFunc dlist ([], n) in
      D_List bl, nn
    | PureInst.D_Pair (d1, d2) ->
      let dd1, nn1 = data_of_PureData d1 n in
      let dd2, nn2 = data_of_PureData d2 nn1 in
      D_Pair (dd1, dd2), nn2
    (* others *)
    | PureInst.D_Int z -> D_Int z, n
    | PureInst.D_String s -> D_String s, n
    | PureInst.D_Bytes b -> D_Bytes b, n
    | PureInst.D_Unit -> D_Unit, n
    | PureInst.D_True -> D_True, n
    | PureInst.D_False -> D_False, n
    | PureInst.D_None -> D_None, n
    | PureInst.D_Mutez i64 -> D_Mutez i64, n
    | PureInst.D_Contract ct -> D_Contract ct, n
    | PureInst.D_Address ad -> D_Address ad, n
    | PureInst.D_Key k -> D_Key k, n
    | PureInst.D_KeyHash kh -> D_KeyHash kh, n
    | PureInst.D_Signature s -> D_Signature s, n
    | PureInst.D_Chain_id ci -> D_Chain_id ci, n
    | PureInst.D_Operation o -> D_Operation o, n
  end

(*****************************************************************************)
(*****************************************************************************)
(* To String                                                                 *)
(*****************************************************************************)
(*****************************************************************************)
(* make value to string *)
let rec string_of_inst_t : PlainConf.inst_t -> bool -> string =
  fun istt tagFlag -> failwith "plainEvalTrace.ml : string_of_inst_t : Not implemented"
and string_of_inst : PlainConf.InstM.inst -> bool -> string =
  fun ist tagFlag -> failwith "plainEvalTrace.ml : string_of_inst : Not implemented"
and string_of_data : PlainConf.data -> bool -> string =
  fun d tagFlag ->
  match d with
  | D_Int z -> "D_Int " ^ Z.to_string z
  | D_String s -> "D_String " ^ "\"" ^ s ^ "\""
  | D_Bytes b -> "D_Bytes 0x" ^ Bytes.to_string b
  | D_Unit -> "D_Unit"
  | D_True -> "D_True"
  | D_False -> "D_False"
  | D_Pair (d1, d2) -> "D_Pair (" ^ string_of_data d1 tagFlag ^ ", " ^ string_of_data d2 tagFlag ^ ")"
  | D_Left d -> "D_Left (" ^ string_of_data d tagFlag ^ ")"
  | D_Right d -> "D_Right (" ^ string_of_data d tagFlag ^ ")"
  | D_Some d -> "D_Some (" ^ string_of_data d tagFlag ^ ")"
  | D_None -> "D_None"
  | D_List dlist -> 
    StringUtil.string_of_seq_tpl
      (fun s -> "D_List [" ^ s ^ "]")
      (fun (d : PlainConf.data) (tFlag : bool) -> string_of_data d tFlag)
      "; "
      tagFlag
      dlist
  | D_Set dset ->
    StringUtil.string_of_seq_tpl
      (fun s -> "D_Set (BatSet.of_list [" ^ s ^ "])")
      (fun (d : PlainConf.data) (tFlag : bool) -> string_of_data d tFlag)
      "; "
      tagFlag
      (BatSet.to_list dset)
  | D_Map dmap ->
  (* NOTICE: Map and BigMap can't be directly converted to OCaml code *)
    let foldiFunc : PlainConf.data -> PlainConf.data -> string -> string =
      fun pd1 pd2 accStr ->
      let pd1s = string_of_data pd1 tagFlag in
      let pd2s = string_of_data pd2 tagFlag in
      accStr ^ "(" ^ pd1s ^ ", " ^ pd2s ^ ");"
    in
    let body = BatMap.foldi foldiFunc dmap "" in
    "D_Map (d_map_of_list [" ^ body ^ "])"
  | D_Bigmap dmap ->
    let foldiFunc : PlainConf.data -> PlainConf.data -> string -> string =
      fun pd1 pd2 accStr ->
      let pd1s = string_of_data pd1 tagFlag in
      let pd2s = string_of_data pd2 tagFlag in
      accStr ^ "(" ^ pd1s ^ ", " ^ pd2s ^ ");"
    in
    let body = BatMap.foldi foldiFunc dmap "" in
    "D_Bigmap (d_bigmap_of_list [" ^ body ^ "])"
  | D_Lambda i -> "D_Lambda (" ^ string_of_inst_t i tagFlag ^ ")"

  | D_Mutez mtz -> failwith "plainEvalTrace.ml : string_of_data : D_Mutez : Not implemented"
  | D_Contract ctr -> failwith "plainEvalTrace.ml : string_of_data : D_Contract : Not implemented"
  | D_Address addr -> failwith "plainEvalTrace.ml : string_of_data : D_Address : Not implemented"
  | D_Key k -> failwith "plainEvalTrace.ml : string_of_data : D_Key : Not implemented"
  | D_KeyHash kh -> failwith "plainEvalTrace.ml : string_of_data : D_KeyHash : Not implemented"
  | D_Signature sg -> failwith "plainEvalTrace.ml : string_of_data : D_Signature : Not implemented"
  | D_Chain_id cid -> failwith "plainEvalTrace.ml : string_of_data : D_Chain_id : Not implemented"
  | D_Operation op -> failwith "plainEvalTrace.ml : string_of_data : D_Operation : Not implemented"

let string_of_stack_t : PlainConf.stack_t -> string -> bool -> string =
  fun stt delim tagFlag ->
  StringUtil.string_of_seq_tpl
    (fun s -> "[" ^ s ^ "]")
    (fun (sElem : Typ.t * PlainConf.InstM.data) (tagFlag : bool) -> 
      match sElem with
      | t, d -> "" ^ Typ.to_string t ^ " ( " ^ string_of_data d tagFlag ^ " )"
    )
    delim
    tagFlag
    stt


(*****************************************************************************)
(*****************************************************************************)
(* Evaluation                                                                *)
(*****************************************************************************)
(*****************************************************************************)
let no_op : PlainConf.inst_t = {inst = DROP_N (Predefined.InstNat.zero); tag = (-1);}

(* Evaluation with execute-count limit. Written in Big-step semantics *)
let rec eval_inst : int -> PlainConf.t -> PlainConf.t =
  fun exe_limit pre ->

  (* DEBUGGING-PURPOSE-LOGGING START *)
  let _ = print_newline () in
  let bodystring = List.fold_left (fun acc x -> acc ^ (string_of_int x) ^ "; ") "" (List.rev pre.trace) in
  let _ = print_endline ("TRACE: " ^ bodystring) in
  let stackstring = (string_of_stack_t pre.stack ";\n" true) in
  let _ = print_endline ("STACK:\n" ^ stackstring) in
  (* DEBUGGING-PURPOSE-LOGGING END *)

  if (exe_limit > 0) && (not (pre.extra < exe_limit)) then raise (Exec_limit pre)
  else begin
    let newtrace = pre.code.tag :: pre.trace in
    let newextra = pre.extra + 1 in
    let post s : PlainConf.t = {code = no_op; stack = s; trace = newtrace; extra = newextra;} in
    match pre.code.inst, pre.stack with

    | SEQ [], _ -> post pre.stack
    | SEQ cl, _ -> 
      let seqEval_foldfunc : PlainConf.t -> PlainConf.inst_t -> PlainConf.t =
        fun acc_conf elem_inst ->
        let next_conf = {acc_conf with code = elem_inst;} in
        eval_inst exe_limit next_conf
      in
      List.fold_left seqEval_foldfunc (post pre.stack) cl

    | DROP, sh :: st -> post st
    | DROP_N n, sl -> 
      let rec dropn : InstNat.t -> PlainConf.stack_t -> PlainConf.stack_t =
        fun k sl -> if InstNat.(<=) k 0 then sl else dropn (InstNat.(-) k 1) (List.tl sl) in
      let newstack = try dropn n sl with _ -> raise (NotEnough_stack pre) in
      post newstack

    | DUP, sh :: st -> post (sh :: sh :: st)

    | SWAP, sh1 :: sh2 :: st -> post (sh2 :: sh1 :: st)

    | DIG n, sl -> 
      let rec dign : InstNat.t -> PlainConf.stack_t -> PlainConf.stack_t -> PlainConf.stack_t =
        fun k shl stl ->
        if InstNat.(<=) k 0 
        then List.hd stl :: (List.rev shl @ List.tl stl)
        else dign (InstNat.(-) k 1) (List.hd stl :: shl) (List.tl stl)
      in
      let newstack = try dign n [] sl with _ -> raise (NotEnough_stack pre) in
      post newstack

    | DUG n, sh :: st ->
      let rec dugn : InstNat.t -> PlainConf.stack_t -> PlainConf.stack_t -> PlainConf.stack_t =
        fun k shl stl ->
        if InstNat.(<=) k 0
        then List.rev shl @ (sh :: stl)
        else dugn (InstNat.(-) k 1) (List.hd stl :: shl) (List.tl stl)
      in
      let newstack = try dugn n [] st with _ -> raise (NotEnough_stack pre) in
      post newstack

    | PUSH (t, d), sl -> post ((t, d) :: sl)

    | SOME, (sht, shd) :: st -> post ((T_option sht, D_Some shd) :: st)

    | NONE t, sl -> post ((T_option t, D_None) :: sl)

    | UNIT, sl -> post ((T_unit, D_Unit) :: sl)

    | IF_NONE (i1, _), (T_option _, D_None) :: st -> 
      eval_inst exe_limit {code = i1; stack = st; trace = newtrace; extra = newextra;}
    | IF_NONE (_, i2), (T_option shto, D_Some shdo) :: st ->
      eval_inst exe_limit {code = i2; stack = (shto, shdo) :: st; trace = newtrace; extra = newextra;}
    | IF_NONE _, _ :: st -> raise (TypeError pre)

    | PAIR, (sh1t, sh1d) :: (sh2t, sh2d) :: st ->
      post ((Typ.genPairTyp sh1t sh2t, D_Pair (sh1d, sh2d)) :: st)

    | CAR, (CT_pair (sh1ht, _), D_Pair (sh1hd, _)) :: st -> post ((sh1ht, sh1hd) :: st)
    | CAR, (T_pair (sh1ht, _), D_Pair (sh1hd, _)) :: st -> post ((sh1ht, sh1hd) :: st)
    | CAR, _ :: st -> raise (TypeError pre)

    | CDR, (CT_pair (_, sh1tt), D_Pair (_, sh1td)) :: st -> post ((sh1tt, sh1td) :: st)
    | CDR, (T_pair (_, sh1tt), D_Pair (_, sh1td)) :: st -> post ((sh1tt, sh1td) :: st)
    | CDR, _ :: st -> raise (TypeError pre)

    | LEFT t, (sht, shd) :: st -> post ((T_or (sht, t), D_Left shd) :: st)

    | RIGHT t, (sht, shd) :: st -> post ((T_or (t, sht), D_Right shd) :: st)

    | IF_LEFT (i1, _), (T_or (shlt, _), D_Left shd) :: st ->
      eval_inst exe_limit {code = i1; stack = (shlt, shd) :: st; trace = newtrace; extra = newextra;}
    | IF_LEFT (_, i2), (T_or (_, shrt), D_Right shd) :: st ->
      eval_inst exe_limit {code = i2; stack = (shrt, shd) :: st; trace = newtrace; extra = newextra;}
    | IF_LEFT _, _ :: st -> raise (TypeError pre)

    | NIL t, sl -> post ((T_list t, D_List []) :: sl)

    | CONS, (sh1t, sh1d) :: (T_list sh2t, D_List sh2d) :: sl when Typ.isEqTyp sh1t sh2t ->
      post ((T_list sh2t, D_List (sh1d :: sh2d)) :: sl)
    | CONS, _ :: _ :: sl -> raise (TypeError pre)

    | IF_CONS (i1, _), (T_list shtl, D_List (shdh :: shdt)) :: st -> 
      eval_inst exe_limit {code = i1; stack = (shtl, shdh) :: (T_list shtl, D_List shdt) :: st; trace = newtrace; extra = newextra;}
    | IF_CONS (_, i2) , (T_list _, D_List []) :: st ->
      eval_inst exe_limit {code = i2; stack = st; trace = newtrace; extra = newextra;}
    | IF_CONS _, _ :: st -> raise (TypeError pre)

    | SIZE, (SCT_string, D_String str_d) :: st ->
      let sz : Z.t = Z.of_int (String.length str_d) in
      post ((SCT_nat, D_Int sz) :: st)
    | SIZE, (SCT_bytes, D_Bytes byt_d) :: st ->
      let sz : Z.t = Z.of_int (Bytes.length byt_d) in
      post ((SCT_nat, D_Int sz) :: st)
    | SIZE, (T_list _, D_List shdl) :: st ->
      let sz : Z.t = Z.of_int (List.length shdl) in
      post ((SCT_nat, D_Int sz) :: st)
    | SIZE, (T_set _, D_Set shds) :: st ->
      let sz : Z.t = Z.of_int (BatSet.cardinal shds) in 
      post ((SCT_nat, D_Int sz) :: st)
    | SIZE, _ :: st -> raise (TypeError pre)

    | EMPTY_SET ct, sl -> 
      if not (Typ.isComparable ct) then raise (TypeError pre)
      else post ((T_set ct, D_Set BatSet.empty) :: sl)
    
    | EMPTY_MAP (ct, t), sl ->
      if not (Typ.isComparable ct) then raise (TypeError pre)
      else post ((T_map (ct, t), D_Map BatMap.empty) :: sl)

    | EMPTY_BIG_MAP (ct, t), sl ->
      if not (Typ.isComparable ct) then raise (TypeError pre)
      else post ((T_bigmap (ct, t), D_Bigmap BatMap.empty) :: sl)
    
    | MAP i, (T_list shtl, D_List shdl) :: st -> raise (Not_implemented pre)
    | MAP i, (T_map (sht1, sht2), D_Map shdm) :: st ->
      if not (Typ.isValidTyp (T_map (sht1, sht2))) then raise (TypeError pre)
      else begin
        raise (Not_implemented pre)
      end
    | MAP _, _ :: st -> raise (TypeError pre)

    | ITER i, (T_list shtl, D_List shdl) :: st -> 
      let foldFunc (accConf : PlainConf.t) v : PlainConf.t =
        eval_inst exe_limit {accConf with code = i; stack = (shtl, v) :: (accConf.stack);} in
      List.fold_left
        foldFunc
        (post (pre.stack))
        shdl
      (* List.fold_left's Type : 
          (PlainConf.t -> d_list_elem_typ -> PlainConf.t) ->
          PlainConf.t ->
          d_list_typ ->
          PlainConf.t
      *)
    | ITER i, (T_set shts, D_Set shds) :: st -> raise (Not_implemented pre)
    | ITER i, (T_map (sht1, sht2), D_Map shdm) :: st ->
      if not (Typ.isValidTyp (T_map (sht1, sht2))) then raise (TypeError pre)
      else begin 
        raise (Not_implemented pre)
        (* IMPLEMENTATION DETAILS 
            1) Make BatEnum.t using BatMap.enum
            2) The MAP's semantics in Michelson looks like fold function, not map. Use BatEnum.fold
            3) Return to BatMap.t from BatMap.enum, using BatMap.of_enum
        *)
        (*
        let enum_val = BatMap.enum shdm in
        let foldFunc (accConf : PlainConf.t) (k, v) : PlainConf.t =
          eval_inst exe_limit (...)
        in
        let result : PlainConf.t = BatEnum.fold foldFunc (...)
        in
        post (...)
        *)
      end
    | ITER _, _ -> raise (TypeError pre)

    | MEM, (sh1t, sh1d) :: (T_set sh2t, D_Set sh2sd) :: st when Typ.isEqTyp sh1t sh2t ->
      let r : bool = try let _ = BatSet.find sh1d sh2sd in true with Not_found -> false in
      let rv : PlainConf.data = if r then D_True else D_False in
      post ((SCT_bool, rv) :: st)
    | MEM, (sh1t, sh1d) :: (T_map (sh2t1, _), D_Map sh2md) :: st when Typ.isEqTyp sh1t sh2t1 ->
      let r : bool = try let _ = BatMap.find sh1d sh2md in true with Not_found -> false in
      let rv : PlainConf.data = if r then D_True else D_False in
      post ((SCT_bool, rv) :: st)
    | MEM, (sh1t, sh1d) :: (T_bigmap (sh2t1, _), D_Bigmap sh2md) :: st when Typ.isEqTyp sh1t sh2t1 ->
      let r : bool = try let _ = BatMap.find sh1d sh2md in true with Not_found -> false in
      let rv : PlainConf.data = if r then D_True else D_False in
      post ((SCT_bool, rv) :: st)
    | MEM, _ :: _ :: st -> raise (TypeError pre)
    
    | GET, _ :: _ :: st -> raise (Not_implemented pre)

    | UPDATE, _ :: _ :: _ :: st -> raise (Not_implemented pre)

    | IF (i1, _), (SCT_bool, D_True) :: st -> 
      eval_inst exe_limit {code = i1; stack = st; trace = newtrace; extra = newextra;}
    | IF (_, i2), (SCT_bool, D_False) :: st ->
      eval_inst exe_limit {code = i2; stack = st; trace = newtrace; extra = newextra;}
    | IF _, _ :: st -> raise (TypeError pre)

    (* IF-MACROS *)
    | IFEQ (i1, i2), _
    | IFNEQ (i1, i2), _
    | IFLT (i1, i2), _
    | IFGT (i1, i2), _
    | IFLE (i1, i2), _
    | IFGE (i1, i2), _ -> raise (Not_implemented pre)

    (* LOOP! *)
    | LOOP i, (SCT_bool, D_True) :: st ->
      let lp = eval_inst exe_limit {code = i; stack = st; trace = newtrace; extra = newextra;} in
      eval_inst exe_limit {lp with code = pre.code;}
    | LOOP i, (SCT_bool, D_False) :: st -> post st
    | LOOP i, _ :: st -> raise (TypeError pre)

    | LOOP_LEFT i, (T_or (shlt, _), D_Left shd) :: st ->
      let lp = eval_inst exe_limit {code = i; stack = (shlt, shd) :: st; trace = newtrace; extra = newextra;} in
      eval_inst exe_limit {lp with code = pre.code;}
    | LOOP_LEFT i, (T_or (_, shrt), D_Right shd) :: st -> post ((shrt, shd) :: st)
    | LOOP_LEFT i, _ :: st -> raise (TypeError pre)

    | LAMBDA (t1, t2, i), sl -> post ((T_lambda (t1, t2), D_Lambda i) :: sl)

    | EXEC, (sh1t, sh1d) :: (T_lambda (sh2lat, sh2lrt), sh2d) :: st when Typ.isEqTyp sh1t sh2lat ->
      raise (Not_implemented pre)
    | EXEC, _ :: _ :: st -> raise (TypeError pre)

    | DIP i, sh :: st -> raise (Not_implemented pre)
    | DIP_N (n, i), sl -> raise (Not_implemented pre)

    | FAILWITH, (SCT_string, D_String failstr) :: st -> raise (Failed_stackstate (pre, failstr))
    | FAILWITH, _ :: st -> raise (Failed_stackstate (pre, ""))

    | FAIL, _ -> raise (Not_implemented pre)

    | CAST t, sh :: st -> raise (Not_implemented pre)

    | RENAME, sh :: st -> raise (Not_implemented pre)

    | CONCAT, (SCT_string, D_String sh1sd) :: (SCT_string, D_String sh2sd) :: st ->
      post ((SCT_string, D_String (sh1sd ^ sh2sd)) :: st)
    | CONCAT, _ :: _ :: st -> raise (TypeError pre)

    | SLICE, (SCT_nat, D_Int sh1id) :: (SCT_nat, D_Int sh2id) :: (SCT_string, D_String sh3sd) :: st ->
      let substrD : PlainConf.data = try D_Some (D_String (String.sub sh3sd (Z.to_int sh1id) (Z.to_int sh2id))) with Invalid_argument _ -> D_None in
      post ((T_option SCT_string, substrD) :: st)
    | SLICE, _ :: _ :: _ :: st -> raise (TypeError pre)

    | PACK, _ :: st -> raise (Not_implemented pre)

    | UNPAIR, _ -> raise (Not_implemented pre)

    | UNPACK t, (SCT_bytes, shbd) :: st -> raise (Not_implemented pre)
    | UNPACK _, _ :: st -> raise (TypeError pre)

    | ADD, (SCT_int, D_Int sh1id) :: (SCT_int, D_Int sh2id) :: st
    | ADD, (SCT_int, D_Int sh1id) :: (SCT_nat, D_Int sh2id) :: st
    | ADD, (SCT_nat, D_Int sh1id) :: (SCT_int, D_Int sh2id) :: st ->
      post ((SCT_int, D_Int (Z.add sh1id sh2id)) :: st)
    | ADD, (SCT_nat, D_Int sh1id) :: (SCT_nat, D_Int sh2id) :: st ->
      post ((SCT_nat, D_Int (Z.add sh1id sh2id)) :: st)
    (*
    (* TODO *)
    | ADD, (SCT_timestamp, D_ (...)) :: (SCT_nat, D_Int sh2id) :: st ->
    | ADD, (SCT_nat, D_Int sh1id) :: (SCT_timestamp, D_ (...)) :: st ->
    *)
    | ADD, (SCT_mutez, D_Mutez sh1i64d) :: (SCT_mutez, D_Mutez sh2i64d) :: st ->
      if (Mutez.compare sh1i64d 0L < 0) || (Mutez.compare sh2i64d 0L < 0)
      then raise (TypeError pre)
      else begin
        if Int64.compare (Int64.sub Int64.max_int sh1i64d) sh2i64d < 0
        then raise (RuntimeErr (pre, Mutez_overflow))
        else post ((SCT_mutez, D_Mutez (Int64.add sh1i64d sh2i64d)) :: st)
      end
    | ADD, _ :: _ :: st -> raise (TypeError pre)

    (* multiple TODOs *)
    | SUB, _
    | MUL, _
    | EDIV, _
    | ABS, _
    | ISNAT, _
    | INT, _
    | NEG, _
    | LSL, _
    | LSR, _
    | OR, _
    | AND, _
    | XOR, _
    | NOT, _ -> raise (Not_implemented pre)

    | COMPARE, (SCT_int, D_Int sh1id) :: (SCT_int, D_Int sh2id) :: st ->
      post ((SCT_int, D_Int (Z.of_int (Z.compare sh1id sh2id))) :: st)
    | COMPARE, (SCT_nat, D_Int sh1nd) :: (SCT_nat, D_Int sh2nd) :: st ->
      post ((SCT_int, D_Int (Z.of_int (Z.compare sh1nd sh2nd))) :: st)
    | COMPARE, (SCT_string, D_String sh1sd) :: (SCT_string, D_String sh2sd) :: st ->
      post ((SCT_int, D_Int (Z.of_int (String.compare sh1sd sh2sd))) :: st)
    | COMPARE, (SCT_bytes, D_Bytes sh1bd) :: (SCT_bytes, D_Bytes sh2bd) :: st ->
      post ((SCT_int, D_Int (Z.of_int (Bytes.compare sh1bd sh2bd))) :: st)
    | COMPARE, (SCT_mutez, D_Mutez sh1i64d) :: (SCT_mutez, D_Mutez sh2i64d) :: st ->
      post ((SCT_int, D_Int (Z.of_int (Mutez.compare sh1i64d sh2i64d))) :: st)
    | COMPARE, (SCT_bool, sh1bd) :: (SCT_bool, sh2bd) :: st ->
      let b2b : PlainConf.data -> bool = function | D_True -> true | D_False -> false | _ -> raise (TypeError pre) in
      post ((SCT_int, D_Int (Z.of_int (compare (b2b sh1bd) (b2b sh2bd)))) :: st)
    | COMPARE, (CT_pair _, D_Pair _) :: (CT_pair _, D_Pair _) :: st ->
      raise (Not_implemented pre)
    (* TODO *)
    (*
    | COMPARE, (SCT_timestamp, D_ (...)) :: (SCT_timestamp, D_ (...)) :: st ->
    *)
    | COMPARE, (SCT_key_hash, D_KeyHash _) :: (SCT_key_hash, D_KeyHash _) :: st ->
      raise (Not_implemented pre)
    | COMPARE, (SCT_address, D_Address _) :: (SCT_address, D_Address _) :: st ->
      raise (Not_implemented pre)
    | COMPARE, _ :: _ :: st ->
      raise (TypeError pre)
    
    | EQ, (SCT_int, D_Int shid) :: st ->
      if Z.equal shid Z.zero
      then post ((SCT_bool, D_True) :: st)
      else post ((SCT_bool, D_False) :: st)
    | EQ, _ :: st -> raise (TypeError pre)

    | NEQ, (SCT_int, D_Int shid) :: st ->
      if Z.equal shid Z.zero
      then post ((SCT_bool, D_False) :: st)
      else post ((SCT_bool, D_True) :: st)
    | NEQ, _ :: st -> raise (TypeError pre)

    | LT, (SCT_int, D_Int shi64d) :: st ->
      if Z.compare shi64d Z.zero < 0
      then post ((SCT_bool, D_True) :: st)
      else post ((SCT_bool, D_False) :: st)
    | LT, _ :: st -> raise (TypeError pre)

    | GT, (SCT_int, D_Int shi64d) :: st ->
      if Z.compare shi64d Z.zero > 0
      then post ((SCT_bool, D_True) :: st)
      else post ((SCT_bool, D_False) :: st)
    | GT, _ :: st -> raise (TypeError pre)

    | LE, (SCT_int, D_Int shi64d) :: st ->
      if Z.compare shi64d Z.zero <= 0
      then post ((SCT_bool, D_True) :: st)
      else post ((SCT_bool, D_False) :: st)
    | LE, _ :: st -> raise (TypeError pre)

    | GE, (SCT_int, D_Int shi64d) :: st ->
      if Z.compare shi64d Z.zero >= 0
      then post ((SCT_bool, D_True) :: st)
      else post ((SCT_bool, D_False) :: st)
    | GE, _ :: st -> raise (TypeError pre)

    (*
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
    | Hidden of int list
    *)
    | _ -> raise (NotEnough_stack pre)
  end
