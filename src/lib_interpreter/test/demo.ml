open Micse_Interpreter.Inst
open Micse_Interpreter.Config
open Micse_Interpreter.PlainEvalTrace
open Micse_Interpreter.PlainEvalTraceUtil
open Examples
open RandomInput

let ex1_code, n1 = inst_t_of_PureInst Ex1.code 0
let _ = "Ex1 code ends with number: " ^ string_of_int n1 |> print_endline
let ex1_storage, n2 = data_of_PureData Ex1.storage n1
let ex1_input1, nn1 = data_of_PureData Ex1.input1 n2
let ex1_input2, nn2 = data_of_PureData Ex1.input2 nn1
let ex1_input3, nn3 = data_of_PureData Ex1.input3 nn2
let ex1_input4, nn4 = data_of_PureData Ex1.input4 nn3
let ex1_input5, nn5 = data_of_PureData Ex1.input5 nn4
let ex1_input6, nn6 = data_of_PureData Ex1.input6 nn5


let initConf : PlainConf.t = {
  code = ex1_code;
  stack = packstack Ex1.inputTyp ex1_input1 Ex1.storageTyp ex1_storage;
  trace = [];
  extra = 0;
  }

let firstResult = 
  try 
    eval_inst 0 initConf 
  with 
  | PlainConf.TypeError conf -> 
    print_newline ();
    print_endline "<<TypeError>>";
    conf
let _ = 
  print_newline ();
  print_endline "RESULT:";
  print_endline (string_of_stack_t firstResult.stack ";\n" true);
  print_endline (traceStr_of_conf firstResult);
  ()
