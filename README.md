# MicSE
Michelson Symbolic vErifier, for verifying and testing the integrity of smart contracts in the Tezos blockchain.


## Dependencies
The `Dockerfile` describes a docker image which can build and run MicSE. `dockerenv.sh` helps bake and run the docker image.
* m4
* libgmp
* opam 2.0
  * OCaml 4.07.1
  * dune
  * batteries
  * zarith



## Roadmap
* Interpreter
  * Demo available :white_check_mark:
  * Check our Interpreter code [here](src/lib_interpreter/)
* Symbolic Execution Engine
* Invariant Generator
* Verification Condition Generator
* Transaction Scenario Generator
* Integrated Tool


## Run Interpreter Demo
```
dune clean
dune exec src/lib_interpreter/test/demo.exe
```
