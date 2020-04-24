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
  

## Run Interpreter Demo
```
dune clean
dune exec src/lib_interpreter/test/demo.exe
```
The interpreter will simulate a single transaction of the "Welcome Example" Michelson code in [examples.ml](src/lib_interpreter/test/examples.ml), prints the transaction's traces and the stack changes.

To show every warning, you need to modify `dune` file located at project root. Replace `(flags (:standard -w -A))` into `(flags (:standard -warn-error -A))`.


## Roadmap
* Interpreter
  * Demo available :white_check_mark:
  * Check our Interpreter code [here](src/lib_interpreter/)
* Symbolic Execution Engine
* Invariant Generator
* Verification Condition Generator
* Transaction Scenario Generator
* Integrated Tool

