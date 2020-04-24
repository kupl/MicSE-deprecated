# Michelson Symbolic Executor


## Dependencies
It will use modules and module types from `lib_interpreter`. Following modules are used.

| File or Module Name           | Description |
| -------------------------     | ------------|
| `Typ`                         | Type Representation in Michelson. (nat, string, ...) It does not contains the representation of their values. |
| `Predefined`                  | Implementation of non-common types in Michelson (like timestamp, mutez ...) |
| `PureInst`                    | Instruction Representation (only contains what Michelson Requires) |
| `Inst.TAG`                    | Statically assigned value for each code point |
| `Inst.S`                      | Instruction Representation (Michelson code tagged by Inst.TAG) |
| `Inst.Make`                   | `Inst.TAG` -> `Inst.S` |
| `PlainEvalTrace.IdTag`        | Implementation of `Inst.TAG`. It contains only program-point integer (program-point label) |
| `PlainEvalTrace.PlainInst`    | Implementation of `Inst.S`, which uses `PlainEvalTrace.IdTag` |

Note that the type `PlainEvalTrace.PlainConf.inst_t` is equal to `PlainEvalTrace.PlainInst.t` and the type `PlainEvalTrace.PlainConf.data` is equal to `PlainEvalTrace.PlainInst.data`.


# Code Layout
The list is topological sorted by its module dependency.

| File or Module Name           | Description |
| -------------------------     | ------------|
| `BranchTag`                   | This file implements `Inst.TAG`, `Inst.S`, and `Inst.Make`. |
| `BranchTag.Tag`               | Implementation of `Inst.TAG`. It contains program-point, and branch information to access to the instruction. |
| `BranchTag.Inst`              | Implementation of `Inst.S`, which uses `BranchTag.Tag` |


