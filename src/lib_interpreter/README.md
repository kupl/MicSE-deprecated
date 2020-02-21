# Michelson Interpreter

## Run Demo
This example will show the full trace of the simple Michelson code execution in one transaction. This transaction adds 12 to the head of the pair(12,123), which results in (24,123). You can see the full Michelson code at `test/examples.ml`.
```
dune exec test/demo.exe
```

# Code Layout
The list is topological sorted by its module dependency.

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
| `Config.EXTEND`               | Dynamically collected value when run Michelson code. |
| `Config.S`                    | The reprentation of single state in runtime. It contains the code(`Inst.S.t`) to run in current state, current stack(`Inst.S.data`), collected tag(`Inst.TAG`) to trace its execution path, and the extra information(`Config.EXTEND`) collected at runtime. |
| `Config.Make`                 | `Inst.S` -> `Config.EXTEND` -> `Config.S` |
| `PlainEvalTrace.RunExt`       | Implementation of `Config.EXTEND`. It contains a single integer, which will be used for counting the number of instructions while executing the `PlainEvalTrace.eval_inst` function.
| `PlainEvalTrace.PlainConf`    | Implementation of `Config.S`, which uses `PlainEvalTrace.IdTag` and `PlainEvalTrace.RunExt`.
