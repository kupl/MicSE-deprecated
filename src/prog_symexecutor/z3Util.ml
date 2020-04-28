open Z3

let gen_cfg () = [("model", "true"); ("proof", "false"); ("timeout", !Main.z3limit);]
let gen_ctx () = gen_cfg () |> mk_context