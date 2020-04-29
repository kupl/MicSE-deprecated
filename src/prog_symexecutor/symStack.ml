open Z3

(*****************************************************************************)
(*****************************************************************************)
(* Symbolic Stack                                                            *)
(*****************************************************************************)
(*****************************************************************************)

type t = int * (int, Expr.expr) BatMap.t

let top : t -> Expr.expr = fun (t, m) -> BatMap.find t m

let size : t -> int = fun (t, _) -> t

let push : t -> Expr.expr -> t = fun (t, m) e -> (t+1, BatMap.add (t+1) e m)

(* Since Michelson type checker guarantees stack size safety, symbolic excution need no safety check. *)
let pop : t -> t = fun (t, m) -> (t-1, m)

let seek_unsafe : t -> int -> Expr.expr = fun (t, m) n -> (try BatMap.find n m with _ -> failwith "symStack.ml : seek_unsafe")

let seek : t -> int -> Expr.expr = fun (t, m) n -> (if n <= t && n >= 0 then BatMap.find n m else invalid_arg "symStack.ml : seek")
