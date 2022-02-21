val memory_analysis :
  (unit Conv.coq_tbl -> Format.formatter -> Compiler_util.pp_error -> unit) ->
  debug:bool ->
  (Format.formatter -> 'asm Sopn.sopn -> unit) ->
  ('asm -> bool) ->
  (bool -> (Utils0.funname -> bool list option) -> Utils0.funname -> 'asm Expr._ufundef -> 'asm Expr._ufundef Compiler_util.cexec) ->
  (Eqtype.Equality.sort -> Eqtype.Equality.sort -> Ssralg.GRing.ComRing.sort list -> ((Var0.Var.var * Wsize.wsize) * BinNums.coq_Z) list -> (Utils0.funname -> Stack_alloc.stk_alloc_oracle_t) -> 'asm Expr._uprog -> 'asm Expr._sprog Compiler_util.cexec) ->
  ((Var0.Var.var -> Prog.var) -> ((unit, 'asm) Prog.func -> Expr.stk_fun_extra -> bool) -> (unit, 'asm) Prog.sfundef list -> (Expr.stk_fun_extra * Regalloc.reg_oracle_t * (unit, 'asm) Prog.func) list) ->
  unit Conv.coq_tbl ->
  'asm Expr._uprog -> Compiler.stack_alloc_oracles
