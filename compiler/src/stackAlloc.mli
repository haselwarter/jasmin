val memory_analysis :
  (unit Conv.coq_tbl -> Format.formatter -> Compiler_util.pp_error -> unit) ->
  debug:bool -> unit Conv.coq_tbl ->
  ('asm -> bool) ->
  X86_extra.x86_extended_op Expr._uprog -> Compiler.stack_alloc_oracles
