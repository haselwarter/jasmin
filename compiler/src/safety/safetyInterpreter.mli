module type ExportWrap = sig
  (* main function, before any compilation pass *)
  val main_source : (unit, X86_extra.x86_extended_op) Prog.func
      
  val main : (unit, X86_extra.x86_extended_op) Prog.func
  val prog : (unit, X86_extra.x86_extended_op) Prog.prog
end

(* Abstract Interpreter. *)
module AbsAnalyzer (PW : ExportWrap) : sig
  val analyze :
    (Format.formatter -> X86_extra.x86_extended_op Sopn.sopn -> unit) ->
    unit -> unit
end
