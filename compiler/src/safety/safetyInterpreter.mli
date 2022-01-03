module type ExportWrap = sig
  type asm
  (* main function, before any compilation pass *)
  val main_source : (unit, asm) Prog.func
      
  val main : (unit, asm) Prog.func
  val prog : (unit, asm) Prog.prog
end

(* Abstract Interpreter. *)
module AbsAnalyzer (PW : ExportWrap) : sig
  val analyze : unit -> unit
end
