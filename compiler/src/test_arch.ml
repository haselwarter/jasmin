open Arch_extra
open Prog

(* TODO: check that we cannot use sth already defined on the Coq side *)

module type Core_arch = sig
  type reg
  type xreg
  type rflag
  type cond
  type asm_op
  type extra_op
  type fresh_vars
  type lowering_options

  val asm_e : (reg, xreg, rflag, cond, asm_op, extra_op) asm_extra
  val aparams : (reg, xreg, rflag, cond, asm_op, extra_op, fresh_vars, lowering_options) Compiler.architecture_params

  (* val rip : reg ?? *)
  val rsp : reg

  val allocatable : reg list
  val xmm_allocatable : xreg list
  val arguments : reg list
  val xmm_arguments : xreg list
  val ret : reg list
  val xmm_ret : xreg list
  val reserved : reg list
  val callee_save : reg list
end

module type Arch = sig
  include Core_arch

  val reg_vars : var list
  val xreg_vars : var list
  val flag_vars : var list
  val argument_vars : var list
  val xmm_argument_vars : var list
  val ret_vars : var list
  val xmm_ret_vars : var list
  val allocatable_vars : var list
  val xmm_allocatable_vars : var list
  val callee_save_vars : var list
  val rsp_var : var
  val all_registers : var list
  val pp_opn : Format.formatter -> (reg, xreg, rflag, cond, asm_op, extra_op) Arch_extra.extended_op Sopn.sopn -> unit
end

module Arch_from_Core_arch (A : Core_arch) : Arch = struct
  include A

  let string_of_reg r =
    Conv.string_of_string0 (A.asm_e._asm._arch_decl.toS_r.to_string r)

  let reg_vars =
    let l = A.asm_e._asm._arch_decl.toS_r.strings in
    let reg_k = Prog.Reg Prog.Direct in
    List.map (fun (s, _) -> V.mk (Conv.string_of_string0 s) reg_k (Bty (U U64)) L._dummy []) l

  let var_of_reg r =
    let s = string_of_reg r in
    List.find (fun x -> x.v_name = s) reg_vars

  let string_of_xreg r =
    Conv.string_of_string0 (A.asm_e._asm._arch_decl.toS_x.to_string r)

  let xreg_vars =
    let l = A.asm_e._asm._arch_decl.toS_x.strings in
    let reg_k = Prog.Reg Prog.Direct in
    List.map (fun (s, _) -> V.mk (Conv.string_of_string0 s) reg_k (Bty (U U256)) L._dummy []) l

  let var_of_xreg r =
    let s = string_of_xreg r in
    List.find (fun x -> x.v_name = s) xreg_vars

  let string_of_flag f =
    Conv.string_of_string0 (A.asm_e._asm._arch_decl.toS_f.to_string f)

  let flag_vars =
    let l = A.asm_e._asm._arch_decl.toS_f.strings in
    let reg_k = Prog.Reg Prog.Direct in
    List.map (fun (s, _) -> V.mk (Conv.string_of_string0 s) reg_k (Bty Bool) L._dummy []) l

  let var_of_flag f =
    let s = string_of_flag f in
    List.find (fun x -> x.v_name = s) flag_vars

  let argument_vars =
    List.map var_of_reg A.arguments

  let xmm_argument_vars =
    List.map var_of_xreg A.xmm_arguments

  let ret_vars =
    List.map var_of_reg A.ret

  let xmm_ret_vars =
    List.map var_of_xreg A.xmm_ret

  let allocatable_vars =
    List.map var_of_reg A.arguments

  let xmm_allocatable_vars =
    List.map var_of_xreg A.xmm_arguments

  let callee_save_vars =
    List.map var_of_reg A.callee_save

  let rsp_var = var_of_reg A.rsp

  let all_registers = reg_vars @ xreg_vars @ flag_vars

  let pp_opn fmt o =
    Format.fprintf fmt "%s" (Conv.string_of_string0 (Sopn.string_of_sopn (Arch_extra.asm_opI A.asm_e) o))
end
