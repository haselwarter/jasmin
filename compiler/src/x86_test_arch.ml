open X86_decl

module X86 : Test_arch.Core_arch = struct
  type reg = register
  type xreg = xmm_register
  type nonrec rflag = rflag
  type cond = condt
  type asm_op = X86_instr_decl.x86_op
  type extra_op = X86_extra.x86_extra_op

  let asm_e = X86_extra.x86_extra
  let aparams = X86_params.aparams

  (* val rip : reg ?? *)
  let rsp = RSP

  let allocatable = [
      RAX; RCX; RDX;
      RSI; RDI;
      R8; R9; R10; R11;
      RBP;
      RBX;
      R12; R13; R14; R15
    ]

  let xmm_allocatable = [
    XMM0; XMM1; XMM2; XMM3; XMM4; XMM5; XMM6; XMM7;
    XMM8; XMM9; XMM10; XMM11; XMM12; XMM13; XMM14; XMM15
  ]

  let arguments = [
    RDI; RSI; RDX; RCX;
    R8; R9
  ]

  let xmm_arguments = [
    XMM0; XMM1; XMM2; XMM3; XMM4; XMM5; XMM6; XMM7
  ]

  let ret = [
    RAX; RDX
  ]

  let xmm_ret = [
    XMM0; XMM1
  ]

  let reserved = [
    RSP
  ]

  (* rsp does not need to be saved since it is an invariant
     of jasmin program *)
  let callee_save = [
    RBP; RBX; R12; R13; R14; R15
  ]
end
