module F = Format

let pp_gd _fmt _globs = failwith "todo"

let pp_positive fmt n =
  let rec pp_positive_aux fmt = function
    | BinNums.Coq_xH -> F.fprintf fmt "xH"
    | Coq_xI Coq_xH -> F.fprintf fmt "xI xH"
    | Coq_xO Coq_xH -> F.fprintf fmt "xO xH"
    | Coq_xI n -> F.fprintf fmt "xI (%a)" pp_positive_aux n
    | Coq_xO n -> F.fprintf fmt "xO (%a)" pp_positive_aux n in
  F.fprintf fmt "@[<2>%a@]" pp_positive_aux n

let pp_funname (tbl : Conv.coq_tbl) fmt fn =
  let fn' = Conv.fun_of_cfun tbl fn in
  Format.fprintf fmt "@[<2>(* %s *)@ %a@]" fn'.fn_name pp_positive fn

let pp_wsize fmt u =
  F.fprintf fmt (match u with
                 | Wsize.U8 -> "U8" | U16 -> "U16" | U32 -> "U32"
                 | U64 -> "U64" | U128 -> "U128" | U256 -> "U256" )


let pp_stype fmt (ty : Type.stype) =
  match ty with
  | Type.Coq_sbool -> F.fprintf fmt "sbool"
  | Type.Coq_sint -> F.fprintf fmt "sint"
  | Type.Coq_sarr n -> F.fprintf fmt "sarr (%a)" pp_positive n
  | Type.Coq_sword u -> F.fprintf fmt "sword %a" pp_wsize u

let pp_vname fmt (t : Obj.t) =
  let t = Obj.magic t in
  F.fprintf fmt "\"" ;
  ignore (List.map (F.pp_print_char fmt) t) ;
  F.fprintf fmt "\""

(* type var = { vtype : stype; vname : Equality.sort } *)
let pp_var fmt { Var0.Var.vtype : Type.stype; vname : Eqtype.Equality.sort } =
  F.fprintf fmt "@[<v 1>{| @[<2>vtype@ :=@ %a@]@ ; @[<2>vname@ :=@ %a @] |}@]" pp_stype vtype pp_vname vname

(* type var_info = Location.t *)
(* TODO: could print the actual location, shouldn't matter though. *)
let pp_var_info fmt _vi =
  F.fprintf fmt "dummy_var_info"

(* Record var_i := VarI { *)
(*   v_var :> var; *)
(*   v_info : var_info *)
(* }. *)
let pp_var_i fmt { Expr.v_var : Var0.Var.var; v_info : Expr.var_info } =
  F.fprintf fmt
    "@[<v 1>{| @[<2>v_var@ :=@ %a@]@ ; @[<2>v_info@ :=@ %a@] |}@]"
    pp_var v_var pp_var_info v_info

(* TODO could print the actual info *)
let pp_instr_info fmt (_loc, _annot) =
  F.fprintf fmt "InstrInfo.witness"

let pp_lval fmt = function
  | Expr.Lnone (var_info, _stype) ->
     F.fprintf fmt "Lnone _ _"
  | Lvar (var_i) ->
     F.fprintf fmt "Lvar _"
  | Lmem (wsize, var_i, pexpr) ->
     F.fprintf fmt "Lmem _"
  | Laset (arr_access, wsize, var_i, pexpr) ->
     F.fprintf fmt "Laset _ _ _ _"
  | Lasub (arr_access, wsize, positive, var_i, pexpr) ->
     F.fprintf fmt "Lasub _ _ _ _ _"

let pp_asm_op_t fmt asm_op_t =
  F.fprintf fmt "_"

let pp_wsize_opt fmt = function
  | None -> F.fprintf fmt "None"
  | Some wsize -> F.fprintf fmt "(@[<2>Some@ %a@])" pp_wsize wsize

let pp_velem fmt = function
  | Wsize.VE8 -> F.fprintf fmt "VE8"
  | Wsize.VE16 -> F.fprintf fmt "VE16"
  | Wsize.VE32 -> F.fprintf fmt "VE32"
  | Wsize.VE64 -> F.fprintf fmt "VE64"

let pp_sopn (asmOp : 'asm Sopn.asmOp) fmt =
  function
  | Sopn.Ocopy (wsize, positive) ->
     F.fprintf fmt "@[<2>Ocopy@ (%a)@ (%a)@]"
       pp_wsize wsize pp_positive positive
  | Onop -> F.fprintf fmt "Onop"
  | Omulu wsize -> F.fprintf fmt "@[<2>Omulu@ (%a)@]"
                     pp_wsize wsize
  | Oaddcarry wsize -> F.fprintf fmt "@[<2>Oaddcarry@ (%a)@]"
                         pp_wsize wsize
  | Osubcarry wsize -> F.fprintf fmt "@[<2>Osubcarry@ (%a)@]"
                         pp_wsize wsize
  | (Oasm asm_op_t) as opn ->
     F.fprintf fmt "@[<2>Oasm@ (* %a *)@ ("
       (Printer.pp_opn asmOp) opn ;
     begin match asm_op_t with
     | Arch_extra.BaseOp (wsize_opt, a) ->
        F.fprintf fmt "@[<2>BaseOp@ (%a,@ " pp_wsize_opt wsize_opt ;
        let (a : X86_instr_decl.x86_op) = Obj.magic a in
        (match a with
         | X86_instr_decl.MOV wsize -> F.fprintf fmt "(MOV@ %a)" pp_wsize wsize
         | MOVSX (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | MOVZX (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | CMOVcc wsize -> F.fprintf fmt "(@[<2>CMOVcc@ %a@])" pp_wsize wsize
         | ADD wsize -> F.fprintf fmt "(@[<2>ADD@ %a@])" pp_wsize wsize
         | SUB wsize -> F.fprintf fmt "(@[<2>SUB@ %a@])" pp_wsize wsize
         | MUL wsize -> F.fprintf fmt "(@[<2>MUL@ %a@])" pp_wsize wsize
         | IMUL wsize -> F.fprintf fmt "(@[<2>IMUL@ %a@])" pp_wsize wsize
         | IMULr wsize -> F.fprintf fmt "(@[<2>IMULr@ %a@])" pp_wsize wsize
         | IMULri wsize -> F.fprintf fmt "(@[<2>IMULri@ %a@])" pp_wsize wsize
         | DIV wsize -> F.fprintf fmt "(@[<2>DIV@ %a@])" pp_wsize wsize
         | IDIV wsize -> F.fprintf fmt "(@[<2>IDIV@ %a@])" pp_wsize wsize
         | CQO wsize -> F.fprintf fmt "(@[<2>CQO@ %a@])" pp_wsize wsize
         | ADC wsize -> F.fprintf fmt "(@[<2>ADC@ %a@])" pp_wsize wsize
         | SBB wsize -> F.fprintf fmt "(@[<2>SBB@ %a@])" pp_wsize wsize
         | NEG wsize -> F.fprintf fmt "(@[<2>NEG@ %a@])" pp_wsize wsize
         | INC wsize -> F.fprintf fmt "(@[<2>INC@ %a@])" pp_wsize wsize
         | DEC wsize -> F.fprintf fmt "(@[<2>DEC@ %a@])" pp_wsize wsize
         | LZCNT wsize -> F.fprintf fmt "(@[<2>LZCNT@ %a@])" pp_wsize wsize
         | SETcc -> F.fprintf fmt "SETcc"
         | BT wsize -> F.fprintf fmt "(@[<2>BT@ %a@])" pp_wsize wsize
         | CLC -> F.fprintf fmt "CLC"
         | STC -> F.fprintf fmt "STC"
         | LEA wsize -> F.fprintf fmt "(@[<2>LEA@ %a@])" pp_wsize wsize
         | TEST wsize -> F.fprintf fmt "(@[<2>TEST@ %a@])" pp_wsize wsize
         | CMP wsize -> F.fprintf fmt "(@[<2>CMP@ %a@])" pp_wsize wsize
         | AND wsize -> F.fprintf fmt "(@[<2>AND@ %a@])" pp_wsize wsize
         | ANDN wsize -> F.fprintf fmt "(@[<2>ANDN@ %a@])" pp_wsize wsize
         | OR wsize -> F.fprintf fmt "(@[<2>OR@ %a@])" pp_wsize wsize
         | XOR wsize -> F.fprintf fmt "(@[<2>XOR@ %a@])" pp_wsize wsize
         | NOT wsize -> F.fprintf fmt "(@[<2>NOT@ %a@])" pp_wsize wsize
         | ROR wsize -> F.fprintf fmt "(@[<2>ROR@ %a@])" pp_wsize wsize
         | ROL wsize -> F.fprintf fmt "(@[<2>ROL@ %a@])" pp_wsize wsize
         | RCR wsize -> F.fprintf fmt "(@[<2>RCR@ %a@])" pp_wsize wsize
         | RCL wsize -> F.fprintf fmt "(@[<2>RCL@ %a@])" pp_wsize wsize
         | SHL wsize -> F.fprintf fmt "(@[<2>SHL@ %a@])" pp_wsize wsize
         | SHR wsize -> F.fprintf fmt "(@[<2>SHR@ %a@])" pp_wsize wsize
         | SAL wsize -> F.fprintf fmt "(@[<2>SAL@ %a@])" pp_wsize wsize
         | SAR wsize -> F.fprintf fmt "(@[<2>SAR@ %a@])" pp_wsize wsize
         | SHLD wsize -> F.fprintf fmt "(@[<2>SHLD@ %a@])" pp_wsize wsize
         | SHRD wsize -> F.fprintf fmt "(@[<2>SHRD@ %a@])" pp_wsize wsize
         | MULX wsize -> F.fprintf fmt "(@[<2>MULX@ %a@])" pp_wsize wsize
         | ADCX wsize -> F.fprintf fmt "(@[<2>ADCX@ %a@])" pp_wsize wsize
         | ADOX wsize -> F.fprintf fmt "(@[<2>ADOX@ %a@])" pp_wsize wsize
         | BSWAP wsize -> F.fprintf fmt "(@[<2>BSWAP@ %a@])" pp_wsize wsize
         | POPCNT wsize -> F.fprintf fmt "(@[<2>POPCNT@ %a@])" pp_wsize wsize
         | PEXT wsize -> F.fprintf fmt "(@[<2>PEXT@ %a@])" pp_wsize wsize
         | MOVX wsize -> F.fprintf fmt "(@[<2>MOVX@ %a@])" pp_wsize wsize
         | MOVD wsize -> F.fprintf fmt "(@[<2>MOVD@ %a@])" pp_wsize wsize
         | VMOV wsize -> F.fprintf fmt "(@[<2>VMOV@ %a@])" pp_wsize wsize
         | VMOVDQU wsize -> F.fprintf fmt "(@[<2>VMOVDQU@ %a@])" pp_wsize wsize
         | VPMOVSX (_, _, _, _) -> ()
         | VPMOVZX (_, _, _, _) -> ()
         | VPAND wsize -> F.fprintf fmt "(@[<2>VPAND@ %a@])" pp_wsize wsize
         | VPANDN wsize -> F.fprintf fmt "(@[<2>VPANDN@ %a@])" pp_wsize wsize
         | VPOR wsize -> F.fprintf fmt "(@[<2>VPOR@ %a@])" pp_wsize wsize
         | VPXOR wsize -> F.fprintf fmt "(@[<2>VPXOR@ %a@])" pp_wsize wsize
         | VPADD (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPSUB (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPMULL (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPMULH (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPMULHU (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPMULHRS (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPMULU wsize -> F.fprintf fmt "(@[<2>VPMULU@ %a@])" pp_wsize wsize
         | VPEXTR wsize -> F.fprintf fmt "(@[<2>VPEXTR@ %a@])" pp_wsize wsize
         | VPINSR velem -> F.fprintf fmt "(@[<2>VPINSR@ %a@])" pp_velem velem
         | VPSLL (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPSRL (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPSRA (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPSLLV (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPSRLV (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPSLLDQ wsize -> F.fprintf fmt "(@[<2>VPSLLDQ@ %a@])" pp_wsize wsize
         | VPSRLDQ wsize -> F.fprintf fmt "(@[<2>VPSRLDQ@ %a@])" pp_wsize wsize
         | VPSHUFB wsize -> F.fprintf fmt "(@[<2>VPSHUFB@ %a@])" pp_wsize wsize
         | VPSHUFD wsize -> F.fprintf fmt "(@[<2>VPSHUFD@ %a@])" pp_wsize wsize
         | VPSHUFHW wsize -> F.fprintf fmt "(@[<2>VPSHUFHW@ %a@])" pp_wsize wsize
         | VPSHUFLW wsize -> F.fprintf fmt "(@[<2>VPSHUFLW@ %a@])" pp_wsize wsize
         | VPBLEND (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPBLENDVB wsize -> F.fprintf fmt "(@[<2>VPBLENDVB@ %a@])" pp_wsize wsize
         | VPACKUS (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPACKSS (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VSHUFPS wsize -> F.fprintf fmt "(@[<2>VSHUFPS@ %a@])" pp_wsize wsize
         | VPBROADCAST (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VMOVSHDUP (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VMOVSLDUP (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPALIGNR wsize -> F.fprintf fmt "(@[<2>VPALIGNR@ %a@])" pp_wsize wsize
         | VBROADCASTI128 -> F.fprintf fmt "VBROADCASTI128"
         | VPUNPCKH (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPUNPCKL (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VEXTRACTI128 -> F.fprintf fmt "VEXTRACTI128"
         | VINSERTI128 -> F.fprintf fmt "VINSERTI128"
         | VPERM2I128 -> F.fprintf fmt "VPERM2I128"
         | VPERMD -> F.fprintf fmt "VPERMD"
         | VPERMQ -> F.fprintf fmt "VPERMQ"
         | VPMOVMSKB (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPCMPEQ (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPCMPGT (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPMADDUBSW wsize -> F.fprintf fmt "(@[<2>VPMADDUBSW@ %a@])" pp_wsize wsize
         | VPMADDWD wsize -> F.fprintf fmt "(@[<2>VPMADDWD@ %a@])" pp_wsize wsize
         | VMOVLPD -> F.fprintf fmt "VMOVLPD"
         | VMOVHPD -> F.fprintf fmt "VMOVHPD"
         | VPMINU (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPMINS (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPMAXU (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPMAXS (_, _) -> F.fprintf fmt "_ (* TODO *)"
         | VPTEST wsize -> F.fprintf fmt "(@[<2>VPTEST@ %a@])" pp_wsize wsize
         | RDTSC wsize -> F.fprintf fmt "(@[<2>RDTSC@ %a@])" pp_wsize wsize
         | RDTSCP wsize -> F.fprintf fmt "(@[<2>RDTSCP@ %a@])" pp_wsize wsize
         | AESDEC -> F.fprintf fmt "AESDEC"
         | VAESDEC -> F.fprintf fmt "VAESDEC"
         | AESDECLAST -> F.fprintf fmt "AESDECLAST"
         | VAESDECLAST -> F.fprintf fmt "VAESDECLAST"
         | AESENC -> F.fprintf fmt "AESENC"
         | VAESENC -> F.fprintf fmt "VAESENC"
         | AESENCLAST -> F.fprintf fmt "AESENCLAST"
         | VAESENCLAST -> F.fprintf fmt "VAESENCLAST"
         | AESIMC -> F.fprintf fmt "AESIMC"
         | VAESIMC -> F.fprintf fmt "VAESIMC"
         | AESKEYGENASSIST -> F.fprintf fmt "AESKEYGENASSIST"
         | VAESKEYGENASSIST -> F.fprintf fmt "VAESKEYGENASSIST"
        ) ;
        F.fprintf fmt ")@]"
     | ExtOp y ->
        F.fprintf fmt "@[<2>ExtOp@ " ;
        let (y : X86_extra.x86_extra_op) = Obj.magic y in
        (match y with
         | X86_extra.Oset0 wsize -> F.fprintf fmt "(Oset0 %a)" pp_wsize wsize
         | X86_extra.Oconcat128 -> F.fprintf fmt "Oconcat128"
         | X86_extra.Ox86MOVZX32 -> F.fprintf fmt "Ox86MOVZX32"
        ) ;
        F.fprintf fmt "@]"
     end ;
     F.fprintf fmt ")@]"

     (* F.fprintf fmt "@[<2>Oasm@ (\* %a *\)@ (%a)@]" *)
     (*   (Printer.pp_opn asmOp) opn *)
     (*   pp_asm_op_t asm_op_t *)
     (* Arch_extra.sopn_prim_string_base ; *)
     (* Sopn.sopn_prim_constructor asmOp *)

let pp_instr_r (asmOp : 'asm Sopn.asmOp) fmt = function
  | Expr.Cassgn (lval, assgn_tag, stype, pexpr) ->
     F.fprintf fmt "Cassgn@ (%a)@ _@ (%a)@ _"
       pp_lval lval pp_stype stype
  | Copn (lvals, assgn_tag, sopn, pexprs) ->
     F.fprintf fmt "Copn@ _@ _@ (%a)@ _"
       (pp_sopn asmOp) sopn
  | Csyscall (lvals, syscall_t, pexprs) ->
     F.fprintf fmt "_"
  | Cif (pexpr, asm_op_instrs, asm_op_instrs') ->
     F.fprintf fmt "_"
  | Cfor (var_i, range, asm_op_instrs) ->
     F.fprintf fmt "_"
  | Cwhile (align, asm_op_instrs, pexpr, asm_op_instrs') ->
     F.fprintf fmt "_"
  | Ccall (inline_info, lvals, funname, pexprs) ->
     F.fprintf fmt "_"

let pp_instr (asmOp : 'asm Sopn.asmOp) fmt (Expr.MkI (ii, instr_r)) =
  F.fprintf fmt "@[<1>MkI@ %a@ (%a)@]"
    pp_instr_info ii (pp_instr_r asmOp) instr_r


let pp_fdef (asmOp : 'asm Sopn.asmOp)
      fmt ({ Expr.f_info : Expr.fun_info;
             f_tyin : Type.stype list;
             f_params : Expr.var_i list;
             f_body : 'asm_op Expr.instr list;
             f_tyout : Type.stype list;
             f_res : Expr.var_i list;
             f_extra : 'extra_fun_t;
      })
  =
  F.fprintf fmt "@[<v 1>{| @[<2>f_info@ :=@ %a@]@ ; @[<2>f_tyin :=@ @[<2>[%a]@]@]@ ; @[<2>f_params :=@ @[<2>[%a]@]@]@ ; @[<2>f_body@ :=@ @[<2>[ %a ]@]@]@ ; @[<2>f_tyout@ :=@ @[<2>[%a]@]@]@ ; @[<2>f_res :=@ @[<2>[%a]@]@]@ ; @[<2>f_extra :=@ @[<2>%a@]@]@ ; |}@]"
    pp_positive f_info
    (Utils.pp_list ";@ " pp_stype) f_tyin
    (Utils.pp_list ";@ " pp_var_i) f_params
    (Utils.pp_list ";@ " (pp_instr asmOp)) f_body
    (Utils.pp_list ";@ " pp_stype) f_tyout
    (Utils.pp_list ";@ " pp_var_i) f_res
    (fun fmt _extra -> F.fprintf fmt "tt") f_extra


let pp_preamble fmt () =
  let line s = F.fprintf fmt "@[<2>" ; F.fprintf fmt s ; F.fprintf fmt "@]@." ; in
  (* mathcomp *)
  F.fprintf fmt "@[" ;
  line "Set Warnings \"-notation-overridden,-ambiguous-paths\"." ;
  line "From@ mathcomp@ Require@ Import@ all_ssreflect@ all_algebra@ reals@ distr@ realsum@ fingroup.fingroup@ solvable.cyclic@ prime@ ssrnat@ ssreflect@ ssrfun@ ssrbool@ ssrnum@ eqtype@ choice@ seq." ;
  line "Set Warnings \"notation-overridden,ambiguous-paths\".@." ;
  (* jasmin *)
  line "Require Import List." ;
  line "From Jasmin Require Import expr." ;
  line "From Jasmin Require Import x86_instr_decl x86_extra." ;
  line "From JasminSSProve Require Import jasmin_translate." ;
  line "From Crypt Require Import Prelude Package.@." ;
  (* notations *)
  line "Import ListNotations." ;
  line "Local Open Scope string.@." ;
  (* compiler parameters *)
  (* TODO: which of these are needed? *)
  (* line "Context `{asmop : asmOp}." ; *)
  (* line "Context {T} {pT : progT T}." ; *)
  (* line "Context {pd : PointerData}." ; *)
  (* line "Context (P : uprog)." ; *)
  (* line "Context (f : funname)." ; *)
  (*  *)
  (* done *)
  F.fprintf fmt "@.@.@]"


let pp_cuprog tbl (asmOp : 'asm Sopn.asmOp) (fmt : F.formatter) (p : 'asm Expr._uprog) : unit =
  (* pp_list "@ @ " pp_gd fmt (List.rev p.p_globs) ; *)
  ignore pp_gd ;
  pp_preamble fmt ();
  (* ignore pp_preamble ; *)
  print_newline () ;

  (* Record _prog (extra_fun_t: Type) (extra_prog_t: Type):= { *)
  (*   p_funcs : seq (_fun_decl extra_fun_t); *)
  (*   p_globs : glob_decls; *)
  (*   p_extra : extra_prog_t; *)
  (* }. *)
  (* Definition fun_decl := (funname * fundef)%type. *)

  let pp_fun fmt (fn, fdef) =
    F.fprintf fmt "@[<2>( %a,@ %a )@]" (pp_funname tbl) fn (pp_fdef asmOp) fdef
  in
  F.fprintf fmt "@[<v 2>Definition ssprove_jasmin_prog : uprog.@.Proof.@.  refine {| p_funcs :=@  [ %a ]@ |}.@]@ "
    (Utils.pp_list "@  ; " pp_fun) (List.rev p.p_funcs)

  (* List.map (gd_of_cgd tbl) p.C.p_globs, *)
  (* List.map (fdef_of_cufdef tbl) p.C.p_funcs *)
