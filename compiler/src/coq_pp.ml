module F = Format

let pp_gd fmt _globs = F.fprintf fmt "(* TODO: pp_gd *) _"

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

let pp_velem fmt = function
  | Wsize.VE8 -> F.fprintf fmt "VE8"
  | Wsize.VE16 -> F.fprintf fmt "VE16"
  | Wsize.VE32 -> F.fprintf fmt "VE32"
  | Wsize.VE64 -> F.fprintf fmt "VE64"

let pp_stype fmt (ty : Type.stype) =
  match ty with
  | Type.Coq_sbool -> F.fprintf fmt "sbool"
  | Type.Coq_sint -> F.fprintf fmt "sint"
  | Type.Coq_sarr n -> F.fprintf fmt "(sarr (%a))" pp_positive n
  | Type.Coq_sword u -> F.fprintf fmt "(sword %a)" pp_wsize u

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

let pp_int fmt = function
  | BinNums.Zneg n -> F.fprintf fmt "Zneg (%a)" pp_positive n
  | BinNums.Z0 -> F.fprintf fmt "Z0"
  | BinNums.Zpos n -> F.fprintf fmt "Zpos (%a)" pp_positive n

let pp_v_scope fmt v_scope =
  F.fprintf fmt "%s"
    (match v_scope with Expr.Slocal -> "Slocal" | Sglob -> "Sglob")

let pp_gvar fmt ({ gv; gs } : Expr.gvar) =
  F.fprintf fmt "@[<2>{| gv := %a ; gs := %a |}@]"
    pp_var_i gv pp_v_scope gs

let pp_arr_access fmt (aa : Warray_.arr_access) =
  F.fprintf fmt "%s"
    (match aa with Warray_.AAdirect -> "AAdirect" | AAscale -> "AAscale")

let pp_op_kind fmt = function
  | Expr.Op_int -> F.fprintf fmt "Op_int"
  | Op_w wsize -> F.fprintf fmt "(@[<2>Op_w@ %a@])" pp_wsize wsize

let pp_sop1 fmt = function
  | Expr.Oword_of_int wsize ->
     F.fprintf fmt "(@[<2>Oword_of_int@ %a@])"
       pp_wsize wsize
  | Oint_of_word wsize ->
     F.fprintf fmt "(@[<2>Oint_of_word@ %a@])"
       pp_wsize wsize
  | Osignext (wsize, wsize') ->
     F.fprintf fmt "(@[<2>Osignext@ %a@ %a@])"
       pp_wsize wsize pp_wsize wsize'
  | Ozeroext (wsize, wsize') ->
     F.fprintf fmt "(@[<2>Ozeroext@ %a@ %a@])"
       pp_wsize wsize pp_wsize wsize'
  | Onot -> F.fprintf fmt "Onot"
  | Olnot wsize ->
     F.fprintf fmt "(@[<2>Olnot@ %a@])"
       pp_wsize wsize
  | Oneg op_kind ->
     F.fprintf fmt "(@[<2>Oneg@ %a@])" pp_op_kind op_kind

let pp_signedness fmt signedness =
  F.fprintf fmt "%s"
    (match signedness with Wsize.Signed -> "Signed" | Unsigned -> "Unsigned")

let pp_cmp_kind fmt = function
    Expr.Cmp_int -> F.fprintf fmt "Cmp_int"
  | Cmp_w (signedness, wsize) ->
     F.fprintf fmt "@[<2>Cmp_w@ %a@ %a@]"
       pp_signedness signedness pp_wsize wsize

let pp_sop2 fmt sop2 =
  let f x = F.fprintf fmt x in
  f "(" ;
  (match sop2 with
   | Expr.Obeq -> f "Obeq"
   | Oand -> f "Oand"
   | Oor -> f "Oor"

   | Oadd op_kind -> f "@[<2>Oadd@ %a@]" pp_op_kind op_kind
   | Omul op_kind -> f "@[<2>Omul@ %a@]" pp_op_kind op_kind
   | Osub op_kind -> f "@[<2>Osub@ %a@]" pp_op_kind op_kind
   | Odiv cmp_kind -> f "@[<2>Odiv@ %a@]" pp_cmp_kind cmp_kind
   | Omod cmp_kind -> f "@[<2>Omod@ %a@]" pp_cmp_kind cmp_kind

   | Oland wsize -> f "@[<2>Oland@ %a@]" pp_wsize wsize
   | Olor  wsize -> f "@[<2>Olor@ %a@]" pp_wsize wsize
   | Olxor wsize -> f "@[<2>Olxor@ %a@]" pp_wsize wsize
   | Olsr  wsize -> f "@[<2>Olsr@ %a@]" pp_wsize wsize
   | Olsl  op_kind -> f "@[<2>Olsl@ %a@]" pp_op_kind op_kind
   | Oasr  op_kind -> f "@[<2>Oasr@ %a@]" pp_op_kind op_kind

   | Oeq op_kind -> f "@[<2>Oeq@ %a@]" pp_op_kind op_kind
   | Oneq op_kind -> f "@[<2>Oneq@ %a@]" pp_op_kind op_kind
   | Olt cmp_kind -> f "@[<2>Olt@ %a@]" pp_cmp_kind cmp_kind
   | Ole cmp_kind -> f "@[<2>Ole@ %a@]" pp_cmp_kind cmp_kind
   | Ogt cmp_kind -> f "@[<2>Ogt@ %a@]" pp_cmp_kind cmp_kind
   | Oge cmp_kind -> f "@[<2>Oge@ %a@]" pp_cmp_kind cmp_kind

   | Ovadd (velem, wsize) ->
      f "@[<2>Ovadd@ %a %a@]" pp_velem velem pp_wsize wsize
   | Ovsub (velem, wsize) ->
      f "@[<2>Ovsub@ %a %a@]" pp_velem velem pp_wsize wsize
   | Ovmul (velem, wsize) ->
      f "@[<2>Ovmul@ %a %a@]" pp_velem velem pp_wsize wsize
   | Ovlsr (velem, wsize) ->
      f "@[<2>Ovlsr@ %a %a@]" pp_velem velem pp_wsize wsize
   | Ovlsl (velem, wsize) ->
      f "@[<2>Ovlsl@ %a %a@]" pp_velem velem pp_wsize wsize
   | Ovasr (velem, wsize) ->
      f "@[<2>Ovasr@ %a %a@]" pp_velem velem pp_wsize wsize)
  ; f ")"

let pp_pelem fmt pelem =
  F.fprintf fmt "%s"
    (match pelem with
     | Wsize.PE1 -> "PE1" | PE2 -> "PE2" | PE4 -> "PE4"
     | PE8 -> "PE8" | PE16 -> "PE16" | PE32 -> "PE32"
     | PE64 -> "PE64" | PE128 -> "PE128")

let pp_combine_flags fmt = function
  | Expr.CF_LT signedness ->
     F.fprintf fmt "@[<2>CF_LT@ %a@]" pp_signedness signedness
  | CF_LE signedness ->
     F.fprintf fmt "@[<2>CF_LE@ %a@]" pp_signedness signedness
  | CF_EQ -> F.fprintf fmt "CF_EQ"
  | CF_NEQ -> F.fprintf fmt "CF_NEQ"
  | CF_GE signedness ->
     F.fprintf fmt "@[<2>CF_GE@ %a@]" pp_signedness signedness
  | CF_GT signedness ->
     F.fprintf fmt "@[<2>CF_GT@ %a@]" pp_signedness signedness

let pp_opn fmt = function
  | Expr.Opack (wsize, pelem) ->
     F.fprintf fmt "@[<2>Opack@ %a@ %a@]"
       pp_wsize wsize pp_pelem pelem
  | Ocombine_flags combine_flags ->
     F.fprintf fmt "@[<2>Ocombine_flags@ (%a)@]"
       pp_combine_flags combine_flags

let rec pp_pexpr fmt (pexpr : Expr.pexpr) =
  let f x = F.fprintf fmt x in
  (match pexpr with
   | Expr.Pconst i -> f "(@[<2>Pconst@ (%a)@])" pp_int i
   | Expr.Pbool b -> f "(@[<2>Pbool@ %b@])" b
   | Expr.Parr_init n -> f "(@[<2>Parr_init@ %a@])" pp_positive n
   | Expr.Pvar v -> f "(@[<2>Pvar@ %a@])" pp_gvar v
   | Expr.Pget (arr_access, wsize, gvar, pexpr) ->
      f "(@[<2>Pget@ %a@ %a@ %a@ %a@])"
        pp_arr_access arr_access pp_wsize wsize
        pp_gvar gvar pp_pexpr pexpr
   | Expr.Psub (arr_access, wsize, positive, gvar, pexpr) ->
      f "(@[<2>Psub@ %a@ %a@ (%a)@ %a@ %a@])"
        pp_arr_access arr_access pp_wsize wsize
        pp_positive positive pp_gvar gvar pp_pexpr pexpr
   | Expr.Pload (wsize, var_i, pexpr) ->
      f "(@[<2>Pload@ %a@ %a@ %a@])"
        pp_wsize wsize pp_var_i var_i pp_pexpr pexpr
   | Expr.Papp1 (sop1, pexpr) ->
      f "(@[<2>Papp1@ %a@ %a@])" pp_sop1 sop1 pp_pexpr pexpr
   | Expr.Papp2 (sop2, pexpr, pexpr') ->
      f "(@[<2>Papp2@ %a@ %a@ %a@])" pp_sop2 sop2
        pp_pexpr pexpr pp_pexpr pexpr'
   | Expr.PappN (opn, pexprs) ->
      f "(@[<2>PappN@ (%a)@ %a@])" pp_opn opn pp_pexprs pexprs
   | Expr.Pif (stype, pexpr1, pexpr2, pexpr3) ->
      f "(@[<2>Pif@ (%a)@ %a@ %a@ %a@])"
        pp_stype stype pp_pexpr pexpr1
        pp_pexpr pexpr2 pp_pexpr pexpr3)

and pp_pexprs fmt =
  F.fprintf fmt "@[<2>[%a]@]" (Utils.pp_list ";@ " pp_pexpr)

let pp_lval fmt = function
  | Expr.Lnone (var_info, stype) ->
     F.fprintf fmt "@[<2>Lnone@ %a@ %a@]"
       pp_var_info var_info pp_stype stype
  | Lvar (var_i) ->
     F.fprintf fmt "@[<2>Lvar@ %a@]"
       pp_var_i var_i
  | Lmem (wsize, var_i, pexpr) ->
     F.fprintf fmt "@[<2>Lmem@ %a@ %a@ %a@]"
       pp_wsize wsize pp_var_i var_i pp_pexpr pexpr
  | Laset (arr_access, wsize, var_i, pexpr) ->
     F.fprintf fmt "@[<2>Laset@ %a@ %a@ %a@ %a@]"
       pp_arr_access arr_access pp_wsize wsize
       pp_var_i var_i pp_pexpr pexpr
 | Lasub (arr_access, wsize, positive, var_i, pexpr) ->
     F.fprintf fmt "@[<2>Lasub@ %a@ %a@ (%a)@ %a@ %a@]"
       pp_arr_access arr_access pp_wsize wsize
       pp_positive positive
       pp_var_i var_i pp_pexpr pexpr

(* let pp_asm_op_t fmt asm_op_t = *)
(*   F.fprintf fmt "_" *)

let pp_wsize_opt fmt = function
  | None -> F.fprintf fmt "None"
  | Some wsize -> F.fprintf fmt "(@[<2>Some@ %a@])" pp_wsize wsize


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
         | MOVSX (ws, ws') -> F.fprintf fmt "(@[<2>MOVSX@ %a@ %a@])" pp_wsize ws pp_wsize ws'
         | MOVZX (ws, ws') -> F.fprintf fmt "(@[<2>MOVZX@ %a@ %a@])" pp_wsize ws pp_wsize ws'
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
         | VPMOVSX (ve, ws, ve', ws') ->
            F.fprintf fmt "(@[<2>VPMOVSX@ %a@ %a@ %a@ %a@])"
              pp_velem ve pp_wsize ws pp_velem ve' pp_wsize ws'
         | VPMOVZX (ve, ws, ve', ws') ->
            F.fprintf fmt "(@[<2>VPMOVZX@ %a@ %a@ %a@ %a@])"
              pp_velem ve pp_wsize ws pp_velem ve' pp_wsize ws'
         | VPAND wsize -> F.fprintf fmt "(@[<2>VPAND@ %a@])" pp_wsize wsize
         | VPANDN wsize -> F.fprintf fmt "(@[<2>VPANDN@ %a@])" pp_wsize wsize
         | VPOR wsize -> F.fprintf fmt "(@[<2>VPOR@ %a@])" pp_wsize wsize
         | VPXOR wsize -> F.fprintf fmt "(@[<2>VPXOR@ %a@])" pp_wsize wsize
         | VPADD (ve, ws) -> F.fprintf fmt "(@[<2>VPADD@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VPSUB (ve, ws) -> F.fprintf fmt "(@[<2>VPSUB@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VPMULL (ve, ws) -> F.fprintf fmt "(@[<2>VPMULL@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VPMULH (ve, ws) -> F.fprintf fmt "(@[<2>VPMULH@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VPMULHU (ve, ws) -> F.fprintf fmt "(@[<2>VPMULHU@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VPMULHRS (ve, ws) -> F.fprintf fmt "(@[<2>VPMULHRS@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VPMULU wsize -> F.fprintf fmt "(@[<2>VPMULU@ %a@])" pp_wsize wsize
         | VPEXTR wsize -> F.fprintf fmt "(@[<2>VPEXTR@ %a@])" pp_wsize wsize
         | VPINSR velem -> F.fprintf fmt "(@[<2>VPINSR@ %a@])" pp_velem velem
         | VPSLL (ve, ws) -> F.fprintf fmt "(@[<2>VPSLL@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VPSRL (ve, ws) -> F.fprintf fmt "(@[<2>VPSRL@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VPSRA (ve, ws) -> F.fprintf fmt "(@[<2>VPSRA@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VPSLLV (ve, ws) -> F.fprintf fmt "(@[<2>VPSLLV@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VPSRLV (ve, ws) -> F.fprintf fmt "(@[<2>VPSRLV@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VPSLLDQ wsize -> F.fprintf fmt "(@[<2>VPSLLDQ@ %a@])" pp_wsize wsize
         | VPSRLDQ wsize -> F.fprintf fmt "(@[<2>VPSRLDQ@ %a@])" pp_wsize wsize
         | VPSHUFB wsize -> F.fprintf fmt "(@[<2>VPSHUFB@ %a@])" pp_wsize wsize
         | VPSHUFD wsize -> F.fprintf fmt "(@[<2>VPSHUFD@ %a@])" pp_wsize wsize
         | VPSHUFHW wsize -> F.fprintf fmt "(@[<2>VPSHUFHW@ %a@])" pp_wsize wsize
         | VPSHUFLW wsize -> F.fprintf fmt "(@[<2>VPSHUFLW@ %a@])" pp_wsize wsize
         | VPBLEND (ve, ws) -> F.fprintf fmt "(@[<2>VPBLEND@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VPBLENDVB wsize -> F.fprintf fmt "(@[<2>VPBLENDVB@ %a@])" pp_wsize wsize
         | VPACKUS (ve, ws) -> F.fprintf fmt "(@[<2>VPACKUS@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VPACKSS (ve, ws) -> F.fprintf fmt "(@[<2>VPACKSS@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VSHUFPS wsize -> F.fprintf fmt "(@[<2>VSHUFPS@ %a@])" pp_wsize wsize
         | VPBROADCAST (ve, ws) -> F.fprintf fmt "(@[<2>VPBROADCAST@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VMOVSHDUP (ve, ws) -> F.fprintf fmt "(@[<2>VMOVSHDUP@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VMOVSLDUP (ve, ws) -> F.fprintf fmt "(@[<2>VMOVSLDUP@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VPALIGNR wsize -> F.fprintf fmt "(@[<2>VPALIGNR@ %a@])" pp_wsize wsize
         | VBROADCASTI128 -> F.fprintf fmt "VBROADCASTI128"
         | VPUNPCKH (ve, ws) -> F.fprintf fmt "(@[<2>VPUNPCKH@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VPUNPCKL (ve, ws) -> F.fprintf fmt "(@[<2>VPUNPCKL@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VEXTRACTI128 -> F.fprintf fmt "VEXTRACTI128"
         | VINSERTI128 -> F.fprintf fmt "VINSERTI128"
         | VPERM2I128 -> F.fprintf fmt "VPERM2I128"
         | VPERMD -> F.fprintf fmt "VPERMD"
         | VPERMQ -> F.fprintf fmt "VPERMQ"
         | VPMOVMSKB (ws, ws') -> F.fprintf fmt "(@[<2>VPMOVMSKB@ %a@ %a@])" pp_wsize ws pp_wsize ws'
         | VPCMPEQ (ve, ws) -> F.fprintf fmt "(@[<2>VPCMPEQ@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VPCMPGT (ve, ws) -> F.fprintf fmt "(@[<2>VPCMPGT@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VPMADDUBSW wsize -> F.fprintf fmt "(@[<2>VPMADDUBSW@ %a@])" pp_wsize wsize
         | VPMADDWD wsize -> F.fprintf fmt "(@[<2>VPMADDWD@ %a@])" pp_wsize wsize
         | VMOVLPD -> F.fprintf fmt "VMOVLPD"
         | VMOVHPD -> F.fprintf fmt "VMOVHPD"
         | VPMINU (ve, ws) -> F.fprintf fmt "(@[<2>VPMINU@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VPMINS (ve, ws) -> F.fprintf fmt "(@[<2>VPMINS@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VPMAXU (ve, ws) -> F.fprintf fmt "(@[<2>VPMAXU@ %a@ %a@])" pp_velem ve pp_wsize ws
         | VPMAXS (ve, ws) -> F.fprintf fmt "(@[<2>VPMAXS@ %a@ %a@])" pp_velem ve pp_wsize ws
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

let pp_assgn_tag fmt (assgn_tag : Expr.assgn_tag) =
  F.fprintf fmt "%s"
    (match assgn_tag with
     | Expr.AT_none -> "AT_none"
     | Expr.AT_keep -> "AT_keep"
     | Expr.AT_rename -> "AT_rename"
     | Expr.AT_inline -> "AT_inline"
     | Expr.AT_phinode -> "AT_phinode")

let pp_lvals fmt =
  F.fprintf fmt "@[<2>[%a]@]" (Utils.pp_list ";@ " pp_lval)

let pp_syscall fmt = function
  | Syscall_t.RandomBytes p ->
     F.fprintf fmt "@[<2>RandomBytes@ (%a)@]" pp_positive p

let pp_align fmt (align : Expr.align) =
  F.fprintf fmt "%s"
    (match align with
     | Expr.Align -> "Align"
     | Expr.NoAlign -> "NoAlign")

let pp_range fmt ((dir, x), y) =
  F.fprintf fmt "@[<2>((%s,@ %a),@ %a)@]"
    (match dir with Expr.UpTo -> "UpTo" | DownTo -> "DownTo")
    pp_pexpr x pp_pexpr y

let rec pp_instr_r (asmOp : 'asm Sopn.asmOp) fmt x =
  let pp_instrs fmt =
    F.fprintf fmt "@[<2>[%a]@]"
      (Utils.pp_list ";@ " (pp_instr asmOp)) in
  match x with
  | Expr.Cassgn (lval, assgn_tag, stype, pexpr) ->
     F.fprintf fmt "@[<2>Cassgn@ (%a)@ %a@ (%a)@ (%a)@]"
       pp_lval lval pp_assgn_tag assgn_tag pp_stype stype
       pp_pexpr pexpr
  | Copn (lvals, assgn_tag, sopn, pexprs) ->
     F.fprintf fmt "@[<2>Copn@ %a@ %a@ (%a)@ %a@]"
       pp_lvals lvals
       pp_assgn_tag assgn_tag (pp_sopn asmOp) sopn
       pp_pexprs pexprs
  | Csyscall (lvals, syscall_t, pexprs) ->
     F.fprintf fmt "@[<2>Csyscall@ %a@ %a@ %a@]"
       pp_lvals lvals
       pp_syscall syscall_t
       pp_pexprs pexprs
  | Cif (pexpr, asm_op_instrs, asm_op_instrs') ->
     F.fprintf fmt "@[<2>Cif@ %a@ %a@ %a@]"
       pp_pexpr pexpr
       pp_instrs asm_op_instrs
       pp_instrs asm_op_instrs'
  | Cfor (var_i, range, asm_op_instrs) ->
     F.fprintf fmt "@[<2>Cfor@ (%a)@ %a@ @[<2>[%a]@]@]"
       pp_var_i var_i
       pp_range range
       (Utils.pp_list ";@ " (pp_instr asmOp)) asm_op_instrs
  | Cwhile (align, asm_op_instrs, pexpr, asm_op_instrs') ->
     F.fprintf fmt "@[<2>Cwhile@ %a@ %a@ (%a)@ %a@]"
       pp_align align
       pp_instrs asm_op_instrs
       pp_pexpr pexpr
       pp_instrs asm_op_instrs'
  | Ccall (inline_info, lvals, funname, pexprs) ->
     F.fprintf fmt "@[<2>Ccall@ %s@ %a@ (%a)@ %a@]"
       (match inline_info with
          Expr.InlineFun -> "InlineFun" | DoNotInline -> "DoNotInline")
       pp_lvals lvals
       pp_positive funname
       pp_pexprs pexprs

and pp_instr (asmOp : 'asm Sopn.asmOp) fmt (Expr.MkI (ii, instr_r)) =
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
  line "Set Warnings \"-notation-overridden\"." ;
  line "From Jasmin Require Import expr." ;
  line "Set Warnings \"notation-overridden\"." ;
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

let pp_word fmt _word =
  F.fprintf fmt "(* TODO: pp_gd *) _"

let pp_array fmt _array =
  F.fprintf fmt "(* TODO: pp_gd *) _"

let pp_glob_value fmt = function
  | Global.Gword (wsize, word) ->
     F.fprintf fmt "(@[<2>@Gword@ %a@ %a@])"
       pp_wsize wsize pp_word word
  | Garr (p, array) ->
     F.fprintf fmt "(@[<2>@Garr@ (%a)@ %a@])"
       pp_positive p pp_array array

let pp_glob_decl fmt (var, gv) =
  F.fprintf fmt "@[<2>(%a,@ %a)@]"
    pp_var var pp_glob_value gv

let pp_globs fmt globs =
  F.fprintf fmt "@[<2>[%a]@]"
    (Utils.pp_list "@  ; " pp_glob_decl) globs

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
  F.fprintf fmt "@[<v 2>Definition ssprove_jasmin_prog : uprog.@.Proof.@.  refine {| p_funcs :=@  [ %a ] ;@.  p_globs := %a ;@.  p_extra := tt |}.@.Defined.@]@ "
    (Utils.pp_list "@  ; " pp_fun) (List.rev p.p_funcs)
    pp_globs p.p_globs

  (* List.map (gd_of_cgd tbl) p.C.p_globs, *)
  (* List.map (fdef_of_cufdef tbl) p.C.p_funcs *)
