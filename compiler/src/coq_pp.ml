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

let pp_funname (tbl : 'info Conv.coq_tbl) fmt fn =
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
  | Type.Coq_sarr n -> F.fprintf fmt "sarr %a" pp_positive n
  | Type.Coq_sword u -> F.fprintf fmt "sword %a" pp_wsize u

let pp_vname fmt (t : Obj.t) =
  let t = Obj.magic t in
  F.fprintf fmt "\"" ;
  ignore (List.map (F.pp_print_char fmt) t) ;
  F.fprintf fmt "\""

(* type var = { vtype : stype; vname : Equality.sort } *)
let pp_var fmt { Var0.Var.vtype : Type.stype; vname : Eqtype.Equality.sort } =
  F.fprintf fmt "@[<v 1>{| @[<2>vtype@ :=@ %a@]@ ; @[<2>vname@ :=@ %a @] |}@]" pp_stype vtype pp_vname vname

(* Definition var_info := positive. *)
let pp_var_info = pp_positive

(* Record var_i := VarI { *)
(*   v_var :> var; *)
(*   v_info : var_info *)
(* }. *)
let pp_var_i fmt { Expr.v_var : Var0.Var.var; v_info : Expr.var_info } =
  F.fprintf fmt
    "@[<v 1>{| @[<2>v_var@ :=@ %a@]@ ; @[<2>v_info@ :=@ %a@] |}@]"
    pp_var v_var pp_var_info v_info

let pp_instr_info = pp_positive

let pp_lval fmt = function
  | Expr.Lnone (var_info, _stype) ->
     F.fprintf fmt "Lnone _"
  | Lvar (var_i) ->
     F.fprintf fmt "Lvar _"
  | Lmem (wsize, var_i, pexpr) ->
     F.fprintf fmt "Lmem _"
  | Laset (arr_access, wsize, var_i, pexpr) ->
     F.fprintf fmt "Laset _"
  | Lasub (arr_access, wsize, positive, var_i, pexpr) ->
     F.fprintf fmt "Lasub _"

let pp_asm_op_t fmt asm_op_t =
  ()

let pp_sopn fmt = function
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
  | Oasm asm_op_t -> F.fprintf fmt "@[<2>Oasm@ (%a)@]"
                       pp_asm_op_t asm_op_t


let pp_instr_r fmt = function
  | Expr.Cassgn (lval, assgn_tag, stype, pexpr) ->
     F.fprintf fmt "Cassgn@ (%a)@ _@ (%a)@ _"
       pp_lval lval pp_stype stype
  | Copn (lvals, assgn_tag, sopn, pexprs) ->
     F.fprintf fmt "Copn@ _@ _@ (%a)@ _"
       pp_sopn sopn
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

let pp_instr fmt (Expr.MkI (ii, instr_r)) =
  F.fprintf fmt "@[<1>MkI@ (%a)@ (%a)@]"
    pp_instr_info ii pp_instr_r instr_r


let pp_fdef fmt ({ Expr.f_info : Expr.fun_info;
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
    (Utils.pp_list ";@ " pp_instr) f_body
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
  line "From Jasmin Require Import x86_extra." ;
  line "From JasminSSProve Require Import jasmin_translate." ;
  line "From Crypt Require Import Prelude Package.@." ;
  (* notations *)
  line "Import ListNotations." ;
  line "Local Open Scope string.@." ;
  (* compiler parameters *)
  line "Context `{asmop : asmOp}." ;
  line "Context {T} {pT : progT T}." ;
  line "Context {pd : PointerData}." ;
  line "Context (P : uprog)." ;
  line "Context (f : funname)." ;
  (* done *)
  F.fprintf fmt "@.@.@]"


let pp_cuprog tbl pp_asm (fmt : F.formatter) (p : 'asm Expr._uprog) : unit =
  (* pp_list "@ @ " pp_gd fmt (List.rev p.p_globs) ; *)
  ignore pp_gd ;
  ignore pp_asm ;
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
    F.fprintf fmt "@[<2>( %a,@ %a )@]" (pp_funname tbl) fn pp_fdef fdef
  in
  F.fprintf fmt "@[<v 2>Definition ssprove_jasmin_prog : uprog.@.Proof.@ refine {| p_funcs :=@  [ %a ]@ |}.@]@ "
    (Utils.pp_list "@  ; " pp_fun) (List.rev p.p_funcs)

  (* List.map (gd_of_cgd tbl) p.C.p_globs, *)
  (* List.map (fdef_of_cufdef tbl) p.C.p_funcs *)
