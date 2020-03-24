(* ** License
 * -----------------------------------------------------------------------
 * Copyright 2016--2017 IMDEA Software Institute
 * Copyright 2016--2017 Inria
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * ----------------------------------------------------------------------- *)

(* ** Imports and settings *)
From mathcomp Require Import all_ssreflect all_algebra.
From CoqWord Require Import ssrZ.
Require Import psem compiler_util constant_prop_proof.
Require Export psem stack_alloc.
Require Import Psatz.
Import Utf8.

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Local Open Scope vmap.
Local Open Scope seq_scope.
Local Open Scope Z_scope.

Import Region.

(* --------------------------------------------------------------------------- *)

Definition size_of (t:stype) :=
  match t with
  | sword sz => wsize_size sz
  | sarr n   => Zpos n
  | _        => 1
  end.

Section Section.

Variables (pmap:pos_map) (stk_size glob_size:Z).
Variables (rsp rip: pointer).
(* FIXME: we should be more flexible on the alignment of rsp and rip *)
Hypothesis (is_align_rsp: forall ws, is_align rsp ws) (is_align_rip: forall ws, is_align rip ws).

Hypothesis (rsp_bound : wunsigned rsp + stk_size <= wbase Uptr)
           (rip_bound : wunsigned rip + glob_size <= wbase Uptr).

Hypothesis disj_rsp_rip :
  0 < stk_size -> 0 < glob_size ->
  wunsigned rsp + stk_size <= wunsigned rip \/ wunsigned rip + glob_size <= wunsigned rsp.

Hint Resolve is_align_no_overflow Memory.valid_align.

Definition global_pos x := 
  omap (fun z => (z, vtype x)) (Mvar.get pmap.(globals) x).

Definition local_pos x := 
  match get_local pmap x with
  | Some (Pstack z) => Some (z, vtype x)
  | Some (Pstkptr z) => Some (z, sword Uptr)
  | _ => None 
  end.

Record wf_pos (get : var -> option (Z * stype)) size := {
  wfg_ofs   : forall x ofs ty, get x = Some (ofs, ty) -> 
             0 <= ofs /\ ofs + size_of ty <= size;
  wfg_disj  : forall x y xofs yofs xty yty,
             x <> y ->
             get x = Some (xofs, xty) -> get y = Some (yofs, yty) ->
             xofs + size_of xty <= yofs \/ yofs + size_of yty <= xofs;
}.

Class wf_regptr := {
  wt_rip   : vtype pmap.(vrip) = sword Uptr;
  wt_rsp   : vtype pmap.(vrsp) = sword Uptr;
  wt_rptr  : forall x p, get_local pmap x = Some (Pregptr p) -> vtype p = sword Uptr;
  wf_ptr   : forall x, (exists p, get_local pmap x = Some (Pregptr p)) \/
                       (exists z, get_local pmap x = Some (Pstkptr z)) ->
                       is_sarr (vtype x);
  disj_ptr : forall x xp, 
              get_local pmap x = Some (Pregptr xp) -> 
              [/\ xp <> pmap.(vrip), xp <> pmap.(vrsp), Sv.In xp pmap.(vnew) &
                  forall y yp, x <> y -> 
                               get_local pmap y = Some (Pregptr yp) -> xp <> yp];
}.

Class wf_pmap := {
  rip_in_new : Sv.In pmap.(vrip) pmap.(vnew);
  rsp_in_new : Sv.In pmap.(vrsp) pmap.(vnew);
  wt_regptr  :> wf_regptr;
  wf_globals :> wf_pos global_pos glob_size;
  wf_locals  :> wf_pos local_pos stk_size;
}.

Declare Instance wf_pmapI : wf_pmap.

Definition valid_mp mp ty := 
  exists x, 
    subtype ty (vtype x) /\
    if mp.(mp_s) == MSglob then get_global pmap x = ok mp.(mp_ofs)
    else get_local pmap x = Some (Pstack mp.(mp_ofs)).

Definition eq_vm (vm1 vm2:vmap) := 
  forall (x:var), get_local pmap x = None -> get_var vm1 x = get_var vm2 x.

Definition wptr ms := 
  if ms == MSglob then rip else rsp.

Definition wptr_size ms :=
  if ms == MSstack then stk_size else glob_size.

Definition disjoint_ptr m :=
  forall w sz ms,
    valid_pointer m w sz ->
    ~ ( wunsigned (wptr ms) < wunsigned w + wsize_size sz /\ wunsigned w < wunsigned (wptr ms) + wptr_size ms).

Definition mp_addr mp := 
  (wptr mp.(mp_s) + wrepr _ mp.(mp_ofs))%R.

Definition eq_mp_word (m2:mem) mp sz v := 
  exists w:word sz, 
    v = Vword w /\ read_mem m2 (mp_addr mp) sz = ok w.

Definition eq_mp_array (m2:mem) mp s v := 
  exists (a:WArray.array s),
   v = Varr a /\
   forall off, (0 <= off < Zpos s)%Z ->
     forall v, WArray.get AAscale U8 a off = ok v ->
     read_mem m2 (wptr mp.(mp_s) + (wrepr _ (off + mp.(mp_ofs)))) U8 = ok v.

Definition eq_mp_val ty (m2:mem) mp v := 
  match ty with
  | sword ws => eq_mp_word m2 mp ws v
  | sarr  s  => eq_mp_array m2 mp s v 
  | _        => True
  end.
  
Variable (P: uprog) (ev: extra_val_t (progT := progUnit)).
Notation gd := (p_globs P).

Definition valid_pk pk mp (s2:estate) := 
  match pk with
  | Pstack z => mp = {| mp_s := MSstack; mp_ofs := z |}
  | Pstkptr z => 
    read_mem s2.(emem) (mp_addr {| mp_s := MSstack; mp_ofs := z |}) Uptr = ok (mp_addr mp)
  | Pregptr p =>
    get_var s2.(evm) p = ok (Vword (mp_addr mp))
  end.
   
Definition check_gvalid rmap x := 
  if is_glob x then 
    omap (fun ofs => {| mp_s := MSglob; mp_ofs := ofs |}) (Mvar.get pmap.(globals) (gv x))
  else 
    match check_valid rmap (gv x) with
    | Ok mp => Some mp
    | _     => None 
    end.

(* wfr_get_v_mp : 
    forall x xs mp, Mmp.get rmap.(region_vars) mp = Some xs ->
                    Sv.In x xs -> Mvar.get rmap.(var_region) x = Some mp;*)

Definition wfr_VALID (rmap:regions) :=
   forall x mp, Mvar.get rmap.(var_region) x = Some mp -> valid_mp mp (vtype x).

Definition wfr_VAL (rmap:regions) (s1:estate) (s2:estate) :=
  forall x mp v, check_gvalid rmap x = Some mp -> 
    get_gvar gd s1.(evm) x = ok v ->
    eq_mp_val (vtype (gv x)) s2.(emem) mp v.

Definition wfr_PTR (rmap:regions) (s2:estate) :=
  forall x mp, Mvar.get (var_region rmap) x = Some mp ->
    exists pk, get_local pmap x = Some pk /\ valid_pk pk mp s2. 

Class wf_region (rmap:regions) (s1:estate) (s2:estate) := {  
  wfr_valid_mp :  wfr_VALID rmap;
  wfr_val : wfr_VAL rmap s1 s2;
  wfr_ptr : wfr_PTR rmap s2;
}.

Definition eq_glob (m1 m2:mem) := 
  forall w sz, valid_pointer m1 w sz -> read_mem m1 w sz = read_mem m2 w sz.

Definition eq_not_mod (m0 m2:mem) := 
  forall ofs, 
     0 <= ofs < stk_size ->
     (forall x xofs xty, 
        local_pos x = Some(xofs, xty) -> 
        xofs + size_of xty <= ofs \/ ofs + 1 <= xofs) ->
     read_mem m0 (rsp + (wrepr _ ofs)) U8 = 
     read_mem m2 (rsp + (wrepr _ ofs)) U8.

Class valid_state (rmap:regions) (m0:mem) (s1:estate) (s2:estate) := {
   vs_val_ptr : 
      forall s w, (0 <= wunsigned w < wptr_size s) -> valid_pointer (emem s2) (wptr s + w) U8;
   vs_disj      : disjoint_ptr (emem s1);
   vs_rip       : get_var (evm s2) pmap.(vrip) = ok (Vword rip);
   vs_rsp       : get_var (evm s2) pmap.(vrsp) = ok (Vword rsp);
   vs_top_frame : ohead (frames (emem s2)) = Some (rsp, stk_size);
   vs_eq_vm     : eq_vm s1.(evm) s2.(evm);
   vs_wf_region :> wf_region rmap s1 s2;
   vs_eq_mem    : eq_glob s1.(emem) s2.(emem);
   vs_eq_not_mod: eq_not_mod m0 s2.(emem);
}.

(* -------------------------------------------------------------------------- *)

Lemma get_globalP x z : get_global pmap x = ok z <-> Mvar.get pmap.(globals) x = Some z.
Proof.
  rewrite /get_global; case: Mvar.get; last by split.
  by move=> ?;split => -[->].
Qed.

Lemma get_gvar_glob gd x vm : is_glob x -> get_gvar gd vm x = sem.get_global gd (gv x).
Proof. by rewrite /get_gvar /is_lvar /is_glob => /eqP ->. Qed.

Lemma get_gvar_nglob gd x vm : ~~is_glob x -> get_gvar gd vm x = get_var vm (gv x).
Proof. by rewrite /get_gvar /is_lvar /is_glob; case: (gs x). Qed.

Lemma cast_ptrP gd s e i : sem_pexpr gd s e >>= to_int = ok i ->
  sem_pexpr gd s (cast_ptr e) = ok (Vword (wrepr U64 i)).
Proof. by t_xrbindP => v he hi ;rewrite /cast_ptr /cast_w /= he /sem_sop1 /= hi. Qed.

Lemma cast_wordP gd s e i : 
  sem_pexpr gd s e >>= to_int = ok i ->
  exists sz (w:word sz), sem_pexpr gd s (cast_word e) = ok (Vword w) /\
                         truncate_word U64 w = ok (wrepr U64 i).
Proof.
  move=> he.
  have : exists sz (w:word sz), 
              sem_pexpr gd s (cast_ptr e) = ok (Vword w) /\
                      truncate_word U64 w = ok (wrepr U64 i). 
  + exists U64, (wrepr U64 i); split; first by apply cast_ptrP.
    by rewrite truncate_word_u.
  case: e he => // -[] // [] //=.
  move=> e he _; move: he; rewrite /sem_sop1 /=; t_xrbindP => v v' -> w.
  move=> /to_wordI [sw [w' [hsw -> ->]]] <- [<-].
  by exists sw, w'; split => //; rewrite /truncate_word hsw wrepr_unsigned.
Qed.

Lemma mk_ofsP aa sz gd s2 ofs e i :
  sem_pexpr gd s2 e >>= to_int = ok i ->
  sem_pexpr gd s2 (mk_ofs aa sz e ofs) = ok (Vword (wrepr U64 (i * mk_scale aa sz + ofs)%Z)).
Proof.
  rewrite /mk_ofs; case is_constP => /= [? [->] //| {e} e he] /=.
  rewrite /sem_sop2 /=.
  have [sz' [w [-> /= -> /=]]]:= cast_wordP he.
  by rewrite !zero_extend_u wrepr_add wrepr_mul GRing.mulrC. 
Qed.

Lemma validr_valid_pointer (m1:mem) p ws : is_ok (validr m1 p ws) = valid_pointer m1 p ws.
Proof.
  case: (Memory.readV m1 p ws); rewrite Memory.readP /CoreMem.read.
  + by move=> [w]; case: validr.
  by case: validr => //= _ [];eauto.
Qed.

Lemma check_validP rmap x mp : 
  check_valid rmap x = ok mp <->
  (Mvar.get (var_region rmap) x = Some mp /\
   exists xs,  Mmp.get (region_vars rmap) mp = Some xs /\ Sv.In x xs).
Proof.
  rewrite /check_valid.
  case heq: Mvar.get => [mp' |]; last by intuition.
  case heq1 : Mmp.get => [xs | /=]; last by split => // -[] [?]; subst mp'; rewrite heq1 => -[] ?[].
  case: ifPn => /Sv_memP hin /=.
  + split => [ [?] | [[?] ]]; subst mp' => //.
    by split => //;exists xs.
  by split => // -[] [?]; subst mp'; rewrite heq1 => -[xs'] [[<-]] /hin.
Qed.

Section EXPR.
  Variables (rmap:regions) (m0:mem) (s:estate) (s':estate).
  Hypothesis (hvalid: valid_state rmap m0 s s').

  Lemma get_var_kindP x v:
    get_var_kind pmap x = ok None -> 
    get_gvar gd (evm s) x = ok v ->
    get_gvar [::] (evm s') x = ok v.
  Proof.
    rewrite /get_var_kind; case: ifPn => hglob; first by t_xrbindP.
    case hgl : get_local => // _.
    assert (heq := vs_eq_vm hgl). 
    by rewrite !get_gvar_nglob // heq.
  Qed.

  Lemma check_mk_addr_ptr (x:var_i) aa ws xi ei e1 i1 pk mp :
    sem_pexpr [::] s' e1 >>= to_int = ok i1 ->
    get_local pmap x = Some pk ->
    Mvar.get (var_region rmap) x = Some mp ->
    is_align (wrepr Uptr mp.(mp_ofs)) ws ->
    mk_addr_ptr pmap x aa ws pk e1 = ok (xi, ei) ->
    ∃ (wx wi : u64),
      [/\ Let x := get_var (evm s') xi in to_pointer x = ok wx,
          Let x := sem_pexpr [::] s' ei in to_pointer x = ok wi, (mp_addr mp + wrepr U64 (i1 * mk_scale aa ws))%R = (wx + wi)%R
          & is_align (mp_addr mp) ws].
  Proof.
    move=> he1 hloc hgetr hal.
    rewrite /mk_addr_ptr; assert (h := wfr_ptr hgetr).
    case: h => [pk' []]; rewrite hloc => -[?]; subst pk'.
    case: pk hloc => //[ ofs | p] hloc /= hvmk [<- <-].
    + exists rsp, (wrepr U64 ofs + wrepr U64 (i1 * mk_scale aa ws))%R. 
      subst mp; rewrite (mk_ofsP aa ws ofs he1) /= /mp_addr /wptr vs_rsp /= !truncate_word_u. 
      split => //.
      + by f_equal; rewrite !(wrepr_add, wrepr_mul); ssrring.ssring.
      + by ssrring.ssring.
      by apply is_align_add.
    exists (mp_addr mp), (wrepr U64 (i1 * mk_scale aa ws)).
    rewrite (mk_ofsP aa ws 0 he1) /= hvmk /= !truncate_word_u; split => //.
    + by f_equal; rewrite !(wrepr_add, wrepr_mul, wrepr0); ssrring.ssring.
    by case: (mp) hal => -[] /= ofs ?; apply is_align_add.
  Qed.

  Lemma check_mk_addr x vpk aa ws t xi ei e1 i1: 
    sem_pexpr [::] s' e1 >>= to_int = ok i1 ->
    get_var_kind pmap x = ok (Some vpk) ->
    check_vpk_word rmap (gv x) vpk ws = ok t ->
    mk_addr pmap (gv x) aa ws vpk e1 = ok (xi, ei) ->
    exists mp wx wi, 
    [/\ check_gvalid rmap x = Some mp,
        get_var (evm s') xi >>= to_pointer = ok wx,
        sem_pexpr [::] s' ei >>= to_pointer = ok wi, 
        (mp_addr mp + wrepr Uptr (i1 * mk_scale aa ws)%Z = wx + wi)%R & 
        is_align (mp_addr mp) ws].
  Proof.
    move=> he1; rewrite /get_var_kind /check_vpk_word /mk_addr /with_var.
    case: ifP => hglob.
    + t_xrbindP => ofs /get_globalP hget <- /assertP halign [<-] <-.
      exists {| mp_s := MSglob; mp_ofs := ofs |}, rip,
        (wrepr U64 ofs + wrepr U64 (i1 * mk_scale aa ws))%R. 
      rewrite /mp_addr /= /wptr vs_rip (mk_ofsP aa ws ofs he1) /= !truncate_word_u /check_gvalid hglob hget.
      split => //.
      + by f_equal; rewrite !(wrepr_add, wrepr_mul); ssrring.ssring.
      + by ssrring.ssring.
      by apply is_align_add. 
    move=> []; case hloc: get_local => [pk | //] [<-] hc ha.
    move: hc; rewrite /check; t_xrbindP => mp hcv.
    case /check_validP: (hcv) => [hgetr _] /assertP hal.
    have [wx [wi [*]]] := check_mk_addr_ptr he1 hloc hgetr hal ha.   
    exists mp, wx, wi; split => //.
    by rewrite /check_gvalid hglob hcv.
  Qed.

  Lemma ge0_wunsigned ws (w:word ws) : 0 <= wunsigned w.
  Proof. by case (wunsigned_range w). Qed.
  
  Lemma wunsigned_repr_le ws z : 0 <= z -> wunsigned (wrepr ws z) <= z. 
  Proof. by move=> hz; rewrite wunsigned_repr; apply Z.mod_le. Qed.

  Lemma arr_is_align p ws : WArray.is_align p ws -> is_align (wrepr _ p) ws.
  Proof.
    move=> /eqP h1; apply is_align_mod.
    have hn := wsize_size_pos ws.
    have hnz : wsize_size ws ≠ 0%Z by Psatz.lia.
    by rewrite wunsigned_repr -/(wbase Uptr) (cut_wbase_Uptr ws) Z.mul_comm mod_pq_mod_q.
  Qed.

  Lemma check_gvalid_mp x mp : 
    check_gvalid rmap x = Some mp ->
    valid_mp mp (vtype (gv x)).
  Proof.
    rewrite /check_gvalid; case:ifP => hglob.
    + case heq: Mvar.get => [ofs /= | //] [<-].
      by exists (gv x); rewrite /get_global heq; split => //; rewrite Z.leb_refl.
    case heq: check_valid => [mp' | //] [?]; subst mp'.
    move: heq; rewrite check_validP => -[hget _].
    by apply: wfr_valid_mp hget.
  Qed.

  Lemma check_gvalid_valid_pointer (n:positive) x mp aa ws i : 
    vtype (gv x) = sarr n ->
    check_gvalid rmap x = Some mp -> 
    is_align (mp_addr mp) ws ->
    0 <= i * mk_scale aa ws ->
    i * mk_scale aa ws + wsize_size ws <= n ->
    WArray.is_align (i * mk_scale aa ws) ws ->
    valid_pointer (emem s') (mp_addr mp + wrepr U64 (i * mk_scale aa ws))%R ws.
  Proof.
    move=> hty hcheck hal h0i hi hala.    
    apply Memory.is_align_valid_pointer.
    + by apply is_align_add => //; apply arr_is_align.
    move=> k hk; have := check_gvalid_mp hcheck.
    rewrite hty => -[x' [/subtypeEl /= [n' [hty' hnn']] hget]].
    rewrite /mp_addr -!GRing.addrA -!wrepr_add.
    apply vs_val_ptr; split; first by apply ge0_wunsigned.
    case: (mp) hget => -[] ofs /= hget;rewrite /wptr /wptr_size /=.
    + assert (h:= wfg_ofs wf_globals); move: (h x').
      move: hget; rewrite get_globalP => hget.
      rewrite /global_pos /wptr hget => /(_ _ _ erefl) [h0z].
      rewrite hty' /= => hbound; apply: Z.le_lt_trans. 
      + by apply wunsigned_repr_le; nia.
      lia.
    assert (h:= wfg_ofs wf_locals); move: (h x').
    rewrite /local_pos /wptr hget => /(_ _ _ erefl) [h0z].
    rewrite hty' /= => hbound; apply: Z.le_lt_trans. 
    + by apply wunsigned_repr_le; nia.
    lia.
  Qed.
   
  Lemma get_arr_read_mem (n:positive) (t : WArray.array n) mp aa ws i w x:
    vtype (gv x) = sarr n ->
    eq_mp_val (sarr n) (emem s') mp (Varr t) ->
    check_gvalid rmap x = Some mp ->
    WArray.get aa ws t i = ok w ->
    is_align (mp_addr mp) ws ->
    read_mem (emem s') (mp_addr mp + wrepr U64 (i * mk_scale aa ws)) ws = ok w.
  Proof.
    move=> hty heq hval hget hal.
    rewrite Memory.readP /CoreMem.read.
    have [hbound1 hbound2 hbound3] := WArray.get_bound hget.
    have := check_gvalid_valid_pointer hty hval hal hbound1 hbound2 hbound3. 
    rewrite -validr_valid_pointer => /is_okP [?] /= => hv; rewrite hv.
    case: heq => t' [] /Varr_inj [] heq.
    rewrite (Eqdep_dec.UIP_dec Pos.eq_dec heq erefl) /= => {heq} ? hgr;subst t'.
    move: (hget);rewrite /WArray.get /CoreMem.read; t_xrbindP => ? _ <-.
    do 2 f_equal; rewrite /CoreMem.uread.
    apply eq_map_ziota => k hk /=.
    have [v hget8]:= WArray.get_get8 AAscale hget hk.
    have /WArray.get_uget -> := hget8.
    move /hgr: hget8; rewrite Memory.readP /CoreMem.read.
    have h: 0 <= i * mk_scale aa ws + k ∧ i * mk_scale aa ws + k < n by lia.
    move=> /(_ h){h}; t_xrbindP => ? hvr.
    have -> : (wptr (mp_s mp) + wrepr U64 (i * mk_scale aa ws + k + mp_ofs mp))%R = 
              ((mp_addr mp + wrepr U64 (i * mk_scale aa ws)) + wrepr U64 k)%R.
    + by rewrite /mp_addr !wrepr_add; ssrring.ssring.
    by rewrite CoreMem.uread8_get Memory.addP => ->.
  Qed.

  Let X e : Prop :=
    ∀ e' v,
      alloc_e pmap rmap e = ok e' →
      sem_pexpr gd s e = ok v →
      sem_pexpr [::] s' e' = ok v.

  Let Y es : Prop :=
    ∀ es' vs,
      alloc_es pmap rmap es = ok es' →
      sem_pexprs gd s es = ok vs →
      sem_pexprs [::] s' es' = ok vs.

  Lemma check_varP (x:var_i) t: 
    check_var pmap x = ok t -> 
    get_var_kind pmap (mk_lvar x) = ok None.
  Proof. by rewrite /check_var /get_var_kind /=; case: get_local. Qed.

  Lemma check_e_esP : (∀ e, X e) * (∀ es, Y es).
  Proof.
    apply: pexprs_ind_pair; subst X Y; split => /=.
    + by move=> ?? [<-] [<-].
    + move=> e he es hes ??; t_xrbindP => e' /he{he}he es' /hes{hes}hes <- /=.
      by move=> v /he -> vs /hes -> <-.
    + by move=> z ?? [<-] [<-].
    + by move=> b ?? [<-] [<-].
    + by move=> n ?? [<-] [<-].
    + move=> x e' v; t_xrbindP => -[ vpk | ] hgvk; last first.
      + by move=> [<-]; apply: get_var_kindP hgvk.
      case hty: is_word_type => [ws | //]; move /is_word_typeP: hty => hty.
      t_xrbindP => ? hcheck [xi ei] haddr <- /=.
      have h0: Let x := sem_pexpr [::] s' 0 in to_int x = ok 0 by done.
      have [mp [wx [wi [hgval -> -> /= <- hal ]]]]:= check_mk_addr h0 hgvk hcheck haddr.
      by move=> /(wfr_val hgval); rewrite hty wrepr0 GRing.addr0 => -[ w [-> ->]].
    + move=> aa sz x e1 he1 e' v he'; apply: on_arr_gvarP => n t hty /= hget.
      t_xrbindP => i vi /he1{he1}he1 hvi w hw <-.
      move: he'; t_xrbindP => e1' /he1{he1}he1' _ _. 
      have h0 : sem_pexpr [::] s' e1' >>= to_int = ok i.
      + by rewrite he1'.
      move=> [vptr | ]; last first.
      + by move => /get_var_kindP -/(_ _ hget) h [<-] /=; rewrite h /= h0 /= hw.
      t_xrbindP => hgvk ? hcheck [xi ei] haddr <- /=.
      have [mp [wx [wi [hgval -> -> /= <- hal ]]]] := check_mk_addr h0 hgvk hcheck haddr.
      assert (h:= wfr_val hgval hget); move: h;rewrite hty => h.
      by rewrite (get_arr_read_mem hty h hgval hw hal).
    + move=> sz1 v1 e1 IH e2 v.
      t_xrbindP => ? /check_varP hc e1' /IH hrec <- wv1 vv1 /= hget hto' we1 ve1.
      move=> /hrec -> hto wr hr ?;subst v. 
      have := get_var_kindP hc hget; rewrite /get_gvar /= => -> /=.
      by rewrite hto' hto /= -(vs_eq_mem (read_mem_valid_pointer hr)) hr.
    + move=> o1 e1 IH e2 v.
      by t_xrbindP => e1' /IH hrec <- ve1 /hrec /= ->.
    + move=> o1 e1 H1 e1' H1' e2 v.
      by t_xrbindP => e1_ /H1 hrec e1'_ /H1' hrec' <- ve1 /hrec /= -> /= ve2 /hrec' ->.
    + move => e1 es1 H1 e2 v.
      t_xrbindP => es1' /H1{H1}H1 <- vs /H1{H1} /=.
      by rewrite /sem_pexprs => ->.
    move=> t e He e1 H1 e1' H1' e2 v.   
    t_xrbindP => e_ /He he e1_ /H1 hrec e1'_ /H1' hrec' <-.
    by move=> b vb /he /= -> /= -> ?? /hrec -> /= -> ?? /hrec' -> /= -> /= ->.
  Qed.

  Definition alloc_eP := check_e_esP.1.
  Definition alloc_esP := check_e_esP.2.

End EXPR.

Lemma check_diffP x t : check_diff pmap x = ok t -> ~Sv.In x (vnew pmap).
Proof. by rewrite /check_diff; case:ifPn => /Sv_memP. Qed.

Lemma get_var_eq x vm v : get_var vm.[x <- v] x = on_vu (pto_val (t:=vtype x)) undef_error v.
Proof. by rewrite /get_var Fv.setP_eq. Qed.

Lemma get_var_neq x y vm v : x <> y -> get_var vm.[x <- v] y = get_var vm y.
Proof. by move=> /eqP h; rewrite /get_var Fv.setP_neq. Qed.

Lemma get_var_set_eq vm1 vm2 (x y : var) v: 
  get_var vm1 y = get_var vm2 y ->
  get_var vm1.[x <- v] y = get_var vm2.[x <- v] y.
Proof.
  by case:( x =P y) => [<- | hne]; rewrite !(get_var_eq, get_var_neq).
Qed.

Lemma is_lvar_is_glob x : is_lvar x = ~~is_glob x.
Proof. by case: x => ? []. Qed.

Lemma get_gvar_eq gd x vm v : 
  ~is_glob x -> get_gvar gd vm.[gv x <- v] x = on_vu (pto_val (t:=vtype (gv x))) undef_error v.
Proof. 
  by move=> /negP => h; rewrite /get_gvar is_lvar_is_glob h get_var_eq.
Qed.

Lemma get_gvar_neq gd (x:var) y vm v : (~is_glob y -> x <> (gv y)) -> get_gvar gd vm.[x <- v] y = get_gvar gd vm y.
Proof. 
  move=> h; rewrite /get_gvar is_lvar_is_glob. 
  by case: negP => // hg; rewrite get_var_neq //; apply h.
Qed.

Lemma get_gvar_set_eq gd vm1 vm2 x y v: 
  get_gvar gd vm1 y = get_gvar gd vm2 y ->
  get_gvar gd vm1.[x <- v] y = get_gvar gd vm2.[x <- v] y.
Proof.
  case : (@idP (is_glob y)) => hg; first by rewrite !get_gvar_neq.
  case:( x =P (gv y)) => heq; last by rewrite !get_gvar_neq.
  by move: v; rewrite heq => v; rewrite !get_gvar_eq.
Qed.

Lemma get_localn_checkg_diff rmap mp s2 x y : 
  get_local pmap x = None ->
  wfr_PTR rmap s2 ->
  check_gvalid rmap y = Some mp ->
  (~is_glob y -> x <> (gv y)).
Proof.
  rewrite /check_gvalid /wfr_PTR; case:ifPn => // hg hl hwf.
  case heq: check_valid => [mp' | // ].
  move=> [?] _; subst mp'.
  have /check_validP [ /hwf [mpy [hy _]] _] := heq.
  by move=> hx; subst x; move: hy; rewrite hl.
Qed.

Lemma valid_state_set_var rmap m0 s1 s2 x v:
  valid_state rmap m0 s1 s2 ->
  get_local pmap x = None ->
  ¬ Sv.In x (vnew pmap) ->
  valid_state rmap m0 (with_vm s1 (evm s1).[x <- v]) (with_vm s2 (evm s2).[x <- v]).
Proof.
  case: s1 s2 => mem1 vm1 [mem2 vm2] [/=] hvptr hdisj hrip hrsp hf heqvm hwfr heqg hnotm hget hnin.
  constructor => //=.
  + by rewrite get_var_neq //; assert (h:=rip_in_new); SvD.fsetdec.
  + by rewrite get_var_neq //; assert (h:=rsp_in_new); SvD.fsetdec.
  + by move=> y hy; apply get_var_set_eq; apply heqvm.
  rewrite /with_vm /=; case: hwfr => /= hgmp hval hptr.
  constructor => //=.
  + move=> y mp vy hy; have ? := get_localn_checkg_diff hget hptr hy.
    by rewrite get_gvar_neq //; apply hval.
  move=> y mp hy; have [pk [hgety hpk]]:= hptr y mp hy; exists pk; split => //.
  case: pk hgety hpk => //= yp hyp.  
  assert (h := disj_ptr hyp); case: h => _ _ hin _ <-.
  by rewrite get_var_neq //; SvD.fsetdec.
Qed.

Lemma set_wordP rmap x ws rmap': 
  set_word rmap x ws = ok rmap' ->
  exists mp,
  [/\ Mvar.get (var_region rmap) x = Some mp,
      mp_s mp = MSstack, 
      is_align (wrepr Uptr (mp_ofs mp)) ws &
      rmap' = rset_word rmap x mp]. 
Proof.
  rewrite /set_word.
  case heq : Mvar.get => [ [[] ofs] | ] //=.
  t_xrbindP => ? /assertP hal <-.
  by eexists; split; first reflexivity.
Qed.

Lemma wsize_size_le ws ws': (ws ≤ ws')%CMP -> (wsize_size ws <= wsize_size ws').
Proof. by case: ws ws' => -[]. Qed.

Lemma size_of_le ty ty' : subtype ty ty' -> size_of ty <= size_of ty'.
Proof.
  case: ty => [||p|ws]; case:ty' => [||p'|ws'] //.
  + by move=> /ZleP.
  by apply wsize_size_le.
Qed.
  
Lemma gt0_size_of ty : 0 < size_of ty.
Proof. by case: ty. Qed.

Lemma check_gvalid_rset_word rmap x y mp mpy: 
  mp_s mp = MSstack ->
  Mvar.get (var_region rmap) x = Some mp ->
  check_gvalid (rset_word rmap x mp) y = Some mpy ->
  [/\ ~is_glob y, x = (gv y) & mp = mpy] \/ 
  [/\ (~is_glob y -> x <> gv y), mp <> mpy &check_gvalid rmap y = Some mpy].
Proof.
  rewrite /check_gvalid /rset_word => hmps hgx.
  case:ifPn => ?.
  + move=> h; right;split => //.
    by case: Mvar.get h => // ? [<-] => ?; subst mp.
  case heq : check_valid => [mp1 | //] [?]; subst mp1.
  move:heq; rewrite check_validP => /= -[hgy [xs []]].
  rewrite Mmp.setP; case: eqP => [? [?]| ].
  + by subst mpy xs => /SvD.F.singleton_iff ?; left.
  move=> hd hg /Sv_memP hin; right; split => //.
  + by move=> _ ?; subst x; apply hd; move: hgy;rewrite hgx => -[].
  by rewrite /check_valid hgy hg hin.
Qed.

Lemma get_global_pos x ofs: 
  get_global pmap x = ok ofs ->
  global_pos x = Some (ofs, vtype x).
Proof. by rewrite get_globalP /global_pos => ->. Qed.

Lemma get_local_pos x ofs:
  get_local pmap x = Some (Pstack ofs) ->
  local_pos x = Some (ofs, vtype x).
Proof. by rewrite /local_pos => ->. Qed.

Lemma valid_mp_bound mp ty : 
  valid_mp mp ty ->
  0 <= mp_ofs mp ∧ 
  mp_ofs mp + size_of ty <= wptr_size (mp_s mp).
Proof.
  move=> [x [/size_of_le hsub hv]].
  case: eqP hv => heq.
  + rewrite heq => /get_global_pos hgp. 
    assert (h:= wfg_ofs wf_globals hgp); rewrite /wptr_size /=; lia.
  move=> /get_local_pos hgx.
  assert (h:= wfg_ofs wf_locals hgx).
  have -> : mp_s mp = MSstack by case: (mp_s mp) heq.
  rewrite /wptr_size /=; lia.
Qed.

Lemma valid_mp_addr mp ty: 
  valid_mp mp ty ->
  wunsigned (mp_addr mp) = wunsigned (wptr (mp_s mp)) + (mp_ofs mp).
Proof.
  move=> /valid_mp_bound; rewrite /mp_addr => h.
  apply wunsigned_add; have ? := gt0_size_of ty.
  have := @ge0_wunsigned Uptr (wptr (mp_s mp)).
  by case: (mp_s mp) h; rewrite /wptr /wptr_size /=; lia.
Qed.

Lemma valid_mp_addr_bound mp ty: 
  valid_mp mp ty ->
  wunsigned (wptr (mp_s mp)) <= wunsigned (mp_addr mp) /\
  wunsigned (mp_addr mp) + size_of ty <= wunsigned (wptr (mp_s mp)) + wptr_size (mp_s mp).
Proof.
  move=> h; rewrite (valid_mp_addr h); have := (valid_mp_bound h); lia.
Qed.

Lemma ms_bound s : wunsigned (wptr s) + wptr_size s <= wbase Uptr.
Proof. by case: s. Qed.

Lemma disjoint_eq_mp_val p mp ty mem1 mem2 ws w v: 
  valid_mp mp ty ->
  disjoint_zrange p (wsize_size ws) (mp_addr mp) (size_of ty) ->
  write_mem mem1 p ws w = ok mem2 ->
  eq_mp_val ty mem1 mp v → 
  eq_mp_val ty mem2 mp v.
Proof.
  have hba := @ms_bound (mp_s mp); have hge0 := @ge0_wunsigned Uptr (mp_addr mp).
  move=> /valid_mp_addr_bound.
  case: ty => // [pt | ws']; rewrite /size_of => hbound hd hw /=.
  + move=> [t] [-> heq]; exists t;split => // off hoff v1 /(heq _ hoff) <-.     
    apply: (Memory.writeP_neq hw).
    case: hd => hno1 hno2 hd; rewrite /disjoint_range /disjoint_zrange wsize8.
    rewrite wrepr_add (GRing.addrC (wrepr _ _)) GRing.addrA -/(mp_addr mp).
    have heqa : wunsigned (mp_addr mp + wrepr U64 off) = wunsigned (mp_addr mp) + off.
    + rewrite wunsigned_add //; lia.
    split => //; rewrite /no_overflow ?zify; lia.
  move=> [w1] [-> heq]; exists w1; split => //; rewrite -heq.
  by apply: (Memory.writeP_neq hw). 
Qed.

Lemma valid_mp_disjoint mp1 mp2 ty1 ty2: 
  valid_mp mp1 ty1 -> 
  valid_mp mp2 ty2 ->
  mp1 <> mp2 -> 
  wunsigned (mp_addr mp1) + size_of ty1 <= wunsigned (mp_addr mp2) ∨ 
  wunsigned (mp_addr mp2) + size_of ty2 <= wunsigned (mp_addr mp1).
Proof.
  move=> h1 h2; rewrite (valid_mp_addr h1) (valid_mp_addr h2).
  case: mp1 mp2 h1 h2 => [ms1 ofs1] [ms2 ofs2].
  move=> [x1 [/size_of_le hle1 /= hget1]] [x2 [/size_of_le hle2 /= hget2]].
  have ? := gt0_size_of (vtype x1); have ? := gt0_size_of (vtype x2).
  assert (wfg := wf_globals); assert (wfl := wf_locals).
  case: ms1 hget1 => /= [/get_global_pos | /get_local_pos] h1.
  + have hg1 := wfg_ofs wfg h1; have hg2 := wfg_disj wfg _ h1.
    case: ms2 hget2 => /= [/get_global_pos | /get_local_pos] h2 hd.
    + have hxd: x1 <> x2 by move=> h;apply hd; move: h2; rewrite -h h1 => -[->].
      by have:= hg2 _ _ _ hxd h2; lia.
    have hl2:=  wfg_ofs wfl h2; rewrite /wptr /wptr_size /=; lia.
  have hl1 := wfg_ofs wfl h1; have hl2 := wfg_disj wfl _ h1.
  case: ms2 hget2 => /= [/get_global_pos | /get_local_pos] h2 hd.
  + have hg2:=  wfg_ofs wfg h2; rewrite /wptr /wptr_size /=; lia.
  have hxd: x1 <> x2 by move=> h;apply hd; move: h2; rewrite -h h1 => -[->].
  by have:= hl2 _ _ _ hxd h2; lia.
Qed.

Lemma eq_mp_val_write_disj rmap m0 s1 s2 x y mp mpy mem2 v ws w p:
  valid_state rmap m0 s1 s2 ->  
  Mvar.get (var_region rmap) x = Some mp ->
  mp_s mp = MSstack ->
  between (mp_addr mp) (size_of (vtype x)) p ws ->
  check_gvalid rmap y = Some mpy ->
  mp ≠ mpy ->
  write_mem (emem s2) p ws w = ok mem2 ->
  eq_mp_val (vtype (gv y)) (emem s2) mpy v → 
  eq_mp_val (vtype (gv y)) mem2 mpy v.
Proof.
  move=> hvs hgx hs hb hcv hd.
  have hmpy := check_gvalid_mp hvs hcv; assert (hmp := wfr_valid_mp hgx).
  apply: disjoint_eq_mp_val => //.
  move: hb; rewrite /between !zify.
  have ? := valid_mp_addr_bound hmp; have ? := valid_mp_addr_bound hmpy.
  have ? := @ms_bound (mp_s mp); have ? := @ms_bound (mp_s mpy).
  split; rewrite /no_overflow ?zify. 
  + lia. + lia. 
  have := valid_mp_disjoint hmp hmpy hd; lia.
Qed.

Lemma get_local_pos_sptr x ofs : get_local pmap x = Some (Pstkptr ofs) -> local_pos x = Some(ofs, sword Uptr).
Proof. by rewrite /local_pos => ->. Qed.

Lemma sptr_addr x ofs: 
  local_pos x = Some (ofs, sword Uptr) ->
  wunsigned (mp_addr {| mp_s := MSstack; mp_ofs := ofs |}) = wunsigned rsp + ofs.
Proof.
  move=> h; assert (h1:= wfg_ofs wf_locals h).
  rewrite /mp_addr /wptr /=; apply wunsigned_add.
  move: (gt0_size_of (sword Uptr)) (@ge0_wunsigned Uptr rsp); lia.
Qed.

Lemma sptr_addr_bound x ofs:
  local_pos x = Some (ofs, sword Uptr) ->
  wunsigned rsp <= wunsigned (mp_addr {| mp_s := MSstack; mp_ofs := ofs |}) /\
  wunsigned (mp_addr {| mp_s := MSstack; mp_ofs := ofs |}) + wsize_size Uptr <= wunsigned rsp + stk_size.
Proof.
  move=> h; rewrite (sptr_addr h).
  assert (h1:= wfg_ofs wf_locals h); move: h1 => /=; lia.
Qed.

Lemma wfr_PTR_rset_word rmap m0 s1 s2 x mp p ws w mem2:
  valid_state rmap m0 s1 s2 ->  
  Mvar.get (var_region rmap) x = Some mp ->
  mp_s mp = MSstack ->
  between (mp_addr mp) (size_of (vtype x)) p ws ->
  write_mem (emem s2) p ws w = ok mem2 ->
  wfr_PTR rmap s2 ->
  wfr_PTR (rset_word rmap x mp) (with_mem s2 mem2).
Proof.
  move=> hvs hgx hms hb hw hwf y mpy /hwf [pk] [hgl hvpk]; exists pk;split => //.
  case: pk hgl hvpk => //= ofs hy <-.
  apply: (Memory.writeP_neq hw).
  assert (hmp := wfr_valid_mp hgx).
  case: (hmp) => x' [] /size_of_le hle; rewrite hms /= => hx.
  have hly:= get_local_pos_sptr hy; have hlx := get_local_pos hx.
  split.
  + by apply (is_align_no_overflow (Memory.valid_align (write_mem_valid_pointer hw))).
  + rewrite /no_overflow zify.
    have := sptr_addr_bound hly; lia.
  move: hb; rewrite /between !zify.
  assert (hwl := wf_locals).
  have hd : y <> x' by move=> heq;move: hy;rewrite heq hx.
  have /= := wfg_disj hwl hd hly hlx.
  rewrite (sptr_addr hly) (valid_mp_addr hmp) hms /wptr /=; lia.
Qed.

Lemma eq_glob_rset_word rmap m0 s1 s2 x mp p ws w mem2:
  valid_state rmap m0 s1 s2 ->  
  Mvar.get (var_region rmap) x = Some mp ->
  mp_s mp = MSstack ->
  between (mp_addr mp) (size_of (vtype x)) p ws ->
  write_mem (emem s2) p ws w = ok mem2 ->
  eq_glob (emem s1) (emem s2) ->
  eq_glob (emem s1) mem2.
Proof.
  move=> hvs hgx hms hb hw heg p1 ws1 hvp.
  rewrite (heg _ _ hvp); symmetry; apply: (Memory.writeP_neq hw).
  assert (hd:= vs_disj (ms:= mp_s mp) hvp); move:hd; rewrite hms /wptr /wptr_size /= => hd.
  assert (hmp := wfr_valid_mp hgx).
  case: (hmp) => x' [] /size_of_le hle; rewrite hms /= => hx.
  split.
  + by apply (is_align_no_overflow (Memory.valid_align (write_mem_valid_pointer hw))).
  + by apply: is_align_no_overflow (Memory.valid_align hvp).
  move: hb; rewrite /between !zify.
  assert (hwl := wfg_ofs wf_locals (get_local_pos hx)).
  rewrite (valid_mp_addr hmp) /wptr hms /=; lia.
Qed.

Lemma eq_not_mod_rset_word rmap m0 s1 s2 x mp p ws w mem2:
  valid_state rmap m0 s1 s2 ->  
  Mvar.get (var_region rmap) x = Some mp ->
  mp_s mp = MSstack ->
  between (mp_addr mp) (size_of (vtype x)) p ws ->
  write_mem (emem s2) p ws w = ok mem2 ->
  eq_not_mod m0 (emem s2) -> 
  eq_not_mod m0 mem2. 
Proof.
  move=> hvs hgx hms hb hw heg p1 ws1 hvp.
  rewrite (heg _ _ hvp) //; symmetry; apply: (Memory.writeP_neq hw).
  assert (hmp := wfr_valid_mp hgx).
  case: (hmp) => x' [] /size_of_le hle; rewrite hms /= => hx.
  split.
  + by apply (is_align_no_overflow (Memory.valid_align (write_mem_valid_pointer hw))).
  + by apply: is_align_no_overflow; apply is_align8.
  move: hb; rewrite /between !zify.
  have:= hvp _ _ _ (get_local_pos hx).
  rewrite wunsigned_add; last by have := @ge0_wunsigned _ rsp; lia.
  rewrite (valid_mp_addr hmp) /wptr hms wsize8 /=; lia.
Qed.

Lemma alloc_lvalP rmap r1 r2 v ty m0 (s1 s2: estate) :
  alloc_lval pmap rmap r1 ty = ok r2 -> 
  valid_state rmap m0 s1 s2 -> 
  type_of_val v = ty ->
  forall s1', write_lval gd r1 v s1 = ok s1' ->
  exists s2', write_lval [::] r2.2 v s2 = ok s2' /\ valid_state r2.1 m0 s1' s2'.
Proof.
  move=> ha hvs ?; subst ty.
  case: r1 ha => /=.
  (* Lnone *)
  + move=> vi ty1 [<-] /= s1' /write_noneP [->] h; exists s2; split => //.
    by rewrite /write_none; case: h => [ [? ->]| [-> ->]].

  (* Lvar *)
  + move=> x; case hlx: get_local => [pk | ]; last first.
    + t_xrbindP => ? /check_diffP hnnew <- s1'.
      rewrite /write_var; t_xrbindP => vm1 hvm1 <- /=.
      by apply: set_varP hvm1=> [v' hv <- | hb hv <-]; rewrite /write_var /set_var hv /= ?hb /=;
        eexists;(split;first by reflexivity); apply valid_state_set_var.
    case heq: is_word_type => [ws | //]; move /is_word_typeP : heq => hty.
    case: eqP => htyv //; rewrite /write_var.
    t_xrbindP => -[xi ei] ha rmap2 hsetw <- s1' vm1' hvm1' ?; subst s1' => /=.
    have he1 : sem_pexpr [::] s2 0 >>= to_int = ok 0 by done.
    have [mp [hgetr hins hal ->]] := set_wordP hsetw. 
    have [wx [wi [-> -> /= <- hmpal]]]:= check_mk_addr_ptr hvs he1 hlx hgetr hal ha.
    move: hvm1'; apply set_varP; last by rewrite {1}hty.
    move=> {ha hsetw}; case: x hty hlx hgetr => -[xty xn] xii /= ->.
    set x := {| vtype := sword ws; vname := xn |} => hlx hgetr /= w.
    move=> /(type_of_val_to_pword htyv) [w' [??]] ?; subst v vm1' => /=.
    rewrite truncate_word_u /= wrepr0 GRing.addr0.
    assert (hmp := wfr_valid_mp hgetr).
    have ha := valid_mp_addr hmp; have /= hab:= valid_mp_addr_bound hmp.
    have hmpb := valid_mp_bound hmp; have hmsb:= @ms_bound (mp_s mp).
    have hvp: valid_pointer (emem s2) (mp_addr mp) ws.
    + apply Memory.is_align_valid_pointer => //.
      move=> k hk.
      rewrite /mp_addr -GRing.addrA -wrepr_add.
      apply vs_val_ptr; split; first by apply ge0_wunsigned.
      by apply: Z.le_lt_trans; first apply wunsigned_repr_le; lia.
    have /Memory.writeV -/(_ w') [mem2 hmem2] := hvp.
    rewrite hmem2 /=; eexists;split;first by reflexivity.
    (* valid_state update word *)
    case:(hvs) => vptr hdisj hrip hrsp hf heqvm hwfr heqg hnotm. 
    have hbet: between (mp_addr mp) (size_of (vtype x)) (mp_addr mp) ws.
    + by rewrite /between /x /= !Z.leb_refl.
    constructor => //=.
    + by move=> ???; rewrite (Memory.write_valid _ _ hmem2); apply vptr.
    + by rewrite -(ss_frames (Memory.write_mem_stable hmem2)).
    + move=> y hy; rewrite get_var_neq; first by apply heqvm.
      by move=> heq;move: hy; rewrite -heq hlx.
    + case: (hwfr) => h1 h2 h3; constructor => //=.
      + move=> y mpy v /(check_gvalid_rset_word hins hgetr) [].
        + move=> [ hng heq ?]; subst mpy.
          have -> : y = mk_lvar {|v_var := x; v_info := v_info (gv y) |}.
          + by case: y hng heq => -[yv vi] [] //= _ ->.
          have /= -> // := @get_gvar_eq gd (mk_lvar {| v_var := x; v_info := v_info (gv y) |}) (evm s1) (ok w).
          move=> [<-]; exists w';split;first by subst w.
          by apply: Memory.writeP_eq hmem2.
        move=> [hd hdm hcv]; rewrite get_gvar_neq // => /h2 -/(_ _ hcv) /=.
        by apply: (eq_mp_val_write_disj hvs hgetr) hmem2.
      by apply: wfr_PTR_rset_word hmem2 h3.
    + by apply: (eq_glob_rset_word hvs hgetr) hmem2 heqg.
    by apply: (eq_not_mod_rset_word hvs hgetr) hmem2 hnotm.

  (* Lmem *)
  + move=> ws x e1; t_xrbindP => ? /check_varP hx e1' /(alloc_eP hvs) he1 <-.
    move=> s1' xp ? hgx hxp w1 v1 /he1 he1' hv1 w hvw mem2 hw <- /=.
    have := get_var_kindP hvs hx; rewrite /get_gvar /= => /(_ _ hgx) -> /=.
    rewrite he1' hxp /= hv1 /= hvw /=.
    have hvp1 := write_mem_valid_pointer hw.
    have hvp2: valid_pointer (emem s2) (xp + w1) ws.
    + by apply /Memory.readV; assert (h := vs_eq_mem hvp1); rewrite -h; apply /Memory.readV.
    have /Memory.writeV  -/(_ w) [mem2' hmem2] := hvp2.
    rewrite hmem2 /=; eexists;split;first reflexivity.
    (* valid_state update mem *)
    case:(hvs) => vptr hdisj hrip hrsp hf heqvm hwfr heqg hnotm. 
    have ha1 := Memory.valid_align hvp1.
    have hno1 := is_align_no_overflow ha1.
    constructor => //=.
    + by move=> ???; rewrite (Memory.write_valid _ _ hmem2); apply vptr.
    + by move=> ??; rewrite (Memory.write_valid _ _ hw); apply hdisj.
    + by rewrite -(ss_frames (Memory.write_mem_stable hmem2)).
    + case: (hwfr) => h1 h2 h3; constructor => //=.
      + move=> y mpy vy hcv hgy.
        have hmp := check_gvalid_mp hvs hcv.
        apply: (disjoint_eq_mp_val hmp _ hmem2 (h2 _ _ _ hcv hgy)).
        have hb1 := valid_mp_addr_bound hmp; have hb2 := @ms_bound (mp_s mpy).
        split => //.
        + by rewrite /no_overflow !zify; lia.
        by have:= hdisj _ _ (mp_s mpy) hvp1; lia.
      move=> y mpy hgy.
      have [pk [hgpk hvpk]] := h3 _ _ hgy; exists pk; split => //.
      case: pk hgpk hvpk => //= ofs /get_local_pos_sptr hgpk <-.
      apply: (Memory.writeP_neq hmem2).
      have h := sptr_addr_bound hgpk; split => //.
      + rewrite /no_overflow !zify; lia.
      by have:= hdisj _ _ MSstack hvp1; rewrite /wptr /wptr_size /=; lia.
    + move=> w2 sz; rewrite (Memory.write_valid _ _ hw) => hv.
      by apply: Memory.read_write_any_mem (hw) (hmem2) => //; apply heqg.
    move=> ofs hofs hl; rewrite hnotm //.
    symmetry; apply: (Memory.writeP_neq hmem2).
    split => //.
    + by rewrite /no_overflow zify wsize8; have := wunsigned_range (rsp + wrepr U64 ofs); lia.
    rewrite wsize8 wunsigned_add; last by have := @ge0_wunsigned _ rsp; lia.
    by have:= hdisj _ _ MSstack hvp1; rewrite /wptr /wptr_size /=; lia.
  
  (* Laset *)
  move=> aa ws x e1; t_xrbindP => e1' /(alloc_eP hvs) he1.
  move=> hr2 s1'; apply on_arr_varP => n t hty hxt.
  t_xrbindP => i1 v1 /he1 he1' hi1 w hvw t' htt' vm1 hvm1 ?; subst s1'.
  case hlx: get_local hr2 => [pk | ]; last first.
  + t_xrbindP => ? /check_diffP hnnew <-.
    have /get_var_kindP -/(_ _ hxt) : get_var_kind pmap (mk_lvar x) = ok None.
    + by rewrite /get_var_kind /= hlx.
    rewrite /get_gvar /= => hxt2.
    rewrite he1' /= hi1 hxt2 /= hvw /= htt' /=.
    apply: set_varP hvm1=> [v' hv <- | ]; last by rewrite {1} hty.
    rewrite /set_var hv /=.
    by eexists;(split;first by reflexivity); apply valid_state_set_var. 
  rewrite /check; t_xrbindP => ? mp hc /assertP halign -[xi ei] ha rmap2 hsetw <- /=.
  have {he1} he1 : sem_pexpr [::] s2 e1' >>= to_int = ok i1 by rewrite he1'.
  have [mp' [hgetr hins hal ->]] := set_wordP hsetw. 
  have ? : mp = mp'.
  + by move /check_validP : hc; rewrite hgetr => -[[->]].
  subst mp'.
  have [wx [wi [-> -> /= <- hmpal]]]:= check_mk_addr_ptr hvs he1 hlx hgetr hal ha.
  move: hvm1; apply set_varP; last by rewrite {1}hty.
  move=> {ha hsetw}; case: x hty hlx hxt hc hgetr => -[xty xn] xii /= ->.
  set x := {| vtype := sarr n; vname := xn |} => hlx hxt hc hgetr /= ti [?] ?; subst ti vm1.
  rewrite hvw /=.
  have [hge0 hlen haa] := WArray.set_bound htt'.
  assert (hmp := wfr_valid_mp hgetr).
  have ha := valid_mp_addr hmp; have /= hab:= valid_mp_addr_bound hmp.
  have /= hmpb := valid_mp_bound hmp; have hmsb:= @ms_bound (mp_s mp).
  have hvp: valid_pointer (emem s2) (mp_addr mp + wrepr Uptr (i1 * mk_scale aa ws)) ws.
  + apply Memory.is_align_valid_pointer. 
    + by apply is_align_add => //; apply arr_is_align. 
    move=> k hk; rewrite /mp_addr -!GRing.addrA -!wrepr_add.
    apply vs_val_ptr; split; first by apply ge0_wunsigned.
    by apply: Z.le_lt_trans; first apply wunsigned_repr_le; lia.
  have /Memory.writeV -/(_ w) [mem2 hmem2] := hvp.
  rewrite hmem2 /=; eexists;split;first by reflexivity.
  (* valid_state update array *)
  have hgt0ws := wsize_size_pos ws; have ge0_mps := @ge0_wunsigned _ (wptr (mp_s mp)).
  have heqa : wunsigned (mp_addr mp + wrepr U64 (i1 * mk_scale aa ws)) = 
              wunsigned (mp_addr mp) + i1 * mk_scale aa ws.
  + by apply wunsigned_add; lia.
  case:(hvs) => vptr hdisj hrip hrsp hf heqvm hwfr heqg hnotm. 
  have hbet: between (mp_addr mp) (size_of (vtype x)) (mp_addr mp + wrepr U64 (i1 * mk_scale aa ws)) ws.
  + by rewrite /between /x /= !zify; lia.
  constructor => //=.
  + by move=> ???; rewrite (Memory.write_valid _ _ hmem2); apply vptr.
  + by rewrite -(ss_frames (Memory.write_mem_stable hmem2)).
  + move=> y hy; rewrite get_var_neq; first by apply heqvm.
    by move=> heq;move: hy; rewrite -heq hlx.
  + case: (hwfr) => h1 h2 h3; constructor => //=.
    + move=> y mpy vy /(check_gvalid_rset_word hins hgetr) [].
      + move=> [ hng heq ?]; subst mpy.
        have -> : y = mk_lvar {|v_var := x; v_info := v_info (gv y) |}.
        + by case: y hng heq => -[yv vi] [] //= _ ->.
        set X := (mk_lvar {| v_var := x; v_info := v_info (gv y) |}).
        have /= -> // := @get_gvar_eq gd X (evm s1) (ok (WArray.inject n t')).
        move=> [<-]; eexists (WArray.inject n t');split => //.
        move=> off hoff v0.
        have -> :  WArray.get AAscale U8 (WArray.inject n t') off = WArray.get AAscale U8 t' off.
        + by rewrite /WArray.inject Z.ltb_irrefl.
        rewrite Z.add_comm wrepr_add GRing.addrA -/(mp_addr mp) (WArray.set_get8 off htt') /=.
        have -> /= := Memory.write_read8 (mp_addr mp + wrepr U64 off) hmem2.
        set t1 := (x in (if x then _ else _)).
        set t2 := (x in _ -> (if x then _ else _) = _).
        have <- : t1 = t2.
        + rewrite Bool.eq_iff_eq_true -/(is_true t1) -/(is_true t2) /t1 /t2 !zify.
          by rewrite !(@wunsigned_add _ (mp_addr mp)); lia.
        case:ifPn; rewrite /t1 !zify => hi {t1 t2}.
        + move=> [<-]; do 2 f_equal; rewrite !(@wunsigned_add _ (mp_addr mp)); lia.
        move=> hh; have hcg : check_gvalid rmap X = Some mp.
        + by rewrite /check_gvalid /= hc.
        have /= [t1 [/Varr_inj]]:= h2 _ _ _ hcg hxt.
        move=> [heq1]; rewrite (Eqdep_dec.UIP_dec Pos.eq_dec heq1 erefl) /= => ?; subst t1 => {heq1}.
        by move=> /(_ _ hoff _ hh) <-; rewrite Z.add_comm wrepr_add GRing.addrA -/(mp_addr mp).
      move=> [hd hdm hcv]; rewrite get_gvar_neq // => /h2 -/(_ _ hcv) /=.
      by apply: (eq_mp_val_write_disj hvs hgetr) hmem2.
    by apply: wfr_PTR_rset_word hmem2 h3.
  + by apply: (eq_glob_rset_word hvs hgetr) hmem2 heqg.
  by apply: (eq_not_mod_rset_word hvs hgetr) hmem2 hnotm.
Qed.










































 Lemma get_arr_read_mem vm mem mp (n n':positive) (t : WArray.array n) x ws i w:
   n <= n' -> eq_mp_val (sarr n') (emem s') mp (Varr t) ->
   check_gvalid rmap x = Some mp ->
   WArray.get sz t i = ok w ->
   read_mem (emem s') (mp_addr mp + wrepr U64 (wsize_size sz * i)) sz = ok w.

 
find_gvar gm sm x = Some mp ->
    valid_ptr_arr vm mem mp.(mp_s) mp.(mp_ofs) n' x ->
    is_align (wrepr U64 mp.(mp_ofs)) ws -> 
    get_gvar gd vm x = ok (Varr t) ->
    WArray.get ws t i = ok w ->
    read_mem mem (wptr mp.(mp_s) + wrepr U64 (i * wsize_size ws + mp.(mp_ofs))) ws = ok w.
  Proof.
    move=> hnn' hm hvalid halign hvget hget.
    rewrite Memory.readP /CoreMem.read.
    have [hbound1 hbound2 (* hwa_align*)] := WArray.get_bound hget.
    have hv : valid_pointer mem (wptr mp.(mp_s) + wrepr U64 (i * wsize_size ws + mp.(mp_ofs))) ws.
    + apply Memory.is_align_valid_pointer.
      + case : (wptr_add mp.(mp_s)) => ? ->; rewrite Z.mul_comm wrepr_add. 
        by apply is_align_add => //; apply is_align_mul.
      move=> k hk; have [] := hvalid  (i * wsize_size ws + k).
      + by rewrite /wsize_size; nia.
      by rewrite -Z.add_assoc (Z.add_comm k) Z.add_assoc wrepr_add GRing.addrA.
    have := validr_valid_pointer mem (wptr mp.(mp_s) + wrepr U64 (i * wsize_size ws + mp.(mp_ofs)))%R ws.
    rewrite hv; case: validr => //= _ _.
    move: (hget);rewrite /WArray.get /CoreMem.read; t_xrbindP => ? _ <-.
    do 2 f_equal; rewrite /CoreMem.uread.
    apply eq_map_ziota => k hk /=.
    have [v]:= WArray.get_get8 hget hk.
    have []/= := hvalid (i * wsize_size ws + k);first by rewrite /wsize_size; nia.
    move=> hva /(_ _ _ hvget) h /dup [] /h h1 /WArray.get_uget ->.
    move: h1; rewrite Memory.readP /CoreMem.read.
    t_xrbindP=> ??; rewrite CoreMem.uread8_get => <-; f_equal.
    by rewrite Memory.addP !wrepr_add; ssrring.ssring.
  Qed.

Search _ WArray.uincl.
Search _ value_uincl to_int.
hgvk).
                  
 /(check_varP hgvk).
t_xrbindP => e1' /he1{he1}he1 ?? vpk hgvk.
; last first.
      + move=> [<-]; apply: on_arr_gvarP => n t hsub /= /(check_varP hgvk).
        move=> [v' [-> hu]]; t_xrbindP => i ?/he1 [? []].
hget.
        t_xrbindP.
/= /eq_mp_word.
Check is_align.
      have : 
      



      + 
 = ok (addr 
        
        

      case: ifPn => hglob.
      + t_xrbindP => z /get_globalP hgg.
        case hty:is_word_type => [ ws |] // [<-].
        have ety:= is_word_typeP hty.
        rewrite get_gvar_glob // => hget.
        chave (vs_glob hgg hget); rewrite ety /= vs_rip => -[] w [->].
        by rewrite mp_addrP / wptr /= => ->; exists (Vword w).
      case heq: get_local => [pk | ]; last first.
      + move=> [<-]; rewrite /= !get_gvar_nglob //= => hx.
        by apply: (check_varP heq hx).
      case hty: is_word_type => [ws | //].
      have /is_word_typeP{hty}hty := hty; rewrite /check;t_xrbindP.
      move=> ? mp hcv /assertP hal [xi ei] hma <-;rewrite get_gvar_nglob // => hget.
      chave (wfr_val hcv hget); rewrite hty /= => -[w [? hr]]; subst v.
      chave (wfr_ptr hcv) => -[pk' []]; rewrite heq => -[<-].
      case: pk heq hma => //=.
      + move=> z heq [??] ?; subst xi ei mp; rewrite /with_var /= vs_rsp /=.
        by move: hr; rewrite mp_addrP /= => -> /=; exists (Vword w).
      move=> p heq [] ?? hp; subst xi ei; rewrite /with_var /= hp /= !zero_extend_u wrepr0. 
      by rewrite GRing.addr0 hr /=; exists (Vword w).
    + move=> sz x e he e1 v; t_xrbindP => e' /he{he}he he1.
      apply: on_arr_gvarP => n t /subtypeEl [n' [hty hnn']].
      
      Search _ subtype.
Print subtype.
move 
      case: ifPn => hglob. 
      + t_xrbindP => z hget; case hic: is_const => [ez | //]; t_xrbindP => _ /assertP hal <-.
       
      +
Search _ right_id.
(_ + 0)%R.
addr0.

auto.
rewrite /valid_mk.
have :      check_valid rmap x = ok mp ->
      mk_addr pmap x ws pk 0 = ok (xi, ei) ->
      exists w1 w2, [/\ get_var (evm s') xi >>= to_pointer = ok w1, 
                        sem_pexpr [::] s' ei >>= to_pointer = ok w2 &
                        mp_addr mp = w1 + w2].
      get_
/eq_mp_word.
Print mk_addr.

Print check.
Search iota.
Search (ord_enum _).
Search map all.
Print preim.
      Print wbit.
Search _ Z.testbit.
Search Z.shiftl .
        chave (vs_eq_vm heq).
       

 Lemma check_varP vm1 vm2 x v:
    check_var sm x -> eq_vm vm1 vm2 ->
    get_var vm1 x = ok v ->
    exists v', get_var vm2 x = ok v' /\ value_uincl v v'.
  Proof.
    move=> Hin_eqon Heq Hget.
    have := Heq _ Hin_eqon. 
    move: Hget;rewrite /get_var; apply: on_vuP => [t | ] -> //=.
    by move=> <-;case vm2.[x] => //= s Hs;exists (pto_val s).
  Qed.


_ hglob).
      case hty:is_word_type => [ ws |] //.
 [<-].
 
rewrite /get_local.      



Print mp_addr.

mp_addr pm = 
(zero_extend Uptr (wptr (mp_s mp)) + zero_extend Uptr (wrepr Uptr (mp_ofs mp))%R.
         

 /eq_mp_val /eq_mp_word.
        rewrite vs_rip.
; move: f.
//.

    
 [v'].
 vs'.
es'.
  


  Record valid (s1: estate) (s2: estate) : Prop := Valid {
    valid_disj : disjoint_ptr (emem s1);
    valid_eq   :
      forall w sz, valid_pointer (emem s1) w sz -> read_mem (emem s1) w sz = read_mem (emem s2) w sz;
    valid_def  :
      forall w sz, valid_pointer (emem s2) w sz = 
         valid_pointer (emem s1) w sz || (between rsp stk_size w sz && is_align w sz) || 
            (between rip glob_size w sz && is_align w sz);
    valid_vm   : eq_vm (evm s1) (evm s2);
    valid_rip  : get_var (evm s2) (vrip gm) = ok (Vword rip);
    valid_rsp  : get_var (evm s2) (vrsp nrsp) = ok (Vword rsp);
    valid_top_frame : ohead (frames (emem s2)) = Some (rsp, stk_size);
    valid_s    : valid_stk sm (evm s1) (evm s2) (emem s2);
    valid_m    : valid_map gm sm stk_size glob_size;
    valid_glob : forall i, 
         0 <= i < glob_size ->
         read_mem (emem s2) (rip + wrepr U64 i) U8 = ok (nth (wrepr U8 0) data (Z.to_nat i))
  }.


 Variable (P: uprog) (ev: extra_val_t (progT := progUnit)).
  Notation gd := (p_globs P).
  Variable nrsp: Ident.ident.
  Variable SP: seq (funname * sfundef).

  Variable stk_size : Z.
  Variable rsp : pointer.
  Variable glob_size : Z.
  Variable rip : pointer. 
  Variable data : seq u8.

  Hypothesis rsp_add : ptr_ok rsp stk_size.
  Hypothesis rip_add : ptr_ok rip glob_size.
































Definition valid_map (gm:pmap) sm (stk_size glob_size:Z) :=
  forall x mpx, find_gvar gm sm x = Some mpx -> 
     exists sx, size_of (vtype x.(gv)) = ok sx /\
     [/\ 0 <= mpx.(mp_ofs), mpx.(mp_ofs) + sx <= ptr_size stk_size glob_size mpx.(mp_s),
         aligned_for (vtype x.(gv)) mpx.(mp_ofs) &
         forall y mpy sy, 
           find_gvar gm sm y = Some mpy -> 
           size_of (vtype y.(gv)) = ok sy ->
           mpx.(mp_s) = mpy.(mp_s) ->
           if (mpx.(mp_ofs) == mpy.(mp_ofs)) then 
              is_word_type (vtype (gv x)) \/ is_word_type (vtype (gv y)) -> v_var (gv x) = v_var (gv y)
           else mpx.(mp_ofs) + sx <= mpy.(mp_ofs) \/ mpy.(mp_ofs) + sy <= mpx.(mp_ofs)].

Hint Resolve is_align_no_overflow Memory.valid_align.

(* TODO: MOVE *)


Lemma mk_ofsP sz gd s2 ofs e i :
  sem_pexpr gd s2 e = ok (Vint i) ->
  sem_pexpr gd s2 (mk_ofs sz e ofs) = ok (Vword (wrepr U64 (i * wsize_size sz + ofs)%Z)).
Proof.
  rewrite /mk_ofs; case is_constP => /= [? [->] //| {e} e he] /=.
  rewrite /sem_sop2 /=.
  have [sz' [w [-> /= -> /=]]]:= cast_wordP he.
  by rewrite !zero_extend_u wrepr_add wrepr_mul GRing.mulrC. 
Qed.

Lemma subtypeEl ty ty': subtype ty ty' → 
  match ty with
  | sbool => ty' = sbool
  | sint => ty' = sint
  | sarr n => ∃ n' : positive, ty' = sarr n' ∧ n <= n'
  | sword sz => ∃ sz' : wsize, ty' = sword sz' ∧ (sz ≤ sz')%CMP
  end.
Proof.
  (case: ty => /= [/eqP <-|/eqP <-| p | sz ] //;
   case: ty') => // [p' /ZleP ? | sz' ?]; eauto.
Qed.
  
Lemma validr_valid_pointer (m1:mem) p ws : is_ok (validr m1 p ws) = valid_pointer m1 p ws.
Proof.
  case: (Memory.readV m1 p ws); rewrite Memory.readP /CoreMem.read.
  + by move=> [w]; case: validr.
  by case: validr => //= _ [];eauto.
Qed.

Section PROOF.

  Variable (P: uprog) (ev: extra_val_t (progT := progUnit)).
  Notation gd := (p_globs P).
  Variable nrsp: Ident.ident.
  Variable SP: seq (funname * sfundef).

  Variable stk_size : Z.
  Variable rsp : pointer.
  Variable glob_size : Z.
  Variable rip : pointer. 
  Variable data : seq u8.

  Hypothesis rsp_add : ptr_ok rsp stk_size.
  Hypothesis rip_add : ptr_ok rip glob_size.

  Variable gm:gmap.

  Definition wptr mp := if mp == MSstack then rsp else rip.

  Lemma wptr_add x : ptr_ok (wptr x) (ptr_size stk_size glob_size x).
  Proof. by rewrite /wptr /ptr_size; case: ifP. Qed.

  Definition valid_ptr_word (vm1:vmap) (m2:mem) ms (p: Z) sz x :=
    valid_pointer m2 (wptr ms + wrepr _ p) sz /\
    forall v, get_gvar gd vm1 x = ok v ->
    exists2 w,
      read_mem m2 (wptr ms + wrepr _ p) sz = ok w &
      v = Vword w.

  Definition valid_ptr_arr (vm1:vmap) (m2:mem) ms (p: Z) s x :=
    forall off, (0 <= off < Zpos s)%Z ->
      valid_pointer m2 (wptr ms + (wrepr _ (off + p))) U8 /\
      forall s' (a:WArray.array s'),
        get_gvar gd vm1 x = ok (Varr a) ->
        forall v, WArray.get U8 a off = ok v ->
          read_mem m2 (wptr ms + (wrepr _ (off + p))) U8 = ok v.

  Definition valid_stk mstk (vm1:vmap) (vm2:vmap) (m2:mem) :=
    forall x,
      match find_gvar gm mstk x with
      | Some mp =>
        (if mp.(mp_id) is Some id then
           get_var vm2 {| vname := id; vtype := sword Uptr |} = ok (Vword (wptr mp.(mp_s) + wrepr _ mp.(mp_ofs)))%R
         else True) /\
        match vtype x.(gv) with
        | sword sz => valid_ptr_word vm1 m2 mp.(mp_s) mp.(mp_ofs) sz x
        | sarr s => valid_ptr_arr vm1 m2 mp.(mp_s) mp.(mp_ofs) s x
        | _ => True
        end
      | _ => True
      end.

Section EXPR.

  Variable sm:smap.

  Definition eq_vm (vm1 vm2:vmap) := 
    forall (x:var), Sv.mem x sm.(meqon) -> eval_uincl vm1.[x] vm2.[x].

  Lemma eq_vm_write vm1 vm2 x v v':
    eq_vm vm1 vm2 -> eval_uincl v v' -> eq_vm vm1.[x <- v] vm2.[x <- v'].
  Proof.
    move=> Heqvm Hu w ?.
    case : (x =P w) => [<- | /eqP ?];first by rewrite !Fv.setP_eq.
    by rewrite !Fv.setP_neq //; apply Heqvm.
  Qed.
 
  Definition disjoint_ptr m :=
    forall w sz,
      valid_pointer m w sz ->
      ~((wunsigned rsp <? wunsigned w + wsize_size sz) && (wunsigned w <? wunsigned rsp + stk_size)) /\
      ~((wunsigned rip <? wunsigned w + wsize_size sz) && (wunsigned w <? wunsigned rip + glob_size)).

  Record valid (s1: estate) (s2: estate) : Prop := Valid {
    valid_disj : disjoint_ptr (emem s1);
    valid_eq   :
      forall w sz, valid_pointer (emem s1) w sz -> read_mem (emem s1) w sz = read_mem (emem s2) w sz;
    valid_def  :
      forall w sz, valid_pointer (emem s2) w sz = 
         valid_pointer (emem s1) w sz || (between rsp stk_size w sz && is_align w sz) || 
            (between rip glob_size w sz && is_align w sz);
    valid_vm   : eq_vm (evm s1) (evm s2);
    valid_rip  : get_var (evm s2) (vrip gm) = ok (Vword rip);
    valid_rsp  : get_var (evm s2) (vrsp nrsp) = ok (Vword rsp);
    valid_top_frame : ohead (frames (emem s2)) = Some (rsp, stk_size);
    valid_s    : valid_stk sm (evm s1) (evm s2) (emem s2);
    valid_m    : valid_map gm sm stk_size glob_size;
    valid_glob : forall i, 
         0 <= i < glob_size ->
         read_mem (emem s2) (rip + wrepr U64 i) U8 = ok (nth (wrepr U8 0) data (Z.to_nat i))
  }.

  Lemma check_varP vm1 vm2 x v:
    check_var sm x -> eq_vm vm1 vm2 ->
    get_var vm1 x = ok v ->
    exists v', get_var vm2 x = ok v' /\ value_uincl v v'.
  Proof.
    move=> Hin_eqon Heq Hget.
    have := Heq _ Hin_eqon. 
    move: Hget;rewrite /get_var; apply: on_vuP => [t | ] -> //=.
    by move=> <-;case vm2.[x] => //= s Hs;exists (pto_val s).
  Qed.

  Lemma check_varsP vm1 vm2 xs vs:
    all (check_var sm) xs -> eq_vm vm1 vm2 ->
    mapM (fun x : var_i => get_var vm1 x) xs = ok vs ->
    exists vs',
      mapM (fun x : var_i => get_var vm2 x) xs = ok vs' /\
      List.Forall2 value_uincl vs vs'.
  Proof.
    elim: xs vs=> [|a l IH] //= vs.
    + move=> _ Heq [<-];by exists [::].
    move=> /andP [Ha Hl] Heq.
    apply: rbindP => va /(check_varP Ha Heq) [v' [-> Hu]].
    apply: rbindP => tl  /(IH _ Hl Heq) [tl' [-> Hus]] [<-] /=.
    by exists (v' :: tl');split=>//;constructor.
  Qed.

  Lemma get_arr_read_mem vm mem mp (n n':positive) (t : WArray.array n) x ws i w:
    n <= n' -> find_gvar gm sm x = Some mp ->
    valid_ptr_arr vm mem mp.(mp_s) mp.(mp_ofs) n' x ->
    is_align (wrepr U64 mp.(mp_ofs)) ws -> 
    get_gvar gd vm x = ok (Varr t) ->
    WArray.get ws t i = ok w ->
    read_mem mem (wptr mp.(mp_s) + wrepr U64 (i * wsize_size ws + mp.(mp_ofs))) ws = ok w.
  Proof.
    move=> hnn' hm hvalid halign hvget hget.
    rewrite Memory.readP /CoreMem.read.
    have [hbound1 hbound2 (* hwa_align*)] := WArray.get_bound hget.
    have hv : valid_pointer mem (wptr mp.(mp_s) + wrepr U64 (i * wsize_size ws + mp.(mp_ofs))) ws.
    + apply Memory.is_align_valid_pointer.
      + case : (wptr_add mp.(mp_s)) => ? ->; rewrite Z.mul_comm wrepr_add. 
        by apply is_align_add => //; apply is_align_mul.
      move=> k hk; have [] := hvalid  (i * wsize_size ws + k).
      + by rewrite /wsize_size; nia.
      by rewrite -Z.add_assoc (Z.add_comm k) Z.add_assoc wrepr_add GRing.addrA.
    have := validr_valid_pointer mem (wptr mp.(mp_s) + wrepr U64 (i * wsize_size ws + mp.(mp_ofs)))%R ws.
    rewrite hv; case: validr => //= _ _.
    move: (hget);rewrite /WArray.get /CoreMem.read; t_xrbindP => ? _ <-.
    do 2 f_equal; rewrite /CoreMem.uread.
    apply eq_map_ziota => k hk /=.
    have [v]:= WArray.get_get8 hget hk.
    have []/= := hvalid (i * wsize_size ws + k);first by rewrite /wsize_size; nia.
    move=> hva /(_ _ _ hvget) h /dup [] /h h1 /WArray.get_uget ->.
    move: h1; rewrite Memory.readP /CoreMem.read.
    t_xrbindP=> ??; rewrite CoreMem.uread8_get => <-; f_equal.
    by rewrite Memory.addP !wrepr_add; ssrring.ssring.
  Qed.

  Lemma check_vfreshP x tt : 
    check_vfresh sm x = ok tt -> is_lvar x /\ Sv.mem (gv x) sm.(meqon).
  Proof.
    rewrite /check_vfresh; case: ifPn => //;case: ifPn => //=.
    by rewrite /is_glob /is_lvar; case: gs.
  Qed.

  Section CHECK_E_ESP.
    Context s s' (Hv: valid s s').

    Let X e : Prop :=
      ∀ e' v,
        alloc_e nrsp gm sm e = ok e' →
        sem_pexpr gd s e = ok v →
        ∃ v', sem_pexpr [::] s' e' = ok v' ∧ value_uincl v v'.

    Let Y es : Prop :=
      ∀ es' vs,
        mapM (alloc_e nrsp gm sm) es = ok es' →
        sem_pexprs gd s es = ok vs →
        ∃ vs', sem_pexprs [::] s' es' = ok vs' ∧ List.Forall2 value_uincl vs vs'.

    Lemma check_vfresh_get x v tt : eq_vm (evm s) (evm s') ->
      check_vfresh sm x = ok tt → 
      find_gvar gm sm x = None ->
      get_gvar gd (evm s) x = ok v → 
      ∃ v' : value, get_gvar [::] (evm s') x = ok v' ∧ value_uincl v v'.
    Proof.
      rewrite /find_gvar /get_gvar => hvm /check_vfreshP [-> eqon].
      by case heq: Mvar.get => [ [] | ] //= _; apply: check_varP.
    Qed.

    Lemma get_vptr ms : get_var (evm s') (vptr nrsp gm ms) = ok (Vword (wptr ms)).
    Proof.
      have hrsp := valid_rsp Hv; have hrip := valid_rip Hv.
      by rewrite /vptr /wptr ;case: ms => /=.
    Qed.

    Lemma check_e_esP : (∀ e, X e) * (∀ es, Y es).
    Proof.
      have Hvm: eq_vm (evm s) (evm s') by move: Hv => -[].
      apply: pexprs_ind_pair; subst X Y; split => /=.

      - by move=> ?? [<-] [<-];exists [::].

      - move=> e he es hes ??; t_xrbindP => e' /he{he}he es' /hes{hes}hes <-.
        move=> ? /he [v' /= [->]] /= vu ? /hes [vs1' [->]] uvs <- /=.
        by exists (v'::vs1');split => //;constructor.

      + by move=> z1 e2 v [<-] [<-]; exists z1.
      + by move=> b1 e2 v [<-] [<-]; exists b1.
      + by move=> n1 e2 v [<-] [<-]; eexists; split; first reflexivity.

      + move=> x1 e2 v.
        case heq : find_gvar => [ mp | ]; last first.
        + by t_xrbindP => ? h1 <-; apply: check_vfresh_get h1 heq.
        case heq1 : mp_id => //.
        case hw : is_word_type => [ws | //].
        have hty := is_word_typeP hw => {hw} -[<-] /=.
        rewrite get_vptr /= !zero_extend_u.
        by have := valid_s Hv x1; rewrite heq heq1 hty => -[] _ [h1 h2] /h2 [w] -> -> /=; exists (Vword w).

      + move=> sz x1 e1 IH e2 v; t_xrbindP => e1' /IH hrec.
        case heq: find_gvar => [mp | ];last first.
        + t_xrbindP => ? h1 <- /=; apply: on_arr_varP => n t Ht Harr.
          have [v' []]:= check_vfresh_get Hvm h1 heq Harr.
          rewrite /get_gvar /mk_lvar /= /on_arr_var => ->.
          case: v' => //= n' a hincl.
          t_xrbindP => i ve /hrec [ve' [-> hve]] /(value_uincl_int hve) [??];subst ve ve'=> /= w hw <-.
          by have -> /= := WArray.uincl_get hincl hw; exists (Vword w).
        case:ifP=> //= halign [<-].
        apply: on_arr_varP => n t hsubt hget.
        t_xrbindP => i ve /hrec [ve' [hve' sve']] /(value_uincl_int sve') [??].
        move=> w hti ?; subst v ve ve' => /=.
        have [n' [/= heqt hnn']]:= subtypeEl hsubt.
        have := valid_s Hv (mk_lvar x1); rewrite heq heqt; case: mp_id => [id | ] [hgeti hva].
        + rewrite hgeti /= (mk_ofsP sz _ hve') /= !zero_extend_u.
          have := get_arr_read_mem hnn' heq hva halign hget hti.
          have -> : (wptr (mp_s mp) + wrepr U64 (mp_ofs mp) + wrepr U64 (i * wsize_size sz + 0))%R = 
                    (wptr (mp_s mp) + wrepr U64 (i * wsize_size sz + mp_ofs mp))%R.
          + by rewrite !wrepr_add wrepr0; ssrring.ssring.
          by move=> -> /=; exists (Vword w).
        rewrite get_vptr /= (mk_ofsP sz mp.(mp_ofs) hve') /= !zero_extend_u.
        by rewrite (get_arr_read_mem hnn' heq hva halign hget hti);exists (Vword w).

      + move=> sz1 v1 e1 IH e2 v.
        case:ifP => // hc; t_xrbindP => e1' /IH hrec <- wv1 vv1 /= hget hto' we1 ve1.
        move=> /hrec [ve1' [->] hu] /= hto wr hr ?;subst v.
        have [vv1' [-> hu'] /=]:= check_varP hc Hvm hget.
        rewrite (value_uincl_word hu hto) (value_uincl_word hu' hto') /=.
        have hv:= read_mem_valid_pointer hr.
        case: Hv => _ /(_ _ _ hv) <- _ _ _ _ _.
        by rewrite hr /=;eexists;(split;first by reflexivity) => /=.

      + move=> o1 e1 IH e2 v.
        t_xrbindP => e1' /IH hrec <- ve1 /hrec [ve1' /= [-> hu] /=] /(vuincl_sem_sop1 hu) ->.
        by eexists;split;first by reflexivity.

      + move=> o1 e1 H1 e1' H1' e2 v.
        t_xrbindP => e1_ /H1 hrec e1'_ /H1' hrec' <- ve1 /hrec.
        move=> [ve1' /= [-> hu] /=] ve2 /hrec' [ve2' /= [-> hu'] /=] .
        move=> /(vuincl_sem_sop2 hu hu') ->.
        by eexists;split;first by reflexivity.

      + move => e1 es1 H1 e2 v.
        t_xrbindP => es1' /H1{H1}H1 <- vs /H1{H1} /= [vs' []].
        rewrite /sem_pexprs => -> /= h1 h2.
        by have [v' ??]:= (vuincl_sem_opN h2 h1);exists v'.

      move=> t e He e1 H1 e1' H1' e2 v.
      t_xrbindP => e_ /He he e1_ /H1 hrec e1'_ /H1' hrec' <-.
      move=> b vb /he [vb' /= [-> ub]] /(value_uincl_bool ub) [??];subst vb vb'.
      move=> vte1 ve1 /hrec [ve1' /= [-> hu] /=] ht1 vte2 ve2 /hrec' [ve2' /= [-> hu'] /=] ht2 <-.
      have [? -> ?] := truncate_value_uincl hu ht1.
      have [? -> ? /=] := truncate_value_uincl hu' ht2.
      eexists;split;first by reflexivity.
      by case: (b).
    Qed.

  End CHECK_E_ESP.

  Definition alloc_eP e e' s s' v he hv :=
    (@check_e_esP s s' hv).1 e e' v he.

  Definition alloc_esP es es' s s' vs hs hv :=
    (@check_e_esP s s' hv).2 es es' vs hs.

End EXPR.

(* TODO: move *)
  Lemma get_gvar_neq x vi vm (v: exec (psem_t (vtype vi))): 
    (is_lvar x → vi != gv x) ->
    get_gvar gd vm.[vi <- v] x = get_gvar gd vm x.
  Proof.
    by rewrite /get_gvar;case: ifP => ? // /(_ erefl) hx; rewrite /get_var Fv.setP_neq.
  Qed.

  Lemma get_var_neq vm x v y :
    x <> y ->
    get_var vm.[x <- v] y = get_var vm y.
  Proof. move=> /eqP hne;rewrite /get_var /on_vu Fv.setP_neq //. Qed.

  Lemma find_gvar_rm mstk vi x : 
    find_gvar gm (Mvar.remove mstk vi) x = 
    if is_lvar x && (vi == gv x) then None 
    else find_gvar gm mstk x.
  Proof.
    rewrite /find_gvar; case:ifP => //.
    by rewrite Mvar.removeP; case: eqP.
  Qed.

  Lemma Mvar_filterP (A:Type) (f: var -> A -> bool) (m:Mvar.t A) x a: 
    Mvar.get (Mvar_filter f m) x = Some a <->
    Mvar.get m x = Some a /\ f x a.
  Proof.
    rewrite /Mvar_filter; rewrite Mvar.foldP.
    set F := (λ (a0 : Mvar.t A) (kv : Mvar.K.t * A), if f kv.1 kv.2 then Mvar.set a0 kv.1 kv.2 else a0).
    have h : forall els m1,
              uniq [seq x.1 | x <- els] ->
              (forall e, List.In e els -> Mvar.get m1 e.1 = None) -> 
              Mvar.get (foldl F m1 els) x = Some a <->
              (Mvar.get m1 x = Some a \/ (List.In (x,a) els /\ f x a)).
    + elim => [ | [y ay] els ih] m1 /=; first by intuition.
      move=> /andP [] hy hu he; rewrite ih //; last first. 
      + move=> e h; rewrite /F /=; case: ifP => ?; last by auto.
        rewrite Mvar.setP_neq; first by auto.
        apply /eqP => ?;subst y.
        move /xseq.InP: hy => hy; apply hy.
        by apply: (List.in_map fst). 
      rewrite {1}/F; case: ifPn => /= [ | /negP] ?. 
      + rewrite Mvar.setP; case: eqP => ?.
        + subst; split.
          + by move=> [] [?]; first subst ay; auto.
          move=> []; first by rewrite (he (x,ay)); auto.
          by move=> [] [[??] | ]; subst; intuition.
        split; first by intuition. 
        move=> []; first by auto.
        by move=> [] [[??] |]; subst; intuition. 
      split; first by intuition. 
      move=> [];first by auto.
      move=> [[[??] | ]]; subst; intuition.
    rewrite h.
    + rewrite Mvar.get0 Mvar.elementsIn /=.
      split; last by auto.
      by move=> []; auto.
    + by apply Mvar.elementsU.
    by move=> e;rewrite Mvar.get0.
  Qed.

  Lemma find_gvar_keep mstk vn x mp: 
    find_gvar gm (Mvar_filter (keep_addrvar vn) mstk) x = Some mp ->
    find_gvar gm mstk x = Some mp /\ mp_id mp <> Some vn.
  Proof.
    rewrite /find_gvar; case: is_lvar.
    + have := Mvar_filterP (keep_addrvar vn) mstk (gv x).
      case: Mvar.get => mp' // /(_ mp') [/(_ erefl) [-> h2] _].
      move: h2; rewrite /keep_addrvar; case: mp' => //.
      + by move=> ?? [<-].
      by move=> ??? /eqP h [?];subst mp; split => //= => -[?]; subst vn.
    by case: Mvar.get => // ? [<-].
  Qed.

  Lemma find_gvar_map_remove sm vi x mp : 
    find_gvar gm (map_remove sm vi) x = Some mp ->
    [/\ (is_lvar x -> vi <> gv x),
        find_gvar gm sm x = Some mp &
        (vtype vi = sword64 -> mp_id mp <> Some (vname vi))].
  Proof.
    rewrite /map_remove /= find_gvar_rm. 
    case: andP => // h1.
    have hh : is_lvar x → vi ≠ gv x by move=> his h;apply h1;split => //; rewrite h.
    case heq: is_word_type => [ws | ].
    + case: eqP => heq1.
      + by move=> /find_gvar_keep [h2 h3].
      by move=> h;split => // ht; move: heq heq1; rewrite ht => -[->].
    by move=> h;split => // ht; rewrite ht in heq.
  Qed.

  Lemma valid_stk_write_remove (sm:smap) s1 s2 vi v v':
    valid_stk sm (evm s1) (evm s2) (emem s2) ->
    valid_stk (map_remove sm vi) (evm s1).[vi <- v] (evm s2).[vi <- v'] (emem s2).
  Proof.
    move=> Hstk x.
    case heq : find_gvar => [mp | //].
    have := Hstk x; have [h1 -> h2]:= find_gvar_map_remove heq. 
    move=> [hid hty]; split.
    + case: mp_id h2 hid => [id | //] hneid.
      rewrite get_var_neq // => ?; subst vi.
      by have := hneid erefl.
    case: vtype hty => // [p | ws].
    + by move=> hv off /hv; rewrite get_gvar_neq // => ?; apply /eqP => heq1; apply h1.
    by move=> [hvp hg];split => //; rewrite get_gvar_neq // => ?; apply /eqP => heq1; apply h1.
  Qed.

  Lemma eq_vm_write_rm sm vm1 vm2 x v v':
    eq_vm sm vm1 vm2 ->
    eval_uincl v v' ->
    eq_vm (map_remove sm x) vm1.[x <- v] vm2.[x <- v'].
  Proof.
    move=> Heqvm Hu w hin. 
    case : (x =P w) => [<- | /eqP hne];first by rewrite !Fv.setP_eq.
    move: hin; rewrite /map_remove /= => /Sv_memP hin.
    rewrite !Fv.setP_neq //; apply Heqvm; apply /Sv_memP.
    by move/eqP: hne; SvD.fsetdec.
  Qed.

  Lemma valid_map_rm (sm:smap) vi: 
    valid_map gm sm stk_size glob_size ->
    valid_map gm (map_remove sm vi) stk_size glob_size.
  Proof.
    move=> hv x mpx /find_gvar_map_remove [hd /hv [sx [hs [h1 h2 h3 h4]]] hid].
    exists sx; split => //; split => // y mpy ofs /find_gvar_map_remove [hd' hf hid'].
    by apply: h4.
  Qed.

  Lemma valid_set_uincl sm s1 s2 vi v v': 
    vi != vrsp nrsp -> vi != vrip gm -> 
    valid sm s1 s2 -> eval_uincl v v' ->
    valid (map_remove sm vi) (with_vm s1 (evm s1).[vi <- v]) (with_vm s2 (evm s2).[vi <- v']).
  Proof.
    move=> neq1 neq2 [?????????] Hu;split=> //=.
    + by apply: eq_vm_write_rm.
    + by rewrite /get_var Fv.setP_neq ?Hx //.
    + by rewrite /get_var Fv.setP_neq ?Hx //.
    + by apply: valid_stk_write_remove.
    by apply valid_map_rm.
  Qed.

  Lemma valid_write sm (vi : var_i) s1 s2 v v':
    ~~ is_vrsp nrsp gm vi -> 
    ~~ is_vrip gm vi -> 
    valid sm s1 s2 -> value_uincl v v' -> 
    forall s1', write_var vi v s1 = ok s1' ->
    exists s2', write_var vi v' s2 = ok s2' /\ valid (map_remove sm vi) s1' s2'.
  Proof.
    move=> Hnvrsp Hnvrip Hval Hu s1'. 
    rewrite /write_var; apply: rbindP=> z /=; apply: set_varP;rewrite /set_var.
    + move=> t /(pof_val_uincl Hu) [t' [-> Htt']] <- [<-] /=.
      eexists;split;first reflexivity.
      by apply valid_set_uincl.
    case: vi Hnvrsp Hnvrip => -[tvi nvi] vii /= Hnvrsp Hnvrip.
    move=> /is_sboolP ?; subst tvi => /= /to_bool_undef ?; subst v => <- [<-].
    by have [-> | [b1 ->]] /=:= pof_val_undef Hu erefl;
     (eexists;split;first reflexivity); apply valid_set_uincl.
  Qed.
  
  Lemma wunsigned_rsp_add ofs :
    0 <= ofs -> ofs < stk_size ->
    wunsigned (rsp + wrepr Uptr ofs) = wunsigned rsp + ofs.
  Proof.
    move => p1 p2; apply: wunsigned_add.
    case: rsp_add => h _; have := wunsigned_range rsp; lia.
  Qed.

  Lemma wunsigned_rip_add ofs :
    0 <= ofs -> ofs < glob_size ->
    wunsigned (rip + wrepr Uptr ofs) = wunsigned rip + ofs.
  Proof.
    move => p1 p2; apply: wunsigned_add.
    case: rip_add => h _; have := wunsigned_range rip; lia.
  Qed.

  Lemma wunsigned_wptr_add ofs x :
    0 <= ofs -> ofs < ptr_size stk_size glob_size x ->
    wunsigned (wptr x + wrepr Uptr ofs) = wunsigned (wptr x) + ofs.
  Proof.
    rewrite /wptr /ptr_size; case: eqP; auto using wunsigned_rsp_add, wunsigned_rip_add.
  Qed.

  Lemma lt_of_add_le x y sz :
    x + wsize_size sz <= y -> x < y.
  Proof. have := wsize_size_pos sz; lia. Qed.

  Lemma get_find_gvar sm x ap : 
    Mvar.get sm x = Some ap ->
    find_gvar gm sm (mk_lvar ({|v_var := x; v_info := xH|})) = Some (mp_of_ap ap).
  Proof. by rewrite /find_gvar /= => ->; case: ap. Qed.

  Lemma find_between sm x mp :
    valid_map gm sm stk_size glob_size ->
    find_gvar gm sm x = Some mp ->
    match vtype x.(gv) with
    | sword ws => between (wptr mp.(mp_s)) (ptr_size stk_size glob_size mp.(mp_s)) 
                                            (wptr mp.(mp_s) + wrepr Uptr mp.(mp_ofs)) ws &&
                  is_align (wptr mp.(mp_s) + wrepr Uptr mp.(mp_ofs)) ws
                  
    | sarr n   => forall i, 0 <= i < n -> 
                  between (wptr mp.(mp_s)) (ptr_size stk_size glob_size mp.(mp_s)) 
                                           (wptr mp.(mp_s) + wrepr Uptr (i + mp.(mp_ofs))) U8 &&
                  is_align (wptr mp.(mp_s) + wrepr Uptr (i + mp.(mp_ofs))) U8
    | _        => True
    end. 
  Proof.
    move=> validm hfind.
    have [sx]:= validm _ _ hfind.
    have [hstk halign] := wptr_add mp.(mp_s).
    case: vtype => //= [p | ws] [] [<-] [] h1 h2 h3 h4 => [i hi| ];
    [have ? : wsize_size U8 = 1 by done| have ? := wsize_size_pos ws].
    + apply/andP;split.
      + by apply /andP;rewrite wunsigned_wptr_add;first (split;apply /ZleP); lia.
      by rewrite halign;apply is_align8.
    apply/andP;split.
    + by apply /andP;rewrite wunsigned_wptr_add;first (split;apply /ZleP); lia.
    by rewrite halign.
  Qed.

  Lemma valid_get_w sm sz vn ofs :
    valid_map gm sm stk_size glob_size ->
    Mvar.get sm {| vtype := sword sz; vname := vn |} = Some (APmem ofs) ->
    between rsp stk_size (rsp + wrepr Uptr ofs) sz && is_align (rsp + wrepr Uptr ofs) sz.
  Proof. by move=> hv /get_find_gvar -/find_between /= /(_ hv). Qed. 

  Hypothesis disj_rsp_rip :
    0 < stk_size -> 0 < glob_size ->
    ((wunsigned rsp + stk_size <=? wunsigned rip) || (wunsigned rip + glob_size <=? wunsigned rsp)).

  Lemma between_rsp_rip_disj p1 p2 sz1 sz2 :
    between rsp stk_size p1 sz1 && is_align p1 sz1 ->
    between rip glob_size p2 sz2 && is_align p2 sz2 -> 
    disjoint_range p1 sz1 p2 sz2.
  Proof.
    rewrite /between /disjoint_range /disjoint_zrange => 
      /andP[]/andP[] /ZleP? /ZleP? ha1 /andP[]/andP[] /ZleP? /ZleP? ha2; split; eauto.
    have h1:  0 < stk_size.
    + have := wsize_size_pos sz1; lia.
    have h2: 0 < glob_size.
    + have := wsize_size_pos sz2; lia.
    move: (disj_rsp_rip h1 h2) => /orP [/ZleP ?| /ZleP ?]; lia.
  Qed.

  Lemma valid_stk_arr_var_stk mstk s1 s2 sz xwn ofsw y mp n w m':
    let xw := {| vname := xwn; vtype := sword sz |} in
    valid_map gm mstk stk_size glob_size ->
    Mvar.get mstk xw = Some (APmem ofsw) ->
    find_gvar gm mstk y = Some mp ->  
    vtype y.(gv) = sarr n -> 
    write_mem (emem s2) (rsp + wrepr _ ofsw) sz w = ok m' ->
    valid_ptr_arr (evm s1) (emem s2) (mp_s mp) (mp_ofs mp) n y ->
    valid_ptr_arr (evm s1).[xw <- ok (pword_of_word w)] m' (mp_s mp) (mp_ofs mp) n y.
  Proof.
    move=> xw Hval Hgetw hfind hty Hm' H off Hoff.
    have Hvmem : valid_pointer (emem s2) (rsp + wrepr _ ofsw) sz by apply /Memory.writeV;eauto.
    move: H=> /(_ off Hoff) [Hoff1 Hoff2]; split.
    - by rewrite (Memory.write_valid _ _ Hm').
    have hxwa : xw != y.(gv) by rewrite vtype_diff // hty.
    rewrite get_gvar_neq // => t a Ht v0 Hv0.
    rewrite -(Hoff2 _ _ Ht _ Hv0).
    apply: (Memory.writeP_neq Hm').
    case heqs : mp_s.
    + have := find_between Hval hfind.
      rewrite hty /ptr_size /wptr heqs /= => /(_ _ Hoff).
      by apply: between_rsp_rip_disj (valid_get_w Hval Hgetw).
    have /andP [h1 h2]:= valid_get_w Hval Hgetw. 
    case: (Hval _ _ hfind) => sa [].
    rewrite /ptr_size /wptr heqs hty /= => -[]hsa [ha ha' haal] _; subst sa.
    case: (Hval _ _ (get_find_gvar Hgetw)) => sw /= [] [<-] [hw hw' ?] /(_ y _ n hfind). 
    rewrite hty heqs => /(_ erefl erefl); case: ifP => heq.
    + by move=> /(_ (or_introl erefl)) h;move: hxwa; rewrite h eq_refl. 
    move=> ?; split.
    + by apply is_align_no_overflow.
    + by apply is_align_no_overflow; apply is_align8.
    have : wunsigned (rsp + wrepr _ ofsw) = wunsigned rsp + ofsw.
    + by apply: (wunsigned_wptr_add hw (lt_of_add_le hw')).
    rewrite wsize8.
    have : wunsigned (rsp + wrepr _ (off + mp_ofs mp)) = wunsigned rsp + off + mp_ofs mp.
    - rewrite wunsigned_rsp_add;lia.
    have hsz := wsize_size_pos sz.
    nia.
  Qed.

  Lemma valid_stk_word_var_stk mstk s1 s2 sz xn sz' y ofsx mp m' w:
    let x := {| vtype := sword sz; vname := xn |} in
    valid_map gm mstk stk_size glob_size ->
    Mvar.get mstk x = Some (APmem ofsx) ->
    find_gvar gm mstk y = Some mp ->  
    vtype y.(gv) = sword sz' -> 
    write_mem (emem s2) (rsp + wrepr _ ofsx) sz w = ok m' ->
    valid_ptr_word (evm s1) (emem s2) (mp_s mp) (mp_ofs mp) sz' y ->
    valid_ptr_word (evm s1).[x <- ok (pword_of_word w)] m' (mp_s mp) (mp_ofs mp) sz' y.
  Proof.
    move=> xw Hval Hget hfind hty Hm' [H H'].
    have Hvmem : valid_pointer (emem s2) (rsp + wrepr _ ofsx) sz by apply /Memory.writeV;eauto.
    split; first by rewrite (Memory.write_valid _ _ Hm').
    move=> v.
    case his: (is_lvar y);first last.
    + rewrite get_gvar_neq ?his // => /H' [w' h1 h2];exists w' => //.
      rewrite -h1; apply: (Memory.writeP_neq Hm').
      have := find_between Hval hfind.
      rewrite hty;  move: (hfind); rewrite /find_gvar his. 
      case: Mvar.get => [ofs' [?]| //];subst mp => /=.
      by apply: between_rsp_rip_disj; apply (valid_get_w Hval Hget).
    case: (xw =P y.(gv)) => [ | /eqP] heq.
    + move: hfind hty; rewrite /find_gvar /get_gvar /wptr his /get_var -heq /= /on_vu 
        Fv.setP_eq Hget => -[?] [?] [?] /=;subst; exists w => //.
      exact: (Memory.writeP_eq Hm').
    rewrite get_gvar_neq // => /H' [w' h1 h2].
    have[sx [] [<-] {sx} [hw hw' hxal]] := Hval _ _ (get_find_gvar Hget).
    move=> /(_ _ _ _ hfind); rewrite hty /=.
    case: mp H H' h1 hfind => /= -[] /= mp_ofs mp_id H H' h1 hfind.
    + move=> _; exists w' => //.
      rewrite -h1; apply: (Memory.writeP_neq Hm').
      have := find_between Hval hfind.
      by rewrite hty /=;apply: between_rsp_rip_disj; apply (valid_get_w Hval Hget).
    move=> /(_ _ erefl erefl); case: ifP => hne.
    + by move=> /(_ (or_introl erefl)) hxw; move: heq; rewrite hxw eq_refl.
    move=> h; exists w' => //. 
    rewrite (Memory.writeP_neq Hm') => //=. 
    case: (Hval _ _ hfind) => sa [] /=.
      rewrite hty /ptr_size /wptr /= => -[<-] {sa} [ha ha' haal] _.
    split => //.
    + by apply is_align_no_overflow; apply: Memory.valid_align Hvmem.
    + by apply is_align_no_overflow; apply: Memory.valid_align (read_mem_valid_pointer h1).
    have : wunsigned (rsp + wrepr _ ofsx) = wunsigned rsp + ofsx.
    - by apply: (wunsigned_rsp_add hw (lt_of_add_le hw')).
    have haha : wunsigned (rsp + wrepr _ mp_ofs) = wunsigned rsp + mp_ofs.
    - by apply: (wunsigned_rsp_add ha (lt_of_add_le ha')).
    lia.
  Qed. 

  Lemma valid_stk_var_stk sm s1 s2 sz (w: word sz) m' xn ofs ii:
    let vi := {| v_var := {| vtype := sword sz; vname := xn |}; v_info := ii |} in
    valid_map gm sm stk_size glob_size ->
    Mvar.get sm vi = Some (APmem ofs) ->
    write_mem (emem s2) (rsp + wrepr _ ofs) sz w = ok m' ->
    valid_stk sm (evm s1) (evm s2) (emem s2) ->
    valid_stk sm (evm s1).[{| vtype := sword sz; vname := xn |} <- ok (pword_of_word w)] (evm s2) m'.
  Proof.
    move=> vi hvm Hget Hm' H x; move: H=> /(_ x).
    have Hvmem : valid_pointer (emem s2) (rsp + wrepr _ ofs) sz by apply /Memory.writeV;eauto.
    case hfind : find_gvar => [mp|//] [h1 h2]; split => //.
    case hty: vtype h2 => //.
    + exact: (valid_stk_arr_var_stk hvm Hget hfind).  
    exact: (valid_stk_word_var_stk hvm Hget hfind).
  Qed.

  Lemma valid_var_stk sm s1 xn sz w s2 m' ofs ii:
    valid sm s1 s2 ->
    write_mem (emem s2) (rsp + wrepr _ ofs) sz w = ok m' ->
    let vi := {| v_var := {| vtype := sword sz; vname := xn |}; v_info := ii |} in
    Mvar.get sm.(mstk) vi = Some (APmem ofs) ->
    valid {| mstk := sm.(mstk); meqon := Sv.remove vi sm.(meqon) |}
      (with_vm s1 (evm s1).[{| vtype := sword sz; vname := xn |} <- ok (pword_of_word w)])
      (with_mem s2 m').
  Proof.
    move=> [] hd hvr hv heqvm hrip hrsp htopf hvs hvm hg hm' vi Hget.
    have Hmem : valid_pointer (emem s2) (rsp + wrepr _ ofs) sz.
    + by apply/Memory.writeV; eauto.
    split=> //=.
    + move=> w' sz' Hvalid.
      have := hvm _ _ (get_find_gvar Hget).
      rewrite /ptr_size /mp_of_ap /= => -[sx [[hsx] [ho1 ho2 hal hdisj]]]; subst sx.
      have [hov hal'] := rsp_add.
      rewrite (hvr _ _ Hvalid); symmetry; apply: (Memory.writeP_neq hm').
      split; eauto.
      have : wunsigned (rsp + wrepr _ ofs) = wunsigned rsp + ofs.
      - by apply: (wunsigned_rsp_add ho1 (lt_of_add_le ho2)).
      have []:= hd _ _ Hvalid.
      case/negP/nandP => /ZltP /Z.nlt_ge h h'; lia.
    + by move=> w' sz'; rewrite -hv -(Memory.write_valid _ _ hm').
    + move=> x /= /Sv_memP hin; rewrite Fv.setP_neq; last first.
      + by apply/negP=> /eqP ?; subst x; SvD.fsetdec.
      by apply heqvm; apply /Sv_memP; SvD.fsetdec.
    + by have [ _ <-]:= Memory.write_mem_stable hm'.
    + exact: (valid_stk_var_stk hvm Hget hm' hvs).
    move=> i [hi1 hi2].
    rewrite (Memory.writeP_neq hm') ?hg //.
    apply between_rsp_rip_disj.
    + rewrite (Memory.valid_align Hmem) andbT.
      have [/=]:= hvm _ _ (get_find_gvar Hget).
      rewrite /ptr_size /= => x [ [? [h1 h2 h3] ?]];subst x.
      have ho : ofs < stk_size by have := wsize_size_pos sz;lia.
      apply /andP; rewrite wunsigned_rsp_add //;split;apply /ZleP;lia.
    rewrite is_align8 andbT;apply /andP; rewrite wunsigned_rip_add // wsize8;split;apply /ZleP; lia.
  Qed.

  Lemma valid_map_arr_addr mstk n x off ap :
    valid_map gm mstk stk_size glob_size ->
    vtype x = sarr n -> 
    Mvar.get mstk x = Some ap ->
    let mp := mp_of_ap ap in
    0 <= off < Z.pos n ->
    wunsigned (wptr mp.(mp_s) + wrepr U64 (off + mp.(mp_ofs))) =
    wunsigned (wptr mp.(mp_s)) + off + mp.(mp_ofs).
  Proof.
    case: x => ty xn /= hvm h; subst ty.
    move=> hget hoff;have := hvm _ _ (get_find_gvar hget).
    rewrite /= => -[sx /= [[?] [??? _]]]; subst sx.
    rewrite wunsigned_wptr_add;lia.
  Qed. 

  Lemma valid_map_word_addr mstk sz x ap: 
    valid_map gm mstk stk_size glob_size ->
    vtype x = sword sz ->
    Mvar.get mstk x = Some ap ->
    let mp := mp_of_ap ap in
    wunsigned (wptr mp.(mp_s) + wrepr U64 mp.(mp_ofs)) = wunsigned (wptr mp.(mp_s)) + mp.(mp_ofs).
  Proof.
    case: x => ty xn /= hvm h; subst ty.
    move=> hget; have  := hvm _ _ (get_find_gvar hget).
    rewrite /ptr_size /= => -[sx /= [[?] [??? _]]];subst sx.
    rewrite wunsigned_wptr_add /ptr_size => //; have ? := wsize_size_pos sz; lia.
  Qed.

  Lemma valid_get_arr sm sz n i vn ap :
    valid_map gm sm stk_size glob_size ->
    Mvar.get sm {| vtype := sarr n; vname := vn |} = Some ap ->
    mp_s (mp_of_ap ap) = MSstack ->
    let ofs := mp_ofs (mp_of_ap ap) in
    0 <= i * wsize_size sz ∧ i * wsize_size sz + wsize_size sz <= n ->
    is_align (rsp + wrepr _ (i * wsize_size sz + ofs)) sz ->
    between rsp stk_size (rsp + wrepr U64 (i * wsize_size sz + ofs)) sz &&
    is_align (rsp + wrepr U64 (i *  wsize_size sz + ofs)) sz.
  Proof. 
    move=> hvm /get_find_gvar hfind hms /= hi ->.
    have [sx]:= hvm _ _ hfind.
    rewrite /ptr_size hms /= => -[] [?] [ho1 ho2 _ _]; subst sx.
    have [hstk halign] := rsp_add.
    have ? := wsize_size_pos sz; apply/andP;split => //.
    by apply /andP;rewrite wunsigned_rsp_add;first (split;apply /ZleP); lia.
  Qed.

  Lemma find_gvar_keep_z mstk vi ofs x mp: 
    find_gvar gm (Mvar_filter (keep_z vi ofs) mstk) x = Some mp ->
    find_gvar gm mstk x = Some mp /\ ((vi = gv x) \/ mp_s mp = MSglob \/ mp_ofs mp <> ofs).
  Proof.
    rewrite /find_gvar; case: is_lvar.
    + have := Mvar_filterP (keep_z vi ofs) mstk (gv x).
      case: Mvar.get => mp' // /(_ mp') [/(_ erefl) [-> h2] _] [<-].
      move: h2; rewrite /keep_z => /orP [/eqP | ]; first by auto.
      case: mp' => /=.
      + by move=> ? /eqP; intuition.
      move => ?[] ?; first by intuition.
      by move=> /eqP; intuition.
    by case: Mvar.get => // ? [<-] /=; auto.
  Qed.

  Lemma valid_map_keep_z sm vi ofs: 
    valid_map gm sm  stk_size glob_size ->
    valid_map gm (Mvar_filter (keep_z vi ofs) sm) stk_size glob_size.
  Proof.
    move=> hm x mpx /find_gvar_keep_z [] /hm [sx] [h1 [h2 h3 h4 h5]] horx.
    exists sx;split => //; split => //.
    by move=> y mpy sy /find_gvar_keep_z [h7 hory]; apply h5.
  Qed.

  Lemma valid_stk_arr_arr_stk sm s1 s2 n n' sz xn x' ap mpx' m' v0 i (a: WArray.array n) t:
    valid_map gm sm.(mstk) stk_size glob_size ->
    let x := {| vtype := sarr n; vname := xn |} in
    Mvar.get sm.(mstk) x = Some ap ->
    mp_s (mp_of_ap ap) = MSstack ->
    let ofsx := mp_ofs (mp_of_ap ap) in
    find_gvar gm sm x' = Some mpx' ->
    (x = gv x' ∨ mp_s mpx' = MSglob ∨ mp_ofs mpx' ≠ mp_ofs (mp_of_ap ap)) ->
    vtype x'.(gv) = sarr n' ->
    get_var (evm s1) x = ok (Varr a) ->
    valid_pointer (emem s2) (rsp + wrepr _ (i * wsize_size sz + ofsx)) sz ->
    write_mem (emem s2) (rsp + wrepr _ (i * wsize_size sz + ofsx)) sz v0 = ok m' ->
    WArray.set a i v0 = ok t ->
    valid_ptr_arr (evm s1) (emem s2) (mp_s mpx') (mp_ofs mpx') n' x' ->
    valid_ptr_arr (evm s1).[x <- ok t] m' (mp_s mpx') (mp_ofs mpx') n' x'.
  Proof.
    move=> hvm x Hget hms ofsx Hget' hx' hty Ha Hvmem Hm' Ht.
    move=> H off Hoff.
    move: H=> /(_ off Hoff) /= [H H']; split => //.
    - by rewrite (Memory.write_valid _ _ Hm').
    move=> s' a0; case his: (is_lvar x');first last.
    + rewrite get_gvar_neq ?his // => /H' h v1 /h <-.
      apply: (Memory.writeP_neq Hm').
      have := find_between hvm Hget'.
      move: Hget'; rewrite /find_gvar his; case heq: Mvar.get => [ofs | //] [?]; subst mpx'.
      rewrite hty /wptr /= => /(_ _ Hoff).
      have [hi1 hi2 (*hi3*)]:= WArray.set_bound Ht.
      have := valid_get_arr hvm Hget hms (conj hi1 hi2) (Memory.valid_align Hvmem).
      by apply: between_rsp_rip_disj.
    case: (x =P x'.(gv)) => [ | /eqP] heq.
    + move: hty; rewrite /get_gvar his -heq /get_var /= Fv.setP_eq /= => -[?] h.
      have /Varr_inj [e ]:= ok_inj h; subst n' s'=> /= ?; subst a0 => {h}.
      move: Hget' H'; rewrite /find_gvar /get_gvar /wptr his -heq Hget /= => -[?]; subst mpx'.
      rewrite hms /= => H' v1 Hv1.
      have -> := Memory.write_read8 (rsp + wrepr U64 (off +  mp_ofs (mp_of_ap ap))) Hm'.
      have /= := WArray.set_get8 off Ht.
      have := valid_map_arr_addr hvm _ Hget Hoff; rewrite hms /wptr /= => -> //.
      have /(valid_map_arr_addr hvm _ Hget) : 0 <= i * wsize_size sz < Z.pos n.
      + have [??] := WArray.set_bound Ht; have ? := wsize_size_pos sz. 
        by rewrite /wsize_size;nia.
      rewrite hms /wptr /= => -> //.
      have -> : wunsigned rsp + off + mp_ofs (mp_of_ap ap) - (wunsigned rsp + i * wsize_size sz + mp_ofs (mp_of_ap ap))
                =
                off - i * wsize_size sz by ring.
      case: ifPn => // ? h.
      + by rewrite Hv1 in h; rewrite h.
      by apply: (H' _ a) => //; rewrite -h.
    rewrite get_gvar_neq //.
    move => /H'{H'}H' v /H'{H'} <-.
    apply: (Memory.writeP_neq Hm').
    case: hx'.
    + by move=> h1; move: heq; rewrite h1 eq_refl.
    case heq1 : (mp_s mpx') H.
    + rewrite /wptr /= => H _.
      have := find_between hvm Hget'.
      rewrite hty heq1 /wptr /= => /(_ _ Hoff).
      have [hi1 hi2 (*hi3*)]:= WArray.set_bound Ht.
      have := valid_get_arr hvm Hget hms (conj hi1 hi2) (Memory.valid_align Hvmem).
      by apply: between_rsp_rip_disj.
    rewrite /wptr /= => H [//| hne].
    split; eauto.
    have /(valid_map_arr_addr hvm _ Hget) : 0 <= i * wsize_size sz < Z.pos n.
    + have [??] := WArray.set_bound Ht; have ? := wsize_size_pos sz. 
      by rewrite /wsize_size;nia.
    rewrite hms /wptr /= => -> //.
    move: (Hget'); rewrite /find_gvar his; case Hget1' : Mvar.get => [ap' | //] [?];subst mpx'.
    have := valid_map_arr_addr hvm _ Hget1' Hoff.
    rewrite hty heq1 /wptr /= wsize8 => -> //.
    have [sx [/= [?]]]:= hvm _ _ (get_find_gvar Hget);subst sx.
    rewrite hms /ptr_size /= => - [??? H1].
    have := H1 _ _ _ Hget'; rewrite hty heq1; move: hne; rewrite eq_sym => /eqP /negbTE -> /(_ _ erefl erefl).
    have := wsize_size_pos sz; have [?? /=] := WArray.set_bound Ht.
    rewrite /wsize_size;nia.
  Qed.

  Lemma valid_stk_word_arr_stk sm n xan sz xw sz' ap mpw (a: WArray.array n) m' s1 s2 t v0 i:
    valid_map gm sm.(mstk) stk_size glob_size ->
    let xa := {| vtype := sarr n; vname := xan |} in
    Mvar.get sm.(mstk) xa = Some ap ->
    mp_s (mp_of_ap ap) = MSstack ->
    let ofsa := mp_ofs (mp_of_ap ap) in
    find_gvar gm sm xw = Some mpw ->
    (xa = gv xw ∨ mp_s mpw = MSglob ∨ mp_ofs mpw ≠ mp_ofs (mp_of_ap ap)) ->
    vtype xw.(gv) = sword sz' ->
    get_var (evm s1) xa = ok (Varr a) ->
    valid_pointer (emem s2) (rsp + wrepr _ (i * wsize_size sz + ofsa)) sz ->
    write_mem (emem s2) (rsp + wrepr _ (i * wsize_size sz + ofsa)) sz v0 = ok m' ->
    WArray.set a i v0 = ok t ->
    valid_ptr_word (evm s1) (emem s2) (mp_s mpw) (mp_ofs mpw) sz' xw ->
    valid_ptr_word (evm s1).[xa <- ok t] m' (mp_s mpw) (mp_ofs mpw) sz' xw.
  Proof.
    move=> hvm xa Hgeta hms ofsa Hgetw hxw hty Ha Hvmem Hm' Ht [H H'].
    split.
    + by rewrite (Memory.write_valid _ _ Hm').
    move=> /= v1.
    have hxwa : xa != xw.(gv) by rewrite vtype_diff // hty.
    rewrite get_gvar_neq // => Hv1.
    have [w' h1 h2]:= H' _ Hv1;subst v1; exists w' => //; rewrite -h1.
    apply: (Memory.writeP_neq Hm').
    case hmsw : (mp_s mpw) H.
    + move=> H;have := find_between hvm Hgetw.
      rewrite hty hmsw /wptr /ptr_size /=. 
      have [u1 u2 (*u3*)] := WArray.set_bound Ht.
      have := valid_get_arr hvm Hgeta hms (conj u1 u2) (Memory.valid_align Hvmem).
      by apply: between_rsp_rip_disj. 
    rewrite /wptr /= => H.
    split; eauto.
    rewrite /ofsa.
    have [hi1 hi2 (* hi3 *)]:= WArray.set_bound Ht; have ? := wsize_size_pos sz.
    have /(valid_map_arr_addr hvm _ Hgeta) : 0 <= i * wsize_size sz < Z.pos n.
    + by rewrite /wsize_size;nia.
    rewrite /wptr hms /= => -> //.
    have := hvm _ _ (get_find_gvar Hgeta).
    rewrite /= /ptr_size /= => -[sx [/= [?] ]]; subst sx.
    rewrite hms /= => - [??? H1].
    have := H1 _ _ _ Hgetw; rewrite hty hmsw.
    case: hxw hxwa.
    + by move=> ->; rewrite eq_refl.
    rewrite hmsw => -[// | /eqP]; rewrite eq_sym => /negbTE -> hxwa /(_ _ erefl erefl).
    have := hvm _ _ Hgetw.
    rewrite /= /ptr_size hty hmsw /wptr /= => -[sx [/= [?] [????]]]; subst sx.
    by have ? := wsize_size_pos sz'; rewrite wunsigned_rsp_add //; rewrite /wsize_size; nia. 
  Qed.

  Lemma valid_stk_arr_stk sm s1 s2 sz vn n m' v0 i ap (a: WArray.array n) t:
    valid_map gm sm.(mstk) stk_size glob_size ->
    let vi := {| vtype := sarr n; vname := vn |} in
    Mvar.get sm vi = Some ap ->
    mp_s (mp_of_ap ap) = MSstack ->
    get_var (evm s1) vi = ok (Varr a) ->
    let ofs := mp_ofs (mp_of_ap ap) in
    valid_pointer (emem s2) (rsp + wrepr _ (i * wsize_size sz + ofs)) sz ->
    write_mem (emem s2) (rsp + wrepr _ (i * wsize_size sz + ofs)) sz v0 = ok m' ->
    WArray.set a i v0 = ok t ->
    valid_stk sm (evm s1) (evm s2) (emem s2) ->
    valid_stk (Mvar_filter (keep_z vi (mp_ofs (mp_of_ap ap))) sm) (evm s1).[vi <- ok t] (evm s2) m'.
  Proof.
    move=> hvm vi Hget hms Ha ofs Hvmem Hm' Ht H x.
    case heq: find_gvar => [mpx | //].
    have [h1 h2]:= find_gvar_keep_z heq.
    have := H x;rewrite h1 => -[h3 h4]; split => //.
    case hty: vtype h4 => // [n' | ws]. 
    + by apply: (valid_stk_arr_arr_stk hvm Hget hms h1 h2 hty Ha Hvmem Hm' Ht).
    exact: (valid_stk_word_arr_stk hvm Hget hms h1 h2 hty Ha Hvmem Hm' Ht).
  Qed.

  Lemma valid_arr_stk sm sz n vn v0 i ap s1 s2 m' (a: WArray.array n) t:
    let vi := {| vtype := sarr n; vname := vn |} in
    Mvar.get sm.(mstk) vi = Some ap ->
    mp_s (mp_of_ap ap) = MSstack ->
    get_var (evm s1) vi = ok (Varr a) ->
    let ofs := mp_ofs (mp_of_ap ap) in
    write_mem (emem s2) (rsp + wrepr _ (i * wsize_size sz + ofs)) sz v0 = ok m' ->
    WArray.set a i v0 = ok t ->
    valid sm s1 s2 ->
    valid {| mstk := Mvar_filter (keep_z vi (mp_ofs (mp_of_ap ap))) sm;
            meqon := Sv.remove vi (meqon sm) |} 
          (with_vm s1 (evm s1).[vi <- ok t])
          (with_mem s2 m').
  Proof.
    move => vi Hget hms Ha ofs Hm' Ht.
    have Hvmem : valid_pointer (emem s2) (rsp + wrepr _ (i * wsize_size sz + ofs)) sz.
    + by apply/Memory.writeV; eauto.
    case => hd he hdef hvm hrip hrsp htopf hs hm hg; split => //=.
    + move=> w sz' Hvmem'. 
      rewrite (he _ _ Hvmem') //.
      symmetry; apply: (Memory.writeP_neq Hm').
      split; eauto.
      have [hi1 hi2 (*hi3*)]:= WArray.set_bound Ht; have ? := wsize_size_pos sz.
      have  /(_ _ _ erefl) /= := valid_map_arr_addr hm _ Hget. 
      rewrite hms /wptr /= => ->; last by rewrite /wsize_size;lia.
      have := hm _ _  (get_find_gvar Hget).
      rewrite /ptr_size /= hms /= => -[sx [/= [?] [??? ?]]].
      by have [ /negP /nandP [ /ZltP| /ZltP ] ]:= hd _ _ Hvmem'; rewrite /wsize_size;nia. 
    + move=> w' sz'.
      by rewrite (Memory.write_valid _ _ Hm') hdef.
    + move=> x /= /Sv_memP Hx. 
      rewrite Fv.setP_neq; last by apply/eqP;SvD.fsetdec.
      apply: hvm=> //; apply /Sv_memP; SvD.fsetdec.
    + by have [_ <-] := Memory.write_mem_stable Hm'.
    + exact: (valid_stk_arr_stk hm Hget hms Ha Hvmem Hm' Ht). 
    + by apply: valid_map_keep_z.
    move=> i0 [hi1 hi2].
    rewrite (Memory.writeP_neq Hm') ?hg //.
    apply between_rsp_rip_disj.
    + rewrite (Memory.valid_align Hvmem) andbT.
      have [/=]:= hm _ _ (get_find_gvar Hget).
      rewrite /ptr_size hms /= => x [ [? [h1 h2 h3] ?]];subst x.
      have ? :=  wsize_size_pos sz.
      have [l1 l2]:= WArray.set_bound Ht.
      apply /andP; rewrite /ofs wunsigned_rsp_add /wsize_size; [ | lia | lia].
      by split;apply /ZleP; lia.
    rewrite is_align8 andbT;apply /andP; rewrite wunsigned_rip_add // wsize8;split;apply /ZleP; lia.
  Qed.

  Lemma valid_stk_mem sm s1 s2 sz ptr off val m' m'2:
    valid_map gm sm stk_size glob_size ->
    write_mem (emem s1) (ptr + off) sz val = ok m' ->
    disjoint_zrange rsp stk_size (ptr + off) (wsize_size sz) ->
    disjoint_zrange rip glob_size (ptr + off) (wsize_size sz) ->
    write_mem (emem s2) (ptr + off) sz val = ok m'2 ->
    valid_stk sm (evm s1) (evm s2) (emem s2) ->
    valid_stk sm (evm s1) (evm s2) m'2.
  Proof.
    move=> validm Hm' Hbound Hgbound Hm'2 Hv x.
    have Hvalid : valid_pointer (emem s1) (ptr + off) sz.
    - by apply/Memory.writeV; eauto.
    move: Hv=> /(_ x).
    case Hget: find_gvar => [offx|//] [h1 h2]; split => // {h1}.
    case hty : vtype h2 => // [p | ws] H.
    + move=> off' Hoff'.
      move: H=> /(_ off' Hoff') [H H']; split.
      + by rewrite (Memory.write_valid _ _ Hm'2).
      move => t a Ht v0 Hv0.
      rewrite -(H' _ a Ht v0 Hv0).
      apply: (Memory.writeP_neq Hm'2).
      split; eauto.
      have hsz := wsize_size_pos sz.
      have := validm _ _ Hget.
      rewrite hty /= => -[sx [] [?] [? h1 _ ?]]; subst sx.
      rewrite wunsigned_wptr_add; [ | nia | nia ].
      move: h1; rewrite /wptr /ptr_size; case:ifP.
      + by case: Hbound => _ _ []; rewrite wsize8; nia.
      by case: Hgbound => _ _ []; rewrite wsize8; nia.     
    case: H => [H'' H']; split.
    + by rewrite (Memory.write_valid _ _ Hm'2).
    move=> v0 Hv0; have [w h1 h2]:= H' v0 Hv0; subst v0;exists w => //.
    rewrite -h1; apply: (Memory.writeP_neq Hm'2).
    split; eauto.
    have hsz := wsize_size_pos sz; have hsz' := wsize_size_pos ws.
    have := validm _ _ Hget.
    rewrite hty /= => -[sx [] [?] [? h3 _ ?]]; subst sx.
    rewrite wunsigned_wptr_add; [ | nia | nia ].
    move: h3; rewrite /wptr /ptr_size; case:ifP.
    + by case: Hbound => _ _ []; nia.
    by case: Hgbound => _ _ []; nia. 
  Qed.

  Lemma valid_mem sm s1 s2 m' m'2 ptr off sz val:
    write_mem (emem s1) (ptr + off) sz val = ok m' ->
    write_mem (emem s2) (ptr + off) sz val = ok m'2 ->
    valid sm s1 s2 ->
    valid sm (with_mem s1 m') (with_mem s2 m'2).
  Proof.
    move=> Hm' Hm'2 [hvd hve hdef hvm hrip hrsp htopf hvs hmap hg]. 
    split=> //=.
    + move=> sz' w Hw.
      rewrite (Memory.write_valid _ _ Hm') in Hw.
      exact: hvd.
    + move=> w sz'.
      rewrite (Memory.write_valid _ _ Hm') => Hw.
      by apply: (Memory.read_write_any_mem Hw (hve _ _ Hw) Hm' Hm'2).
    + by move=> w sz'; rewrite (Memory.write_valid w sz' Hm') (Memory.write_valid w sz' Hm'2).
    + by have [_ <-] := Memory.write_mem_stable Hm'2.
    + have Hvalid1: valid_pointer (emem s1) (ptr + off) sz.
      apply/Memory.writeV; exists m'; exact: Hm'.
      apply: (valid_stk_mem hmap Hm') Hm'2 hvs.
      + split; eauto.
        + by case: rsp_add => /ZleP.
        by have [/negP/nandP []]:= hvd _ _ Hvalid1 => /ZltP; lia.
      split; eauto.
      + by case: rip_add => /ZleP.
      by have [_ /negP/nandP []]:= hvd _ _ Hvalid1 => /ZltP; lia. 
    move=> i [hi1 hi2].
    rewrite (Memory.writeP_neq Hm'2) ? hg //.
    have ha := (write_mem_valid_pointer Hm').
    split.
    + by apply is_align_no_overflow; apply: (Memory.valid_align ha).
    + by apply is_align_no_overflow; apply is_align8.
    have []:= hvd _ _ ha.
    move=> /negP /andP;rewrite /is_true !Z.ltb_lt => ? /negP /andP;rewrite /is_true !Z.ltb_lt.
    rewrite wunsigned_rip_add // wsize8;lia.
  Qed.

  Lemma check_memW sm sz (vi : var_i) s1 s2 v v' e e':
    check_var sm vi -> alloc_e nrsp gm sm e = ok e' -> valid sm s1 s2 -> 
    value_uincl v v' ->
    forall s1', write_lval gd (Lmem sz vi e) v s1 = ok s1'->
    exists s2', write_lval [::] (Lmem sz vi e') v' s2 = ok s2' /\ valid sm s1' s2'.
  Proof.
    move => Hvar He Hv Hu s1'.
    rewrite /write_lval; t_xrbindP => ptr wi hwi hwiptr ofs we he heofs w hvw.
    move => m' Hm' <- {s1'}.
    have [wi' [-> hwi']] := check_varP Hvar (valid_vm Hv) hwi.
    rewrite /= (value_uincl_word hwi' hwiptr) /=.
    have [we' [-> hwe' /=]] := alloc_eP He Hv he.
    rewrite /= (value_uincl_word hwe' heofs) /= (value_uincl_word Hu hvw) /=.
    have [m2' Hm2'] : exists m2', write_mem (emem s2) (ptr + ofs) sz w = ok m2'.
    + apply: Memory.writeV; rewrite (valid_def Hv); apply /orP; left; apply /orP; left; apply/Memory.writeV; eauto.
    rewrite Hm2' /=; eexists; split; first by reflexivity.
    exact: (valid_mem Hm').
  Qed.

  Lemma check_vfresh_lvalP x tt : check_vfresh_lval nrsp gm x = ok tt ->
     ~~ is_vrsp nrsp gm x /\ ~~ is_vrip gm x.
  Proof. rewrite /check_vfresh_lval; case: ifP => //=; case: ifP => //=. Qed.

  Lemma check_lvarP sm sm' s1 s2 x v v':
    valid sm s1 s2 -> 
    check_lvar nrsp gm x sm = ok sm' ->
    value_uincl v v' ->
    ∀ s1', write_var x v s1 = ok s1' ->
    ∃ s2', write_var x v' s2 = ok s2' ∧ valid sm' s1' s2'.
  Proof.
    rewrite /check_lvar => hv; t_xrbindP => ? /check_vfresh_lvalP /= [ not_rsp not_rip] ??.
    by subst sm';apply: valid_write.
  Qed.

  Lemma check_lvarsP sm sm' s1 s2 x v v':
    valid sm s1 s2 -> 
    foldM (check_lvar nrsp gm) sm x = ok sm' ->
    List.Forall2 value_uincl v v' ->
    ∀ s1', write_vars x v s1 = ok s1' ->
    ∃ s2', write_vars x v' s2 = ok s2' ∧ valid sm' s1' s2'.
  Proof.
    elim: x v v' sm s1 s2 => /= [ | x xs hrec] [ | v vs] // [ | v' vs'] // sm s1 s2 hv. 
    + by move=> [<-] _ s1' [<-]; exists s2.
    + by move => _ h; inversion h.
    + by move => _ h; inversion h.
    t_xrbindP => sm1 h hs hall;inversion_clear hall => s1' s3 hw hws.   
    have [s4 [-> /= hv']]:= check_lvarP hv h H hw.
    have [s2' [-> hv'']] := hrec _ _ _ _ _ hv' hs H0 _ hws.
    by exists s2'.
  Qed.

  Lemma alloc_lvalP sm r1 r2 v v' ty s1 s2 :
    alloc_lval nrsp gm sm r1 ty = ok r2 -> valid sm s1 s2 -> 
    type_of_val v = ty -> value_uincl v v' ->
    forall s1', write_lval gd r1 v s1 = ok s1' ->
    exists s2', write_lval [::] r2.2 v' s2 = ok s2' /\ valid r2.1 s1' s2'.
  Proof.
    move=> Hr Hv Htr Hu; move: Hr.
    case: r1=> [vi t |vi|sz vi e| sz vi e] /=.
    + move=> [] ?;subst r2 => s1' H.
      have [-> _ /=]:= write_noneP H.
      exists s2;split => //.
      move: H => /write_noneP [_] H;rewrite /write_none.
      case: H.
      + by move=> [w1] /(pof_val_uincl Hu) [w2 [->]].
      by move=> [] H1 ?;subst t;move /(pof_val_undef Hu): H1 => [ | [b]] ->.

    + case: vi => -[vty vin] vii /=.
      case Hget: Mvar.get => [ofs | ]; last first.
      + by t_xrbindP => sm' ? <- /=; apply: (check_lvarP Hv).
      case: ofs Hget => [ofs | //] Hget.
      case hw: is_word_type => [ sz | //].
      have := is_word_typeP hw => ? {hw};subst vty.
      case: eqP => //  hty [?];subst ty r2 => /= s1'.
      rewrite (valid_rsp Hv) /=; apply: rbindP=> /= vm'; apply: set_varP => //= w1 h ?[?]; subst vm' s1'.
      have [{h} w' [??] ]:= type_of_val_to_pword hty h; subst v w1.
      have /= /(_ sz w') := value_uincl_word Hu .
      rewrite truncate_word_u => -> //=; rewrite /zero_extend !wrepr_unsigned.
      have Hvmem: valid_pointer (emem s2) (rsp + wrepr _ ofs) sz.
      + by rewrite (valid_def Hv); apply/orP; left; apply/orP;right; apply: valid_get_w (valid_m Hv) Hget.
      have [m' Hm'] : exists m', write_mem (emem s2) (rsp + wrepr _ ofs) sz w' = ok m'.
      + by apply/Memory.writeV.
      exists (with_mem s2 m'); split.
      + by rewrite Hm' /=.
      exact: valid_var_stk Hv Hm' Hget.
  
    + case: ifP => // hc; apply: rbindP => e' ha [<-].
      by apply: (check_memW hc ha Hv Hu).
    t_xrbindP => e' ha; case Hget: Mvar.get => [ ap | // ].
    case: eqP => // hms.
    case: ifP => // hal.
    set bidd := match _ with Some id => _ | None => _ end.
    rewrite (surjective_pairing bidd) => -[?]; subst r2.
    case: vi Hget => [[vty vn] vi] /= Hget s1'. 
    apply on_arr_varP => n' t' /subtypeEl [n1] /= [??];subst vty => hget.
    have ? : n1 = n'; last subst n1.
    + by move: hget; apply on_vuP => //= ? _ /Varr_inj [].
    t_xrbindP => i ve he hi vw hvw t'' haset vm hset ?;subst s1'.
    have [ve' [hve' vu]]:= alloc_eP ha Hv he.
    have [??] := value_uincl_int vu hi;subst ve ve'.
    have -> /= := mk_ofsP sz bidd.2 hve'.
    rewrite zero_extend_u.
    have [vx [-> /= -> /=]]: exists vx,
             Let x := get_var (evm s2) {| vtype := sword64; vname := bidd.1 |} in to_pointer x = ok vx /\
             (vx + wrepr U64 (i * wsize_size sz + bidd.2) = 
              rsp + wrepr U64 (i * wsize_size sz + mp_ofs (mp_of_ap ap)))%R.
    + rewrite /bidd; case heq: (mp_id (mp_of_ap ap)) => [id | ] /=.
      + have := valid_s Hv (mk_lvar {| v_var := {| vtype := sarr n'; vname := vn |}; v_info := 1%positive |}). 
        rewrite (get_find_gvar Hget) heq => -[-> _] /=.
        rewrite truncate_word_u;eexists;split;first by reflexivity.
        by rewrite !wrepr_add hms /wptr /= wrepr0; ssrring.ssring.
      by rewrite (valid_rsp Hv) /= truncate_word_u;eauto.
    rewrite (value_uincl_word Hu hvw) /=. 
    apply: set_varP hset => //= t1 []??; subst t1 vm.
    set ofs := mp_ofs _.
    have [m' Hm'] :exists m', 
      write_mem (emem s2) (rsp + wrepr Uptr (i * wsize_size sz + ofs)) sz vw = ok m'.
    + apply/Memory.writeV.
      case: (valid_m Hv (get_find_gvar Hget)) => sx [[heq]]. 
      rewrite -heq /ptr_size hms /= => -[hofs hofs' hal' hdisj] {hi}.
      have [hi1 hi2 (*hi3*)]:= WArray.set_bound haset.
      rewrite (valid_def Hv); apply/orP; left; apply/orP; right.
      have ? := wsize_size_pos sz.
      have ? := wunsigned_range rsp; have [? hpstk] := rsp_add.
      have ->: (rsp + wrepr U64 (i * wsize_size sz + ofs))%R = 
             ((rsp + wrepr U64 ofs) + wrepr U64 (wsize_size sz * i))%R.
      + by rewrite !wrepr_add Z.mul_comm; ssrring.ssring.
      apply /andP;split; last first.
      + apply is_align_add; first by rewrite hpstk.
        by apply is_align_mul.
      have hrsp : wunsigned (rsp + wrepr U64 ofs) = wunsigned rsp + ofs.
      + by rewrite /ofs; apply: wunsigned_rsp_add => //;lia.
      rewrite /between wunsigned_add hrsp /ofs; last by rewrite /wsize_size;nia.
      apply/andP; split; apply /ZleP;rewrite /wsize_size; nia.
    rewrite Hm' /=; eexists; split;first by reflexivity.
    rewrite /WArray.inject Z.ltb_irrefl /ofs.
    by have := valid_arr_stk Hget hms hget Hm' haset Hv; case: (t'').
  Qed.

  Lemma alloc_lvalsP sm r1 r2 vs vs' ty s1 s2 :
    alloc_lvals nrsp gm sm r1 ty = ok r2 -> valid sm s1 s2 ->
    seq.map type_of_val vs = ty -> 
    List.Forall2 value_uincl vs vs' ->
    forall s1', write_lvals gd s1 r1 vs = ok s1' ->
    exists s2', write_lvals [::] s2 r2.2 vs' = ok s2' /\ valid r2.1 s1' s2'.
  Proof.
    elim: r1 r2 sm ty vs vs' s1 s2=> //= [|a l IH] r2 sm [ | ty tys] // [ | v vs] //.
    + move=> vs' s1 s2 [<-] Hvalid _ /List_Forall2_inv_l -> {vs'} s1' [] <-.
      by exists s2; split.
    move=> vs's1 s2; t_xrbindP => a' r3 ha r4 /IH hrec <-.
    move=> Hvalid [] hty htys /List_Forall2_inv_l [v'] [vs'] [->] [hv hvs] s1' s1'' ha1 hl1.
    have [s2' [hs2' vs2']]:= alloc_lvalP ha Hvalid hty hv ha1.
    have [s2'' [hs2'' vs2'']]:= hrec _ _ _ _ vs2' htys hvs _ hl1.
    by exists s2''; split => //=; rewrite hs2'.
  Qed.

  Let P' : sprog := 
    {| p_globs := [::];
       p_funcs := SP;
       p_extra := {|
         sp_rip := gm.(stack_alloc.rip); 
         sp_globs := data; 
         sp_stk_id := nrsp |}
    |}.
   
  Let Pi_r s1 (i1:instr_r) s2 :=
    forall sm1 sm2 ii1 ii2 i2, alloc_i nrsp gm sm1 (MkI ii1 i1) = ok (sm2, MkI ii2 i2) ->
    forall s1', valid sm1 s1 s1' ->
    exists s2', sem_i (sCP:= sCP_stack) P' rip s1' i2 s2' /\ valid sm2 s2 s2'.

  Let Pi s1 (i1:instr) s2 :=
    forall sm1 sm2 i2, alloc_i nrsp gm sm1 i1 = ok (sm2, i2) ->
    forall s1', valid sm1 s1 s1' ->
    exists s2', sem_I (sCP:= sCP_stack) P' rip s1' i2 s2' /\ valid sm2 s2 s2'.

  Let Pc s1 (c1:cmd) s2 :=
    forall sm1 sm2 c2,  fmapM (alloc_i nrsp gm) sm1 c1 = ok (sm2, c2) ->
    forall s1', valid sm1 s1 s1' ->
    exists s2', sem (sCP:= sCP_stack) P' rip s1' c2 s2' /\ valid sm2 s2 s2'.

  Let Pfor (i1: var_i) (vs: seq Z) (s1: estate) (c: cmd) (s2: estate) := True.

  Let Pfun (m1: mem) (fn: funname) (vargs: seq value) (m2: mem) (vres: seq value) := True.

  Local Lemma Hskip : sem_Ind_nil Pc.
  Proof.
    move=> s sm1 sm2 /= c2 [??] s' hv;subst sm1 c2.
    exists s'; split => //; exact: Eskip. 
  Qed.

  Local Lemma Hcons : sem_Ind_cons P ev Pc Pi.
  Proof.
    move=> s1 s2 s3 i c _ Hi _ Hc sm1 _sm3 c1 /=;t_xrbindP => -[sm2 i'] hi [sm3 c'] hc /= ?? s1' hv. 
    subst c1 _sm3.
    have [s2' [si hv2]]:= Hi _ _ _ hi _ hv.
    have [s3' [sc hv3]]:= Hc _ _ _ hc _ hv2.
    exists s3'; split => //; apply: Eseq; [exact: si|exact: sc].
  Qed.

  Local Lemma HmkI : sem_Ind_mkI P ev Pi_r Pi.
  Proof.
    move=> ii i s1 s2 _ Hi sm1 sm2 [ii' ir'] ha s1' hv.
    by have [s2' [Hs2'1 Hs2'2]] := Hi _ _ _ _ _ ha _ hv; exists s2'; split.
  Qed.

  Lemma alloc_assgnP s1 s2 x tag ty e v v' ii1 ii2 i2 sm1 sm2:
    sem_pexpr gd s1 e = ok v -> 
    truncate_val ty v = ok v' -> 
    write_lval gd x v' s1 = ok s2 -> 
    Let ir := alloc_assgn nrsp gm sm1 ii1 x tag ty e in ok (ir.1, MkI ii1 ir.2) = ok (sm2, MkI ii2 i2) ->
    forall s1', valid sm1 s1 s1' → 
     ∃ s2', sem_i (sCP:= sCP_stack) P' rip s1' i2 s2' ∧ valid sm2 s2 s2'.
  Proof.
    move=> hv htr Hw; rewrite /alloc_assgn.
    t_xrbindP => -[sm i'] e'; apply: add_iinfoP => he [sm' x'].
    apply: add_iinfoP => ha /= [?] ???? s1' hs1; subst i' sm sm' ii1 i2.
    have [v1 [He' Uvv1]] := alloc_eP he hs1 hv.
    have [v1' htr' Uvv1']:= truncate_value_uincl Uvv1 htr.
    have hty := truncate_val_has_type htr.
    have [s2' [Hw' Hs2']] := alloc_lvalP ha hs1 hty Uvv1' Hw.
    by exists s2'; split=> //;apply: Eassgn;eauto.
  Qed.

  Lemma is_arr_typeP ty : is_arr_type ty -> exists n, ty = sarr n.
  Proof. case ty => //;eauto. Qed.

  Lemma is_varP e y: is_var e = Some y -> e = Pvar y.
  Proof. by case e => // ? [->]. Qed.

  Lemma find_gvar_set sm x ap z: 
    find_gvar gm (Mvar.set sm x ap) z = 
      if is_lvar z && (x == gv z) then Some (mp_of_ap ap) else find_gvar gm sm z.
  Proof.
    by rewrite /find_gvar; case: ifP => //= ?; rewrite Mvar.setP; case: ifP.
  Qed.

  Local Lemma Hassgn : sem_Ind_assgn P Pi_r.
  Proof.
    move=> s1 s2 x tag ty e v v' hv htr Hw sm1 sm2 ii1 ii2 i2 /=.
    case:x hv htr Hw => [??| x|???|????]; try by apply: alloc_assgnP.
(*    case: ifP => hty. last by apply: alloc_assgnP.
    case he : is_var => [y | ]; last by apply: alloc_assgnP.
    case: ifP => hsubty; last by apply: alloc_assgnP.
    case hf : find_gvar => [mp | ]; last by apply: alloc_assgnP.
    move=> hv htr Hw; have [n htyx] := is_arr_typeP hty; have ? := is_varP he; subst e.
    set x' := {| v_var := _ |}.
    t_xrbindP => -[_sm2 _i2] _tt; apply: add_iinfoP=> /check_vfresh_lvalP [hnrsp hnrip] [??] /= ??? s1' hva.
    subst _sm2 ii1 _i2 i2 sm2; have := valid_s hva y. 
    rewrite hf => -[h1 h2].
    set s2' := {| emem := emem s1';
                  evm := (evm s1').[x' <- ok (@Build_pword U64 U64 
                                               (wptr (mp_s mp) + wrepr U64 (mp_ofs mp))
                                               (erefl true))] |}.
    exists s2'; split.
    + case hid: mp_id h1 => [idx | ] /= h1.
      + apply: S.Eassgn => /=;eauto.
        + by rewrite /truncate_val /= zero_extend_u; auto.
        done.
      apply: S.Eopn; rewrite /sem_sopn /= /get_gvar /= (get_vptr hva) /= /s2'. 
      by do 5! f_equal;rewrite !zero_extend_u wrepr0 wrepr1; ssrring.ssring.
    move: Hw; apply rbindP =>vm1 /=; apply: set_varP;rewrite /set_var; last by rewrite {1}htyx.
    move=> t ht ?[?]; subst vm1 s2.
    move: hv; rewrite /= /s2' /x' => {s2' x'}.
    case: x hnrsp hnrip hty htyx hsubty t ht => -[xty xn] xi /= hnrsp hnrip hty htyx hsubty t ht /=.
    subst xty; case: v' htr ht => // n' a'; last first.
    + by rewrite /pof_val /=; case: (n').
    rewrite /truncate_val; t_xrbindP; case: ty => //= n1 yt.
    case: v => //=;last by case.
    move=> ny ty1 /=; rewrite /WArray.cast; case: ifP => // /ZleP hn1 [?].
    move=> /Varr_inj [?]; subst n1 => /= ? [?] hgety; subst yt t a'. 
    have hyty: vtype (gv y) = sarr ny.  
    + move: hgety; rewrite /get_gvar; case: ifP => ?.
      + rewrite /get_var; apply on_vuP => //.
        by case: (gv y) => -[] [] //= p ???? /Varr_inj [?]; subst p. 
      by rewrite /get_global; case: get_global_value => // ?; case: eqP => // <- [->].
    move: hsubty; rewrite hyty /= => /ZleP hsubty.
    case: hva => hd her hdef hvm hrip hrsp htop hfr hs hm hg {h1 he}; split => //=.
    + move=> z /= /Sv_memP hin; rewrite !Fv.setP_neq; first last.
      + by apply /eqP; SvD.fsetdec. + by apply /eqP; SvD.fsetdec.
      by apply: hvm; apply /Sv_memP; SvD.fsetdec.
    + by rewrite get_var_neq // => h; move: hnrip; rewrite h /is_vrip eq_refl.
    + by rewrite get_var_neq // => h; move: hnrsp; rewrite h /is_vrsp eq_refl.
    + move=> z; case heq: find_gvar => [mpz | //]; move: heq.
      rewrite find_gvar_set; case: andP => [ [hlv /eqP heq] [?]| hna].
      + subst mpz => /=; split; first by rewrite /get_var Fv.setP_eq.
        move: h2; rewrite -heq /= hyty => h off hoff.
        have hoff' : 0 <= off ∧ off < ny by lia.
        have [h1 /(_ _ _ hgety) h2]:= h _ hoff'; split => //.
        rewrite /get_gvar hlv -heq /get_var Fv.setP_eq /= => s' a hok. 
        have /Varr_inj [? {hok}]:= ok_inj hok.
        subst s' => /= ? v hv; subst a; apply h2. 
        apply: WArray.uincl_get hv; split => // i vi hi.
        by rewrite WArray.zget_inject //=; case: ifP.
      move=> /find_gvar_keep [hz hdiff] /=.
      have := hs z; rewrite hz => -[hz1 hz2]; split.
      + case: mp_id hdiff hz1 => // id hne.
        by rewrite get_var_neq // => -[eid]; apply hne;rewrite eid.
      case: vtype hz2 => //.
      + move=> p hp off; rewrite get_gvar_neq; first by apply hp.
        by move=> his; apply /negP => he;auto.
      move=> w [hw1 hw2]; split => // v; rewrite get_gvar_neq; first by apply hw2.
      by move=> his; apply /negP => he;auto.
    move=> z mpz; have := hm _ _ hf; rewrite hyty => -[nn [[?]]]; subst nn.
    move=> [hmp1 hmp2 hmp3 hmp4].
    rewrite find_gvar_set;case: andP => [ [hlv /eqP heq] [?]| hna].
    + subst mpz => /=; exists n; rewrite -heq /=; split => //; split => //; first by lia.
      move=> z1 mpz1 sz1;rewrite find_gvar_set;case: andP => [ [hlv1 /eqP heq1] [?]| hna1].
      + by subst mpz1; rewrite /= eq_refl -heq1 /= => ?? [].
      move=> /find_gvar_keep [hz1 hdiff] /= hsz hms. 
      have := hmp4 _ _ _ hz1 hsz hms; case:eqP;last by lia.
      move=> ? h1 [ //| hh1]; have heq1 := h1 (or_intror hh1).
      by move: hh1; rewrite -heq1 hyty.
    move=> /find_gvar_keep [hz1 hdiff].
    have [sz1 [hsz1 [hmpz1 hmpz2 hmpz3 hmpz4]]]:= hm _ _ hz1; exists sz1; split => //; split => //.
    move => s mps ss;rewrite find_gvar_set;case: andP => [ [hlv1 /eqP heq1] [?]| hna1].
    + subst mps => /=; rewrite -heq1 /= => -[?] h1; subst ss.
      have := hmp4 _ _ _ hz1 hsz1; rewrite h1 => /(_ erefl); rewrite eq_sym.
      case: eqP => ?; last lia.
      move=> h [ hh1| //]; have heq2 := h (or_intror hh1).
      by move: hh1; rewrite -heq2 hyty.
    move=> /find_gvar_keep [hh1 hh2 hh3 hh4].
    by apply: hmpz4 hh1 hh3 hh4. *)
  Qed.
  
  Local Lemma Hopn : sem_Ind_opn P Pi_r.
  Proof.
    move => s1 s2 t o xs es.
    rewrite /sem_sopn;t_xrbindP => vs va He Hop Hw sm1 sm2 ii1 ii2 i2 /=.
    t_xrbindP => -[sm3 i'] es'; apply: add_iinfoP => he [sm4 x']; apply: add_iinfoP => ha /= [??] ??? s1' hv.
    subst i' sm3 sm4 ii1 i2.
    have [va' [He' Uvv']] := (alloc_esP he hv He). 
    have [w' [Hop' Uww']]:= vuincl_exec_opn Uvv' Hop.
    have [s2' [Hw' Hvalid']] := alloc_lvalsP ha hv (sopn_toutP Hop) Uww' Hw.
    exists s2'; split=> //.
    by apply: Eopn;rewrite /sem_sopn He' /= Hop'.
  Qed.

  Lemma valid_incl sm1 sm2 s s' :
    incl sm1 sm2 -> valid sm2 s s' -> valid sm1 s s'.
  Proof.
    rewrite /incl => /andP [] /allP hall 
      /Sv.subset_spec hsub [hd her hdef hvm hrip hrsp htopf hs hm hg].
    have h: forall x mpx, find_gvar gm sm1 x = Some mpx -> find_gvar gm sm2 x = Some mpx.
    + move=> x mpx; rewrite /find_gvar; case: ifP => //.
      case heq: (Mvar.get sm1 (gv x)) => [ap | //].
      have /hall : (v_var (gv x), ap) \in Mvar.elements sm1.(mstk) by apply /Mvar.elementsP.
      by rewrite /incl_alloc_pos /=; case : Mvar.get => // ap' /eqP <-.
    split => //.
    + by move=> x /Sv_memP hin; apply hvm; apply /Sv_memP; SvD.fsetdec.
    + move=> x; have := hs x; case heq : (find_gvar gm sm1 x) => [mp |// ].
      by rewrite (h _ _ heq).
    move=> x mpx /h hf; have [sx [? [??? h1]]]:= hm x mpx hf.
    by exists sx;split => //;split => // y mpy sy /h; apply h1.
  Qed.
    
  Lemma incl_merge_l sm1 sm2 : incl (merge sm1 sm2) sm1.
  Proof.
    rewrite /merge; apply /andP => /=; split; last by apply SvP.inter_subset_1.
    apply /allP => -[x ap] /Mvar.elementsP.
    rewrite Mvar.map2P //= /incl_alloc_pos.
    case: Mvar.get => [ap1| //]; case: Mvar.get => [ap2 | ] //=.
    by case: eqP => [-> | //] [->].
  Qed.

  Lemma incl_merge_r sm1 sm2 : incl (merge sm1 sm2) sm2.
  Proof.
    rewrite /merge; apply /andP => /=; split; last by apply SvP.inter_subset_2.
    apply /allP => -[x ap] /Mvar.elementsP.
    rewrite Mvar.map2P //= /incl_alloc_pos.
    case: Mvar.get => [ap1| //]; case: Mvar.get => [ap2 | ] //=.
    by case: eqP => [-> | //] [->].
  Qed.

  Lemma incl_refl sm : incl sm sm.
  Proof.
    apply /andP; split; last by apply SvP.subset_refl.
    apply /allP => -[x ap] /Mvar.elementsP /= h.
    by rewrite /incl_alloc_pos h.
  Qed.

  Lemma incl_trans sm1 sm2 sm3: incl sm1 sm2 -> incl sm2 sm3 -> incl sm1 sm3.
  Proof.
    move=> /andP [/allP a1 s1] /andP [/allP a2 s2]; apply /andP; split.
    + apply /allP => -[x ap] /a1 /=; rewrite /incl_alloc_pos.
      case heq :  Mvar.get => [ap2| //] /eqP ?;subst ap2.
      by apply: (a2 (x, ap)); apply /Mvar.elementsP.
    apply: SvP.subset_trans s1 s2.
  Qed.

  Local Lemma Hif_true : sem_Ind_if_true P ev Pc Pi_r.
  Proof.
    move=> s1 s2 e c1 c2 Hse ? Hc sm1 sm2 ii1 ii2 i2 /=.
    t_xrbindP => -[sm3 i'] e'; apply: add_iinfoP => he [sm4 c1'] hc1 [sm5 c2'] hc2 /= [??]??? s1' hv.
    subst sm3 i' sm2 ii1 i2.
    have [b [he']]:= alloc_eP he hv Hse.
    move=> /value_uincl_bool /= -/(_ _ erefl) [_ ?];subst b.
    have [s2' [Hsem Hvalid']] := Hc _ _ _ hc1 _ hv.
    exists s2'; split; first by apply: Eif_true.
    by apply: valid_incl Hvalid'; apply incl_merge_l.
  Qed.

  Local Lemma Hif_false : sem_Ind_if_false P ev Pc Pi_r.
  Proof.
    move=> s1 s2 e c1 c2 Hse ? Hc sm1 sm2 ii1 ii2 i2 /=.
    t_xrbindP => -[sm3 i'] e'; apply: add_iinfoP => he [sm4 c1'] hc1 [sm5 c2'] hc2 /= [??]??? s1' hv.
    subst sm3 i' sm2 ii1 i2.
    have [b [he']]:= alloc_eP he hv Hse.
    move=> /value_uincl_bool /= -/(_ _ erefl) [_ ?];subst b.
    have [s2' [Hsem Hvalid']] := Hc _ _ _ hc2 _ hv.
    exists s2'; split; first by apply: Eif_false.
    by apply: valid_incl Hvalid'; apply incl_merge_r.
  Qed.

  Lemma loop2P ii check_c2 n sm sm' e' c1' c2': 
    loop2 ii check_c2 n sm = ok (sm', (e', (c1', c2'))) ->
    exists sm1 sm2, incl sm1 sm /\ check_c2 sm1 = ok ((sm', sm2), (e', (c1', c2'))) /\ incl sm1 sm2.
  Proof.
    elim: n sm => //= n hrec sm; t_xrbindP => -[[sm1 sm2] [e1 [c11 c12]]] hc2 /=; case: ifP.
    + move=> hi [] ????;subst.
      by exists sm; exists sm2;split => //; apply incl_refl.
    move=> _ /hrec [sm3 [sm4 [h1 [h2 h3]]]]; exists sm3, sm4; split => //.
    by apply: (incl_trans h1); apply incl_merge_l.
  Qed.

  Local Lemma Hwhile_true : sem_Ind_while_true P ev Pc Pi_r.
  Proof.
    move=> s1 s2 s3 s4 a c1 e c2 _ Hc1 Hv ? Hc2 ? Hwhile sm1 sm2 ii1 ii2 i2 /=.
    t_xrbindP => -[sm3 i'] [sm4 [e' [c1' c2']]] /loop2P [sm5 [sm6 [hincl1 []]]].
    t_xrbindP => -[sm7 c11] hc1 /= e1; apply: add_iinfoP => he [sm8 c22] /= hc2 ????? hincl2 [??]???.
    subst i2 ii1 sm3 sm4 i' sm7 sm8 e1 c11 c22 => s1' /(valid_incl hincl1) hv. 
    have [s2' [hs1 hv2]]:= Hc1 _ _ _ hc1 _ hv.
    have [b [he']] := alloc_eP he hv2 Hv.
    move=> /value_uincl_bool /= -/(_ _ erefl) [_ ?];subst b.
    have [s3' [hs2 /(valid_incl hincl2) hv3]]:= Hc2 _ _ _ hc2 _ hv2.
    have /= := Hwhile sm5 sm2 ii2 ii2 (Cwhile a c1' e' c2').
    rewrite Loop.nbP /= hc1 /= he /= hc2 /= hincl2 /= => /(_ erefl _ hv3) [s4'] [hs3 hv4].
    by exists s4';split => //; apply: Ewhile_true; eauto.
  Qed.

  Local Lemma Hwhile_false : sem_Ind_while_false P ev Pc Pi_r.
  Proof.
    move=> s1 s2 a c1 e c2 _ Hc1 Hv sm1 sm2 ii1 ii2 i2 /=.
    t_xrbindP => -[sm3 i'] [sm4 [e' [c1' c2']]] /loop2P [sm5 [sm6 [hincl1 []]]].
    t_xrbindP => -[sm7 c11] hc1 /= e1; apply: add_iinfoP => he [sm8 c22] /= hc2 ????? hincl2 [??]???.
    subst i2 ii1 sm3 sm4 i' sm7 sm8 e1 c11 c22 => s1' /(valid_incl hincl1) hv. 
    have [s2' [hs1 hv2]]:= Hc1 _ _ _ hc1 _ hv.
    have [b [he']] := alloc_eP he hv2 Hv.
    move=> /value_uincl_bool /= -/(_ _ erefl) [_ ?];subst b.
    by exists s2';split => //; apply: Ewhile_false; eauto.
  Qed.

  Local Lemma Hfor : sem_Ind_for P ev Pi_r Pfor.
  Proof. by []. Qed.

  Local Lemma Hfor_nil : sem_Ind_for_nil Pfor.
  Proof. by []. Qed.

  Local Lemma Hfor_cons : sem_Ind_for_cons P ev Pc Pfor.
  Proof. by []. Qed.

  Local Lemma Hcall : sem_Ind_call P ev Pi_r Pfun.
  Proof. by []. Qed.

  Local Lemma Hproc : sem_Ind_proc P ev Pc Pfun.
  Proof. by []. Qed.

  Lemma check_cP s1 c s2: sem P ev s1 c s2 -> Pc s1 c s2.
  Proof.
    apply (@sem_Ind _ _ _ P ev Pc Pi_r Pi Pfor Pfun Hskip Hcons HmkI Hassgn Hopn
             Hif_true Hif_false Hwhile_true Hwhile_false Hfor Hfor_nil Hfor_cons Hcall Hproc).
  Qed.
End PROOF.

Definition valid_map1 (m:Mvar.t Z) (size:Z) :=
  forall x px, Mvar.get m x = Some px -> 
     exists sx, size_of (vtype x) = ok sx /\
     [/\ 0 <= px, px + sx <= size,
      aligned_for (vtype x) px &
         forall y py sy, x != y -> 
           Mvar.get m y = Some py -> size_of (vtype y) = ok sy ->
           px + sx <= py \/ py + sy <= px].

Lemma init_mapP l sz m :
  init_map sz l = ok m -> 
  valid_map1 m sz.
Proof.
  rewrite /init_map.
  set f1 := (f in foldM f _ _ ).
  set g := (g in foldM _ _ _ >>= g). 
  have : forall p p',
    foldM f1 p l = ok p' -> 
    valid_map1 p.1 p.2 -> 0 <= p.2 ->
    (forall y py sy, Mvar.get p.1 y = Some py ->
        size_of (vtype y) = ok sy -> py + sy <= p.2) ->
    (p.2 <= p'.2 /\ valid_map1 p'.1 p'.2).
  + elim:l => [|[v pn] l Hrec] p p'//=.
    + by move=>[] <- ???;split=>//;omega.
    case:ifPn=> //= /Z.leb_le Hle.
    case: ifP => // Hal.
    case Hs : size_of=> [svp|]//= /Hrec /= {Hrec}Hrec H2 H3 H4. 
    have Hpos := size_of_pos Hs.
    case:Hrec.
    + move=> x px;rewrite Mvar.setP;case:ifPn => /eqP Heq.
      + move=> [] ?;subst;exists svp;split=>//;split => //.
        + omega. + omega.
        move=> y py sy Hne.
        by rewrite Mvar.setP_neq // => /H4 H /H ?;omega.
      move=> /H2 [sx] [Hsx] [] Hle0 Hpx Hal' Hy;exists sx;split=>//;split=>//.
      + omega.
      move=> y py sy Hne;rewrite Mvar.setP;case:eqP=> [?[]? |].
      + subst;rewrite Hs => -[] ?;subst; omega.
      by move=> Hney;apply Hy.
    + omega.
    + move=> y py sy;rewrite Mvar.setP;case:eqP=> [?[]?|].
      + subst;rewrite Hs => -[] ->;omega.
      move=> ? /H4 H /H ?;omega.
    move=> Hle2 H';split=>//;first by omega.
  move=> H;case Heq : foldM => [p'|]//=.
  case: (H _ _ Heq)=> //= Hp' Hv.
  rewrite /g; case:ifP => //= /Z.leb_le Hp [<-].
  move=> x px Hx.
  case :(Hv x px Hx) => //= sx [] Hsx [] H1 H2 H3 H4.
  by exists sx;split=>//;split=>//;omega.
Qed.

Lemma getfun_alloc nrsp oracle oracle_g (P:uprog) (SP:sprog) :
  alloc_prog nrsp oracle oracle_g P = ok SP ->
  exists lg mglob, 
    [/\ init_map (Z.of_nat (size SP.(p_extra).(sp_globs))) lg = ok mglob,
        check_globs (p_globs P) mglob SP.(p_extra).(sp_globs) &
        forall fn fd,
        get_fundef (p_funcs P) fn = Some fd ->
        exists fd', 
           get_fundef SP.(p_funcs) fn = Some fd' /\
           alloc_fd nrsp SP.(p_extra).(sp_rip) mglob oracle (fn, fd) = ok (fn,fd')].
Proof.
  rewrite /alloc_prog.
  case: (oracle_g P) => -[data rip] l.
  t_xrbindP => mglob; apply: add_err_msgP => heq.
  case heq1: check_globs => //; t_xrbindP => sfd hm ?; subst SP => /=.
  exists l, mglob;split => //.
  elim: (p_funcs P) sfd hm => [ | [fn1 fd1] fs hrec sfd] //=.
  t_xrbindP => -[fn2 fd2] fd2' H [??]; subst fn2 fd2'.
  move => sfds /hrec hm ?; subst sfd.
  move=> fn3 fd3 /=.
  case: eqP => ? .
  + by move=> [?]; subst fn3 fd3; exists fd2; rewrite H.
  by move=> /hm.
Qed.

Definition extend_mem (m1:mem) (m2:mem) (rip:pointer) (data: seq u8) :=
  let glob_size := Z.of_nat (size data) in
  [/\ wunsigned rip + glob_size <= wbase U64 /\
      (forall ofs s, is_align (rip + wrepr _ ofs) s = is_align (wrepr _ ofs) s), 
      (forall w sz, valid_pointer m1 w sz -> read_mem m1 w sz = read_mem m2 w sz),
      (forall w sz, valid_pointer m1 w sz ->
          ~((wunsigned rip <? wunsigned w + wsize_size sz) && (wunsigned w <? wunsigned rip + glob_size))),
      (forall w sz, valid_pointer m2 w sz = 
         valid_pointer m1 w sz || (between rip glob_size w sz && is_align w sz)) &
      (forall i, 
         0 <= i < glob_size ->
         read_mem m2 (rip + wrepr U64 i) U8 = ok (nth (wrepr U8 0) data (Z.to_nat i)))].

Lemma all_In (T:Type) (P: T -> bool) (l:seq T) x :
  all P l ->
  List.In x l -> P x.
Proof. by elim: l => //= x' l hi /andP [] hp /hi h -[<- | /h]. Qed.

Lemma valid_top (P : uprog) nrsp (stk_size : Z) (rsp : u64) (glob_size : Z) 
         (rip : u64) (data : seq u8) (gm : gmap) (sm : smap) 
         s1 s2 :
         valid P nrsp stk_size rsp glob_size rip data gm sm s1 s2 ->
 top_stack (emem s2) = rsp.
Proof.
  by move=> /valid_top_frame; rewrite /top_stack; case: frames => //= ?? [->].
Qed.

Lemma alloc_prog_stk_id nrsp oracle oracle_g P SP :
  alloc_prog nrsp oracle oracle_g P = ok SP →
  sp_stk_id SP.(p_extra) = nrsp.
Proof.
  by rewrite /alloc_prog; case: (oracle_g _) => - []; t_xrbindP => ?????; case: ifP => // _; t_xrbindP => ?? <-.
Qed.

Lemma alloc_fdP nrsp oracle oracle_g (P:uprog) SP fn fd fd':
  alloc_prog nrsp oracle oracle_g P = ok SP ->
  get_fundef (p_funcs P) fn = Some fd ->
  get_fundef (p_funcs SP) fn = Some fd' ->
  forall ev m1 va m1' vr rip, 
    sem_call P ev m1 fn va m1' vr ->
    forall m2, extend_mem m1 m2 rip SP.(p_extra).(sp_globs) ->
    (exists p, alloc_stack m2 (sf_stk_sz fd'.(f_extra)) = ok p) ->
    exists m2' vr',
      List.Forall2 value_uincl vr vr' /\
      extend_mem m1' m2' rip SP.(p_extra).(sp_globs) /\
      sem_call (sCP:=sCP_stack) SP rip m2 fn va m2' vr'.
Proof.
  move=> hap get Sget. 
  have ? := alloc_prog_stk_id hap; subst nrsp.
  have [lg [mglob [higlob hcheck hf]]]:= getfun_alloc hap.
  have [fd1 [] {hf}]:= hf _ _ get.
  rewrite Sget => -[?];subst fd1.
  rewrite /alloc_fd.
  case: (oracle (fn, fd)) => -[stk_size lv] ptrreg_of_reg .
  t_xrbindP => fd1 mstk; rewrite /add_err_fun.
  case histk : init_map => [mstk1 | //] [?]; subst mstk1.
  set gm := {| rip := _ |}; set sm0 := {| mstk := _ |}.
  move=> sm1; case hfold : foldM => [sm1' | //] [?]; subst sm1'.
  move=> [sme c]; apply: add_finfoP => hc /=.
  case: andP => // -[] hneq hres [?] ?;subst fd1 fd' => /=.
  move=> ev m1 vargs m1' vres rip hcall m2 hm2 [m2s ha].
  pose glob_size := Z.of_nat (size (sp_globs SP.(p_extra))).
  have Hstk: ptr_ok (top_stack m2s) stk_size.
  + by move: ha=> /Memory.alloc_stackP [].
  have Hglob: ptr_ok rip (Z.of_nat (size (sp_globs SP.(p_extra)))).
  + by rewrite /ptr_ok; case hm2.

  have hv : valid_map gm sm0 stk_size glob_size.
  + rewrite /sm0 => x mpx; rewrite /find_gvar /=; case:ifP => his.
    + rewrite Mvar.mapP; case heq: Mvar.get => [ofsx | // ] [?]; subst mpx.
      have [sx [h1 [h2 h3 h4 h5]]] := init_mapP histk heq.
      exists sx;split => //; split => //.
      move=> y mpy sy; case: ifP => his'.
      + rewrite  Mvar.mapP; case heq': Mvar.get => [ofsy | // ] [?]; subst mpy => /=.
        move=> hsy _; case: (v_var (gv x) =P gv y).
        + by move => heq1; move: heq'; rewrite -heq1 heq => -[->]; rewrite eq_refl.
        move=> /eqP => heq1; have := h5 _ _ _ heq1 heq' hsy; case: eqP => //.
        by have := size_of_pos h1; have := size_of_pos hsy; lia.
      by case: Mvar.get => [ofsy | //] [<-].
    case heq: Mvar.get => [ofsx' | // ] [?]; subst mpx => /=.
    have [sx [h1 [h2 h3 h4 h5]]] := init_mapP higlob heq.
    exists sx;split => //; split => //.
    move=> y mpy sy; case: ifP => his'.
    + by rewrite  Mvar.mapP; case heq': Mvar.get => [ofsy | // ] [?]; subst mpy.
    case heq': Mvar.get => [ofsy | //] [?] hsy _; subst mpy => /=.
    case: (v_var (gv x) =P gv y).
    + by move => heq1; move: heq'; rewrite -heq1 heq => -[->]; rewrite eq_refl.
    move=> /eqP => heq1; have := h5 _ _ _ heq1 heq' hsy; case: eqP => //.
    by have := size_of_pos h1; have := size_of_pos hsy; lia.
  have heq_init :
    init_state (semCallParams := sCP_stack) {| sf_stk_sz := stk_size; sf_extra := ptrreg_of_reg |} 
                          SP.(p_extra) rip {| emem := m2; evm := vmap0 |} = 
    ok {| emem := m2s; evm := vmap0.[vrsp (sp_stk_id SP.(p_extra)) <- ok (pword_of_word (top_stack m2s))]
                                             .[vrip gm <- ok (pword_of_word rip)] |}.
  + rewrite /init_state /= /init_stk_state ha /= /with_vm //=. 
    f_equal; f_equal; f_equal; [f_equal | ]; f_equal; rewrite /pword_of_word;
    f_equal; apply (Eqdep_dec.UIP_dec Bool.bool_dec).
    
  have hvalid : 
    valid P (sp_stk_id SP.(p_extra)) stk_size (top_stack m2s) 
            (Z.of_nat (size (sp_globs SP.(p_extra)))) rip (sp_globs SP.(p_extra)) gm sm0
              {| emem := m1; evm := vmap0 |}
              {| emem := m2s; evm := vmap0.[vrsp (sp_stk_id SP.(p_extra)) <- ok (pword_of_word (top_stack m2s))]
                                             .[vrip gm <- ok (pword_of_word rip)] |}.
  + case: hm2 => halign2 hread_eq hdisj hval hglob.
    have [hin hread_eqs hvals hal hdisjs htopf]:= Memory.alloc_stackP ha.
    constructor => //=.
    + move=> w sz hw; split; last by apply hdisj.
      by have := hdisjs w sz; rewrite hval hw /= => /(_ erefl) -[] h; apply /negP /nandP;
        [left|right];apply /ZltP; lia.
    + by move=> w sz hw; rewrite (hread_eq _ _ hw) hread_eqs // hval hw.
    + by move=> w sz; rewrite hvals hval -orbA (orbC (_ && _)) orbA.
(*    + by move=> x hnin hnrsp hnrip;rewrite !Fv.setP_neq // eq_sym. *)
    + by rewrite /get_var Fv.setP_eq.
    + rewrite /get_var Fv.setP_neq ? Fv.setP_eq //.
      by apply/eqP => -[k]; move/eqP: hneq; rewrite k.
    + by rewrite htopf. 
    + move=> x; rewrite /find_gvar.
      case: ifP => his. 
      + rewrite Mvar.mapP; case heq: Mvar.get => [ofs /=| //];split => //.
        have := init_mapP histk heq. 
        case Htype: (vtype (gv x))=> [| |n| sz] // [sx /= [[?] [h1 h2 h3 h4]]]; subst sx.
        + move=> off hoff; rewrite hvals; split.
          + apply /orP; right; rewrite is_align8 andbT.
            rewrite /between wsize8 /ptr_size /wptr /= (wunsigned_rsp_add Hstk); [ | nia | nia ].
            by apply/andP; split; apply/ZleP; nia.
          move=> s' a; rewrite /get_gvar his /get_var Fv.get0 /on_vu /pundef_addr Htype /= => hok.
          by have /Varr_inj [e ?]:= ok_inj hok; subst s' a; rewrite WArray.get0.
        split.
        + rewrite hvals; apply /orP; right.
          have heq2 : v_var (gv x) = {| vtype := sword sz; vname := vname (gv x) |}.
          + by case: (v_var (gv x)) Htype => /= ?? ->.
          rewrite heq2 in heq. have /(_ sz (vname (gv x)) ofs):= valid_get_w Hstk Hglob hv. 
          by rewrite /sm0 /= Mvar.mapP heq /= /wptr /= => /(_ erefl).
        by move=> ?;rewrite /get_gvar his /get_var Fv.get0 /on_vu /pundef_addr Htype.
      rewrite /gm /=; case heq: Mvar.get => [ofs /=| //]; split => //.
      have := init_mapP higlob heq. 
      case Htype: (vtype (gv x))=> [| |n| sz] // [sx /= [[?] [h1 h2 h3 h4]]]; subst sx.
      + move=> off hoff; rewrite hvals.
        have hvalid : valid_pointer m2 (wptr (top_stack m2s) rip MSglob + wrepr U64 (off + ofs)) U8.
        + rewrite hval; apply /orP; right; rewrite is_align8 andbT.
          rewrite /between wsize8 /ptr_size /wptr (wunsigned_rip_add Hglob).
          + by apply/andP; split; apply/ZleP; nia.
          + nia.
          by move: (size _) hoff h2 h1; clear=> *;lia. 
        split; first by rewrite hvalid.
        move=> s' a; rewrite /get_gvar his /get_global /get_global_value.
        case heq1 : xseq.assoc => [ []//= | //]; case: eqP => //.
(*
        rewrite Htype => -[?] heq2; subst n'; have /Varr_inj [e ?] := ok_inj heq2; subst n a => /=.
        have := all_In hcheck (xseq.assoc_mem' heq1).
        rewrite /check_glob /= heq => /andP [hle /allP hall] v hget. 
        have := hall (Z.to_nat off); rewrite mem_iota /= Z2Nat.id; last by lia.
        rewrite hget.
        match goal with |- (?A -> _) -> _ => have hh: A end.
        + by apply /ltP; case: hoff => ? /Z2Nat.inj_lt;apply.
        move=> /(_ hh) {hh} /eqP <-.
        rewrite /wptr -hread_eqs;last by move: hvalid; rewrite /wptr /=.
        rewrite hglob; last by lia. 
        rewrite Z.add_comm Z2Nat.z2nD;first last.
        + by apply /lezP;rewrite -ssrring.z0E;lia.
        + by apply /lezP;rewrite -ssrring.z0E;lia. 
        by rewrite -nth_drop wrepr0. *)
      rewrite /valid_ptr_word /get_gvar /wptr his.
      have hvalid : valid_pointer m2 (rip + wrepr U64 ofs) sz.
      + rewrite hval; apply /orP; right.
        case: halign2 => hh1 hh2; rewrite /between hh2 h3 andbT.
        rewrite (wunsigned_rip_add Hglob) //.
        + apply /andP;split; apply /ZleP;lia.
        move: (size _) (@wsize_size_pos sz) h2 => *; lia. 
      rewrite hvals hvalid;split => // v. 
      rewrite -hread_eqs // /get_global /get_global_value Htype.
      case heq1 : xseq.assoc => [[ sz' w] //= | //]; case: eqP => // -[?] [?]; subst sz' v.
      exists w => //; rewrite Memory.readP /CoreMem.read.
      have := validr_valid_pointer m2 (rip + wrepr U64 ofs)%R sz.
      rewrite hvalid => /is_okP => -[? ->] /=.
      have := all_In hcheck (xseq.assoc_mem' heq1).
Opaque Z.to_nat.
      rewrite /check_glob heq /= => /andP [hh1 /eqP hh2];subst w.
      f_equal; f_equal.
      rewrite /CoreMem.uread; apply: (@eq_from_nth _ (wrepr U8 0)).
      have hw := @le0_wsize_size sz.
      + rewrite size_map size_ziota size_take size_drop.
        case: ifPn => // /ltP; rewrite Nat2Z.inj_lt Z2Nat.id // Nat2Z.n2zB; last first. 
        rewrite -(Nat2Z.id (size _)); apply /leP; rewrite -Z2Nat.inj_le; lia.
        rewrite -subZE Z2Nat.id // => hh. 
        have -> : (wsize_size sz) = Z.of_nat (size (sp_globs SP.(p_extra))) - ofs.
        + by move:(size _) h2 hh => *; lia.
        by rewrite Z2Nat.inj_sub // Nat2Z.id.
      move=> i; rewrite size_map size_ziota => hi.
      rewrite (nth_map 0) ?size_ziota // nth_take // nth_drop nth_ziota // Memory.addP /=.
      rewrite -GRing.addrA -wrepr_add.
      move /ltP: hi; rewrite Nat2Z.inj_lt Z2Nat.id // => hi.
      have : 0 <= ofs + Z.of_nat i ∧ ofs + Z.of_nat i < Z.of_nat (size (sp_globs SP.(p_extra))).
      + by move:(size _) h2 => *; lia.
      move=> /hglob; rewrite Memory.readP /CoreMem.read CoreMem.uread8_get. 
      t_xrbindP => ?? ->; rewrite Z2Nat.inj_add //; last by apply Zle_0_nat.
      by rewrite Nat2Z.id addP.
    move=> i [hi1 hi2]; rewrite -hread_eqs; first by apply hglob.
    rewrite hval is_align8 andbT;apply /orP;right.
    apply /andP;rewrite (wunsigned_rip_add Hglob) // wsize8;split;apply /ZleP; lia.
Transparent Z.to_nat.
  inversion_clear hcall.
  case: H1 (* init_state ... *) => ?;subst s0.
  move: H;rewrite get => -[?];subst f.
  have uvargs0 : List.Forall2 value_uincl vargs0 vargs0.
  + by apply List_Forall2_refl.
  have [s1' [hwargs hvalid2 ]] := check_lvarsP hvalid hfold uvargs0 H2.
  have hdisj : 0 < stk_size -> 0 < Z.of_nat (size (sp_globs SP.(p_extra))) ->
       ((wunsigned (top_stack m2s) + stk_size <=? wunsigned rip)
                || (wunsigned rip + Z.of_nat (size (sp_globs SP.(p_extra))) <=? wunsigned (top_stack m2s))).
  + case: hm2 =>  _ _ _ hvm2 _ hss hsg. 
    have [_ _ _ _ hh _]:= Memory.alloc_stackP ha.
    have /hh : valid_pointer m2 rip U8.
    + rewrite hvm2 is_align8 andbT;apply /orP;right.
      rewrite /between Z.leb_refl /= wsize8; apply /ZleP. 
      by move: (size _) hsg => *;lia.
    have /hh : valid_pointer m2 (rip + wrepr Uptr (Z.of_nat (size (sp_globs SP.(p_extra))) - 1)) U8.
    + rewrite hvm2 is_align8 andbT;apply /orP;right.
      rewrite /between (wunsigned_rip_add Hglob); [ | lia | lia]. 
      by rewrite wsize8; apply /andP; split; apply /ZleP; by move: (size _) hsg => *; lia.
    rewrite wsize8 (wunsigned_rip_add Hglob); [ | lia | lia]. 
    move=> a1 a2;apply /orP.
    rewrite /is_true !Z.leb_le. 
    case: a1; first by lia.
    case: a2; last by lia.
    move=> h_1 h_2.
    have u1 : stk_size < Z.of_nat (size (sp_globs SP.(p_extra))) by lia.
    have /hh : valid_pointer m2 (top_stack m2s) U8.
    + rewrite hvm2 is_align8 andbT;apply /orP;right.
      rewrite /between wsize8; apply /andP.
      move: (size _) h_1 h_2 => *; split; apply /ZleP; lia.
    by rewrite wsize8; lia.   
  have [s3 [hssem hvalid3]] := check_cP (P:= P) SP.(p_funcs) Hstk Hglob hdisj H3 hc hvalid2.
  have [vres1 [H1' uincl1]]:= check_varsP hres (valid_vm hvalid3) H4.
  have [vres2 htr uincl2]:= mapM2_truncate_val H5 uincl1.
  exists (free_stack (emem s3) stk_size), vres2.
  split => //; split.
  + have /Memory.free_stackP [h1 h2 h3 h4 (* h5 *)]: 
     omap snd (ohead (frames(emem s3))) = Some stk_size.
    + by rewrite (valid_top_frame hvalid3).
    have [u1 u2 u3 u4 u5] := hm2.
    have vdisj: forall w sz, valid_pointer m1' w sz ->  disjoint_zrange (top_stack m2s) stk_size w (wsize_size sz).
    + subst m1'; move=> w sz hw; have [ /negP /andP] := valid_disj hvalid3 hw.
      rewrite {1 2}/is_true !Z.ltb_lt => ??; split.
      + by apply /ZleP; case: Hstk.
      + by apply is_align_no_overflow; apply (Memory.valid_align hw).
      lia.
    subst m1';split => //.
    + move=> w sz hw.
      rewrite (valid_eq hvalid3) // h1 // h2 (valid_def hvalid3) /= hw /=; split=> //.
      by rewrite (valid_top hvalid3); apply vdisj.
    + by move=> w sz /(valid_disj hvalid3) [].
    + move=> w sz. 
      apply Bool.eq_iff_eq_true; have := h2 w sz.
      rewrite {1}/is_true => ->.
      rewrite (valid_def hvalid3) /= (valid_top hvalid3); split.
      + move=> [] /orP [].
        + move=> /orP [-> //| ].
          move=> /andP [] /andP [] /ZleP ? /ZleP ?? [???].
          by have := wsize_size_pos sz;lia.
        by move=> /andP [-> ->] /=;rewrite orbT.         
      move=> /orP [ hw | /andP [hb hal]]. 
      + by rewrite hw /=;split => //; apply: vdisj.
      rewrite hb hal !orbT;split => //; split.
      + by apply /ZleP; case: Hstk.
      + by apply is_align_no_overflow.
      have : valid_pointer m2 w sz by rewrite u4 hb hal /= orbT.
      have [_ _ _ _ h _]:= Memory.alloc_stackP ha.
      by move=> /h; lia.
    move=> i [hi1 hi2].
    rewrite -h1.
    + by rewrite (valid_glob hvalid3).
    rewrite h2 (valid_top hvalid3) (valid_def hvalid3) is_align8 !andbT; split.
    + apply /orP;right; apply /andP.
      rewrite (wunsigned_rip_add Hglob) // wsize8;split;apply /ZleP.
      lia. move: hi1 hi2; rewrite /h4; move: (size _) => *;lia.
    split.
    + by apply /ZleP; case: Hstk.
    + by apply is_align_no_overflow; apply is_align8.
    have :  valid_pointer m2 (rip + wrepr U64 i) U8.
    + rewrite u4 is_align8 andbT; apply /orP;right.
      by apply /andP; rewrite (wunsigned_rip_add Hglob) // wsize8;split;apply /ZleP;lia.
    have [_ _ _ _ h _]:= Memory.alloc_stackP ha.
    by move=> /h;lia.
  econstructor;eauto.
  move: hap hssem => /=; rewrite /alloc_prog.
  by case: oracle_g => -[???]; t_xrbindP => ??; case:ifP => // ?; t_xrbindP => ?? <-.
Qed.

Definition alloc_ok (SP:sprog) fn m1 :=
  forall fd, get_fundef (p_funcs SP) fn = Some fd ->
  exists p, alloc_stack m1 (sf_stk_sz fd.(f_extra)) = ok p.

Lemma alloc_progP nrsp oracle oracle_g (P: uprog) (SP: sprog) fn:
  alloc_prog nrsp oracle oracle_g P = ok SP ->
  forall ev m1 va m1' vr rip, 
    sem_call (sCP := sCP_unit) P ev m1 fn va m1' vr ->
    forall m2, extend_mem m1 m2 rip SP.(p_extra).(sp_globs) ->
    alloc_ok SP fn m2 ->
    exists m2' vr',
      List.Forall2 value_uincl vr vr' /\
      extend_mem m1' m2' rip SP.(p_extra).(sp_globs) /\
      sem_call (sCP := sCP_stack) SP rip m2 fn va m2' vr'.
Proof.
  move=> Hcheck ev m1 va m1' vr rip hsem m2 he ha.
  have [fd hget]: exists fd, get_fundef (p_funcs P) fn = Some fd.
  + by case: hsem=> ??? fd *;exists fd.
  have [lg [mglob [higlob hcheck hf]]]:= getfun_alloc Hcheck.
  have [fd' [hgetS ?]]:= hf _ _ hget.
  by apply: (alloc_fdP Hcheck hget hgetS hsem he (ha _ hgetS)).
Qed.
