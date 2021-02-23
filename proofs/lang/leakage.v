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
From mathcomp Require Import all_ssreflect all_algebra.
From CoqWord Require Import ssrZ.
Require Import Psatz xseq.
Require Export array expr gen_map low_memory warray_ sem_type.
Import Utf8.

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Delimit Scope leakage_scope with leakage.
Open Scope leakage_scope.

Inductive leak_e :=
| LEmpty : leak_e (* no leak *)
| LIdx : Z -> leak_e (* array access at given index *)
| LAdr : pointer -> leak_e (* memory access at given address *)
| LSub: (seq leak_e) -> leak_e. (* forest of leaks *)

Section Eq_leak_e.

Variable eq_leak_e : leak_e -> leak_e -> bool.

Fixpoint eq_leak_es (les: seq leak_e) (les' : seq leak_e) : bool :=
match les, les' with 
| [::], [::] => true
| a :: l, a' :: l' => andb (eq_leak_e a a') (eq_leak_es l l')
| _, _ => false
end.

End Eq_leak_e.

Fixpoint eq_leak_e (le : leak_e) (le' : leak_e) : bool :=
match le, le' with 
| LEmpty, LEmpty => true
| LIdx z, LIdx z' => if z == z' then true else false
| LAdr p, LAdr p' => if p == p' then true else false
| LSub les, LSub les' => eq_leak_es eq_leak_e les les'
| _, _ => false
end.

Definition leak_lea_exp : leak_e :=
  LSub [:: LEmpty; LSub [:: LEmpty; LSub [:: LEmpty; LEmpty]]].
  
Definition get_seq_leak_e (l : leak_e) : seq leak_e := 
match l with 
| LSub le => le
| _ => [::]
end.

Fixpoint get_nth_leak (l : seq leak_e) n : leak_e := 
 match l with 
  | [::] => LEmpty
  | x :: l => if n == 0 then x else get_nth_leak l (n-1)
 end.

Definition get_leak_e (l : leak_e) : leak_e := 
match l with 
| LSub le => if (size le) == 1 then (get_nth_leak le 0) else LEmpty
| _ => LEmpty
end.

Fixpoint make_leak_e_sub (l : leak_e) : leak_e :=
match l with 
| LSub le => LSub (map make_leak_e_sub le)
| _ => LSub [:: l]
end.

Inductive leak_i : Type :=
  | Lopn  : leak_e ->leak_i                                                  (* 1 *)      
  | Lcond  : leak_e -> bool -> seq leak_i -> leak_i                          (* 1 + cost (seq leak_i) *)
  | Lwhile_true : seq leak_i -> leak_e -> seq leak_i -> leak_i -> leak_i     
  | Lwhile_false : seq leak_i -> leak_e -> leak_i
  | Lfor : leak_e -> seq (seq leak_i) -> leak_i                              (* seq (seq leak_i) + size (seq (seq leak_i)) *)
  | Lcall : leak_e -> (funname * seq leak_i) -> leak_e -> leak_i.            (* size leak_e + cost (seq leak_i) + size leak_e *)

Notation leak_c := (seq leak_i).

Notation leak_for := (seq leak_c) (only parsing).

Notation leak_fun := (funname * leak_c)%type.

Section Eq_leak_i.

Variable eq_leak_i : leak_i -> leak_i -> bool.

Fixpoint eq_leak_is (les: seq leak_i) (les' : seq leak_i) : bool :=
match les, les' with 
| [::], [::] => true
| a :: l, a' :: l' => andb (eq_leak_i a a') (eq_leak_is l l')
| _, _ => false
end.

Fixpoint eq_leak_iss (les: seq (seq leak_i)) (les' : seq (seq leak_i)) : bool :=
match les, les' with 
| [::], [::] => true
| a :: l, a' :: l' => andb (eq_leak_is a a') (eq_leak_iss l l')
| _, _ => false
end.

End Eq_leak_i.

Fixpoint eq_leak_i (li : leak_i) (li' : leak_i) : bool :=
match li, li' with 
 | Lopn le, Lopn le' => eq_leak_e le le'
 | Lcond le b lis, Lcond le' b' lis' => andb (eq_leak_e le le') (andb (andb b b') (eq_leak_is eq_leak_i lis lis'))
 | Lwhile_true lis le lis' l, Lwhile_true lis1 le' lis1' l'=> 
   andb (andb (eq_leak_is eq_leak_i lis lis1) (eq_leak_e le le'))
        (andb (eq_leak_is eq_leak_i lis' lis1') (eq_leak_i l l'))
  | Lwhile_false lis le, Lwhile_false lis' le' => andb (eq_leak_is eq_leak_i lis lis') (eq_leak_e le le')
  | Lfor le liss, Lfor le' liss' => andb (eq_leak_e le le') (eq_leak_iss eq_leak_i liss liss')
  | Lcall le (fn, lis) le', Lcall le1 (fn1, lis1) le1' => if fn == fn1 
    then andb (andb (eq_leak_e le le1) (eq_leak_is eq_leak_i lis lis1))
              (eq_leak_e le' le1') else false
  | _, _ => false
end.

(* ------------------------------------------------------------------------ *)
(* Leakage trees and leakage transformations. *)

Inductive leak_tr_p :=
  | LS_const of pointer 
  | LS_stk
  | LS_Add `(leak_tr_p) `(leak_tr_p) 
  | LS_Mul `(leak_tr_p) `(leak_tr_p).

Fixpoint eq_leak_tr_p (ltp : leak_tr_p) (ltp' : leak_tr_p) : bool :=
match ltp, ltp' with 
 | LS_const p, LS_const p' => if p == p' then true else false
 | LS_stk, LS_stk => true
 | LS_Add l l', LS_Add l1 l1' => andb (eq_leak_tr_p l l1) (eq_leak_tr_p l' l1')
 | LS_Mul l l', LS_Mul l1 l1' => andb (eq_leak_tr_p l l1) (eq_leak_tr_p l' l1')
 | _, _ => false
end.

(*Inductive leak_tr_const := 
  | LTleak  `(leak_e)
  | LTAdr   `(leak_tr_p).
(*  | LTSub   `(seq leak_tr_const). *)
*)

(* Leakage transformer for expressions *)
Inductive leak_e_tr :=
| LT_id (* preserve *)
| LT_remove (* remove *)
| LT_const : leak_tr_p -> leak_e_tr
| LT_subi : nat -> leak_e_tr (* projection *)
| LT_lidx : (Z -> leak_tr_p) -> leak_e_tr
| LT_map : seq leak_e_tr -> leak_e_tr (* parallel transformations *)
| LT_seq : seq leak_e_tr -> leak_e_tr
| LT_compose: leak_e_tr -> leak_e_tr -> leak_e_tr (* compositon of transformations *)
(* lowering *)
| LT_rev : leak_e_tr. (* reverse transformation *)
(*| LT_var : leak_e_tr -> leak_e -> leak_e_tr
| LT_adr : Z -> Z -> leak_e_tr 
| LT_adrptr : pointer -> Z -> Z -> leak_e_tr.*)

Section Eq_leak_e_tr.
Variable eq_leak_e_tr : leak_e_tr -> leak_e_tr -> bool.

Fixpoint eq_leak_e_trs (ltes : seq leak_e_tr) (ltes' : seq leak_e_tr) : bool :=
match ltes, ltes' with 
| [::], [::] => true 
| [::], _ => false
| a :: l, a' :: l' => andb (eq_leak_e_tr a a') (eq_leak_e_trs l l')
| _, _ => false
end.

End Eq_leak_e_tr.

Fixpoint eq_leak_e_tr (lte : leak_e_tr) (lte' : leak_e_tr) : bool :=
match lte, lte' with 
| LT_id, LT_id => true
| LT_remove, LT_remove => true
| LT_const ltp, LT_const ltp' => eq_leak_tr_p ltp ltp'
| LT_subi n, LT_subi n' => if n == n' then true else false
| LT_map ltes, LT_map ltes' => eq_leak_e_trs eq_leak_e_tr ltes ltes'
| LT_seq ltes, LT_seq ltes' => eq_leak_e_trs eq_leak_e_tr ltes ltes'
| LT_compose lte lte', LT_compose lte1 lte1' => andb (eq_leak_e_tr lte lte1) (eq_leak_e_tr lte' lte1')
| LT_rev, LT_rev => true
| _, _ => false
(* need to include case of LT_lidx *)
end.

Inductive leak_e_es_tr :=
| LT_leseq : leak_e_es_tr
| LT_emseq : leak_e_es_tr
| LT_subseq : leak_e_tr -> leak_e_es_tr
| LT_idseq : leak_e_tr -> leak_e_es_tr
| LT_dfst : leak_e_es_tr
| LT_dsnd : leak_e_es_tr.

Definition eq_leak_e_es_tr (lts lts' : leak_e_es_tr) : bool :=
match lts, lts' with 
| LT_leseq, LT_leseq => true 
| LT_emseq, LT_emseq => true 
| LT_subseq lte, LT_subseq lte' => eq_leak_e_tr lte lte' 
| LT_idseq lte, LT_idseq lte' => eq_leak_e_tr lte lte'
| LT_dfst, LT_dfst => true 
| LT_dsnd, LT_dsnd => true
| _, _ => false
end.

Definition get_seq_leak_e_tr (l : leak_e_tr) : seq leak_e_tr := 
match l with 
| LT_seq le => le
| _ => [::]
end.

Fixpoint eval_leak_tr_p stk lp : pointer :=
  match lp with
  | LS_const p => p 
  | LS_stk     => stk
  | LS_Add p1 p2 => (eval_leak_tr_p stk p1 + eval_leak_tr_p stk p2)%R
  | LS_Mul p1 p2 => (eval_leak_tr_p stk p1 * eval_leak_tr_p stk p2)%R
  end.

Fixpoint leak_E (stk:pointer) (lt : leak_e_tr) (l : leak_e) : leak_e :=
  match lt, l with
  | LT_map lts, LSub xs => LSub (map2 (leak_E stk) lts xs)
  | LT_seq lts, _ => LSub (map (fun lt => leak_E stk lt l) lts)
  | LT_lidx f, LIdx i => LAdr (eval_leak_tr_p stk (f i))
  | LT_const f, _     => LAdr (eval_leak_tr_p stk f)
  | LT_id, _ => l
  | LT_remove, _ => LEmpty
  | LT_subi i, LSub xs => nth LEmpty xs i
  | LT_compose lt1 lt2, _ => leak_E stk lt2 (leak_E stk lt1 l)
  | LT_rev, LSub xs => LSub (rev xs)
  (*| LT_adr z1 z2 , LIdx i => LAdr (wrepr U64 (i*z1+z2))
  | LT_var lte le , LEmpty => LSub [:: leak_E lte LEmpty; le]
  | LT_adrptr p1 z1 z2 , LIdx i => LAdr (p1 + (wrepr U64 (i*z1+z2)))*)
  | _, _ => LEmpty
  end.

Definition leak_E_S (stk: pointer) (lts: seq leak_e_tr) (ls : seq leak_e) : seq leak_e :=
  map2 (leak_E stk) lts ls.

(* Transformation from leakage to sequence of leakage *)
Fixpoint leak_ES (stk : pointer) (lte : leak_e_es_tr) (le : leak_e) : seq leak_e :=
match lte, le with
| LT_leseq, le => [:: le]
| LT_emseq, le => [::]
| LT_subseq lte, le => [:: leak_E stk lte le]
| LT_idseq lte, le => get_seq_leak_e (leak_E stk lte le)
| LT_dfst, le => [:: LEmpty; LEmpty; LEmpty; LEmpty; LEmpty; le; LEmpty]
| LT_dsnd, le => [:: LEmpty; LEmpty; LEmpty; LEmpty; LEmpty; LEmpty; le]
end.            

Inductive leak_e_i_tr :=
| LT_iconditionl : leak_e_tr -> leak_e_i_tr (* lower condition transformer *)
| LT_iemptyl : leak_e_i_tr.

Definition eq_leak_e_i_tr (lei : leak_e_i_tr) (lei' : leak_e_i_tr) : bool :=
match lei, lei' with 
| LT_iconditionl lte, LT_iconditionl lte' => eq_leak_e_tr lte lte'
| LT_iemptyl, LT_emptyl => true
| _, _ => false
end.

Inductive leak_es_i_tr :=
| LT_iopn5f_large : leak_es_i_tr
| LT_iopn5f_other : leak_es_i_tr
| LT_iaddcarryf : leak_es_i_tr -> leak_es_i_tr
| LT_iaddcarry : leak_es_i_tr -> leak_es_i_tr
| LT_ianone : leak_es_i_tr
| LT_imul1 : leak_es_i_tr
| LT_imul2 : leak_es_i_tr
| LT_imul3 : leak_es_i_tr
| LT_imul4 : leak_es_i_tr
| LT_iemptysl : leak_es_i_tr.

Fixpoint eq_leak_es_i_tr (ltes ltes' : leak_es_i_tr) : bool :=
match ltes, ltes' with 
| LT_iopn5f_large, LT_iopn5f_large => true 
| LT_iopn5f_other, LT_iopn5f_other => true  
| LT_iaddcarryf lts, LT_iaddcarryf lts' => eq_leak_es_i_tr lts lts'
| LT_iaddcarry lts, LT_iaddcarry lts' => eq_leak_es_i_tr lts lts'
| LT_ianone, LT_ianone => true 
| LT_imul1, LT_imul1 => true 
| LT_imul2, LT_imul2 => true
| LT_imul3, LT_imul3 => true 
| LT_imul4, LT_imul4 => true 
| LT_iemptysl, LT_iemptysl => true
| _, _ => false
end.

(* Leakge transformer for instructions *)

(* tr_cost lt -> Exact n1 n2  | Linear a  
   lt' = leak_tr lt
   cost lt' <= a cost lt + b
*) 

Inductive leak_i_tr :=
(* structural transformation *)
| LT_ikeep : leak_i_tr                                                                  (* Exact 1 1 *)
| LT_ile : leak_e_tr -> leak_i_tr  (* assign and op *)                                  (* Exact 1 1 *)       
| LT_icond : leak_e_tr -> seq leak_i_tr -> seq leak_i_tr -> leak_i_tr (* if *)          (* linear comp *)   
| LT_iwhile : seq leak_i_tr -> leak_e_tr -> seq leak_i_tr -> leak_i_tr (* while *)      (* *)
| LT_ifor : leak_e_tr -> seq leak_i_tr -> leak_i_tr                  (* for c --> for c'     cost c' <= a cost c   cost (for c) = n * (cost c + 1)  <= n * (a cost c' + 1) <= max(1,a)cost (for c')  *)
| LT_icall : leak_e_tr -> leak_e_tr -> leak_i_tr                     (* add funname *)

| LT_iremove : leak_i_tr                                                                 (* 1 -> 0 *)      


| LT_icond_eval : seq leak_i_tr -> leak_i_tr     (* if b then c1 else c0   -> cb   wh*)  (* cost cb' <= a cost cb + b   cost cb' <= cost (if b then ...) *)
| LT_ifor_unroll: seq leak_i_tr -> leak_i_tr
| LT_icall_inline: leak_c -> seq leak_i_tr -> leak_i_tr
(* lowering leak transformers *)
| LT_icondl : leak_e_i_tr -> leak_e_tr -> seq leak_i_tr -> seq leak_i_tr -> leak_i_tr
| LT_iwhilel :  leak_e_i_tr -> leak_e_tr -> seq leak_i_tr -> seq leak_i_tr -> leak_i_tr
| LT_icopn : leak_es_i_tr -> leak_i_tr
(* lowering assgn *)
| LT_ilmov1 : leak_i_tr
| LT_ilmov2 : leak_i_tr
| LT_ilmov3 : leak_i_tr
| LT_ilmov4 : leak_i_tr
| LT_ilinc : leak_e_es_tr -> leak_i_tr
| LT_ilcopn : leak_e_es_tr -> leak_i_tr
| LT_ilsc : leak_i_tr
| LT_ild : leak_i_tr
| LT_ildc : leak_i_tr
| LT_ildcn : leak_i_tr
| LT_ilmul : leak_es_i_tr -> leak_e_tr -> leak_i_tr
| LT_ileq : leak_e_es_tr -> leak_i_tr
| LT_illt : leak_e_es_tr -> leak_i_tr
| LT_ilif : leak_e_i_tr -> leak_e_tr -> leak_i_tr
| LT_ilea : leak_i_tr
| LT_ilfopn : leak_es_i_tr -> leak_e_es_tr -> leak_i_tr
| LT_ilds : leak_i_tr
| LT_ildus : leak_i_tr
| LT_ildiv : leak_i_tr -> leak_e_es_tr -> leak_i_tr
| LT_ilasgn : leak_i_tr.
(*| LT_icompose : leak_i_tr -> leak_i_tr -> leak_i_tr.*)

Section Eq_leak_i_tr.
Variable eq_leak_i_tr : leak_i_tr -> leak_i_tr -> bool.

Fixpoint eq_leak_i_trs (ltis : seq leak_i_tr) (ltis' : seq leak_i_tr) : bool :=
match ltis, ltis' with 
| [::], [::] => true 
| [::], _ => false
| a :: l, a' :: l' => andb (eq_leak_i_tr a a') (eq_leak_i_trs l l')
| _, _ => false
end.

End Eq_leak_i_tr.

Fixpoint eq_leak_i_tr (lti : leak_i_tr) (lti' : leak_i_tr) : bool :=
match lti, lti' with 
| LT_iremove, LT_iremove => true
| LT_ikeep, LT_ikeep => true
| LT_ile lte, LT_ile lte' => eq_leak_e_tr lte lte'
| LT_icond lte ltis ltis', LT_icond lte' ltis1 ltis1' => 
  andb (eq_leak_e_tr lte lte') 
       (andb (eq_leak_i_trs eq_leak_i_tr ltis ltis1) (eq_leak_i_trs eq_leak_i_tr ltis' ltis1'))
| LT_iwhile ltis lte ltis', LT_iwhile ltis1 lte' ltis1' => 
  andb (andb (eq_leak_i_trs eq_leak_i_tr ltis ltis1) (eq_leak_e_tr lte lte')) 
       (eq_leak_i_trs eq_leak_i_tr ltis' ltis1')
| LT_icond_eval ltis, LT_icond_eval ltis'=> eq_leak_i_trs eq_leak_i_tr ltis ltis'
| LT_ifor lte ltis, LT_ifor lte' ltis' => andb (eq_leak_e_tr lte lte') (eq_leak_i_trs eq_leak_i_tr ltis ltis') 
| LT_icall lte lte', LT_icall lte1 lte1' => andb (eq_leak_e_tr lte lte1) (eq_leak_e_tr lte' lte1')
| LT_ifor_unroll ltis, LT_ifor_unroll ltis' => eq_leak_i_trs eq_leak_i_tr ltis ltis'
| LT_icall_inline lc ltis, LT_icall_inline lc' ltis' => andb (eq_leak_is eq_leak_i lc lc') (eq_leak_i_trs eq_leak_i_tr ltis ltis')
| LT_icondl ltei lte ltis ltis', LT_icondl ltei' lte' ltis1 ltis1' => 
  andb (andb (eq_leak_e_i_tr ltei ltei') (eq_leak_e_tr lte lte'))
       (andb (eq_leak_i_trs eq_leak_i_tr ltis ltis1) (eq_leak_i_trs eq_leak_i_tr ltis' ltis1'))
| LT_iwhilel ltei lte ltis ltis', LT_iwhilel ltei' lte' ltis1 ltis1' => 
  andb (andb (eq_leak_e_i_tr ltei ltei') (eq_leak_e_tr lte lte'))
       (andb (eq_leak_i_trs eq_leak_i_tr ltis ltis1) (eq_leak_i_trs eq_leak_i_tr ltis' ltis1'))
| LT_icopn lts, LT_icopn lts' => eq_leak_es_i_tr lts lts'
| LT_ilmov1, LT_ilmov1 => true
| LT_ilmov2, LT_ilmov2 => true 
| LT_ilmov3, LT_ilmov3 => true 
| LT_ilmov4, LT_ilmov4 => true
| LT_ilinc lts, LT_ilinc lts' => eq_leak_e_es_tr lts lts'
| LT_ilcopn lts, LT_ilcopn lts' => eq_leak_e_es_tr lts lts'
| LT_ilsc, LT_ilsc => true 
| LT_ild, LT_ild => true 
| LT_ildc, LT_ildc => true 
| LT_ildcn, LT_ildcn => true
| LT_ilmul lts lte, LT_ilmul lts' lte' => andb (eq_leak_es_i_tr lts lts') (eq_leak_e_tr lte lte')  
| LT_ileq lts, LT_ileq lts' => (eq_leak_e_es_tr lts lts') 
| LT_illt lts, LT_illt lts' => (eq_leak_e_es_tr lts lts')
| LT_ilif ltei lte, LT_ilif ltei' lte' => andb (eq_leak_e_i_tr ltei ltei') (eq_leak_e_tr lte lte')
| LT_ilea, LT_ilea => true
| LT_ilfopn ltes lts, LT_ilfopn ltes' lts' =>  andb (eq_leak_es_i_tr ltes ltes') (eq_leak_e_es_tr lts lts')
| LT_ilds, LT_ilds => true
| LT_ildus, LT_ildus => true 
| LT_ildiv lti lts, LT_ildiv lti' lts' => andb (eq_leak_i_tr lti lti') (eq_leak_e_es_tr lts lts')
| LT_ilasgn, LT_ilasgn => true 
| _ ,_ => false
end.

(* Transformation from expression leakage to instruction leakage *)
Fixpoint leak_EI (stk : pointer) (lti : leak_e_i_tr) (le : leak_e) : seq leak_i :=
match lti, le with 
| LT_iconditionl lte, le => [:: Lopn (LSub [:: leak_E stk lte le; LSub [:: LEmpty; LEmpty; LEmpty; LEmpty; LEmpty]])]
| LT_iemptyl, le => [::]
end.


Fixpoint remove_leak (ls: seq leak_e) : seq leak_e :=
match ls with
| a :: l =>  List.tl l
| [::] => [::]
            end.

Fixpoint remove_last_leak (ls: seq leak_e) : seq leak_e :=
match ls with
| [::] => [::]
| [:: a] => [::]
| a :: l => a :: remove_last_leak l
end.

Fixpoint last_leak (ls : seq leak_e) : leak_e :=
match ls with 
| [::] => LEmpty
| [:: a] => a
| a :: l => last_leak l
end.

Section Leak_Equal.
Variable eq_leak : leak_e -> leak_e -> bool.

Fixpoint eq_leaks (les: seq leak_e) (les' : seq leak_e) : bool :=
match les, les' with 
| [::], [::] => true
| [::], _ => false
| a :: l, a' :: l' => (andb (eq_leak a a') (eq_leaks l l'))
| _, _ => false
end.

End Leak_Equal.

Fixpoint eq_leak (le le' : leak_e) : bool :=
match le, le' with
| LEmpty, LEmpty => true
| LIdx i, LIdx i' => if i == i' then true else false
| LAdr a , LAdr a' => if a == a' then true else false
| LSub l, LSub l' => eq_leaks eq_leak l l'
| _, _ => false
end. 

(* Transformation from expressions (seq of expression) leakage to instruction leakage *)
Fixpoint leak_ESI (stk : pointer) (lti : leak_es_i_tr) (les: seq leak_e) (les': seq leak_e) : seq leak_i :=
match lti, les, les' with 
| LT_iopn5f_large, les, les' =>  ([:: Lopn (LSub [:: LSub [:: nth LEmpty les 1]; LSub [:: LEmpty]])] ++
    [:: Lopn (LSub [:: LSub ([::nth LEmpty les 0; LEmpty] ++ (remove_leak les)); 
                       LSub les'])])
| LT_iopn5f_other, les, les' =>
  [:: Lopn (LSub [:: LSub les ; LSub les'])]
| LT_iaddcarryf ltes, les, les' => leak_ESI stk ltes (remove_last_leak les) [:: LEmpty; get_nth_leak les' 0; LEmpty; LEmpty; LEmpty; get_nth_leak les' 1]
| LT_iaddcarry ltes, les, les' =>  leak_ESI stk ltes les [:: LEmpty; get_nth_leak les' 0; LEmpty; LEmpty; LEmpty; get_nth_leak les' 1]
| LT_ianone, les, les' => [:: Lopn (LSub [:: LSub les ; LSub les'])]
| LT_imul1, les, les' => ([:: Lopn (LSub [:: LSub [:: nth LEmpty les 0]; LSub [:: LEmpty]])] ++
 [:: Lopn (LSub [:: LSub [:: nth LEmpty les 1; LEmpty]; 
           LSub [:: LEmpty; LEmpty; LEmpty; LEmpty; LEmpty; nth LEmpty les' 0; nth LEmpty les' 1]])])
| LT_imul2, les, les' => ([:: Lopn (LSub [:: LSub [:: nth LEmpty les 1]; LSub [:: LEmpty]])] ++
 [:: Lopn (LSub [:: LSub [:: nth LEmpty les 0; LEmpty]; 
           LSub [:: LEmpty; LEmpty; LEmpty; LEmpty; LEmpty; nth LEmpty les' 0; nth LEmpty les' 1]])])
| LT_imul3, les, les' => [:: Lopn (LSub [:: LSub les; 
                                         LSub [:: LEmpty; LEmpty; LEmpty; LEmpty; LEmpty; nth LEmpty les' 0; nth LEmpty les' 1]])]
| LT_imul4, les, les' => [:: Lopn (LSub [:: LSub les ; LSub les'])]
| LT_iemptysl, _, _ => [::]
end.

Section Leak_I.

  Variable leak_I : pointer -> leak_i -> leak_i_tr -> seq leak_i.

  Definition leak_Is (stk : pointer) (lts : seq leak_i_tr) (ls : seq leak_i) : seq leak_i :=
    flatten (map2 (leak_I stk) ls lts).

  Definition leak_Iss (stk: pointer) (ltss : seq leak_i_tr) (ls : seq (seq leak_i)) : seq (seq leak_i) :=
    (map (leak_Is stk ltss) ls). 

End Leak_I.

Section Leak_Call.

Variable leak_Fun : funname -> seq leak_i_tr.

Definition dummy_lit := Lopn LEmpty.

Definition leak_assgn := 
(Lopn (LSub [:: LEmpty ; LEmpty])).

Definition get_empty_leak_seq (l : seq leak_e_tr) :=
(map (fun x => LEmpty) l).

Fixpoint leak_I (stk:pointer) (l : leak_i) (lt : leak_i_tr) {struct l} : seq leak_i :=
  match lt, l with
  | LT_iremove, _ => [::]
  | LT_ikeep, _ => [::l]
  | LT_ile lte, Lopn le   => [:: Lopn (leak_E stk lte le) ]
  | LT_icond lte ltt ltf, Lcond le b lti => 
    [:: Lcond (leak_E stk lte le) b (leak_Is leak_I stk (if b then ltt else ltf) lti) ]
  | LT_iwhile ltis lte ltis', Lwhile_true lts le lts' lw => 
    [:: Lwhile_true (leak_Is leak_I stk ltis lts)
                     (leak_E stk lte le)
                     (leak_Is leak_I stk ltis' lts')
                     (head dummy_lit (leak_I stk lw lt))]
  | LT_iwhile ltis lte ltis', Lwhile_false lts le => 
    [::Lwhile_false (leak_Is leak_I stk ltis lts)
                     (leak_E stk lte le)]
  | LT_icond_eval lts, Lcond _ _ lti => 
    leak_Is leak_I stk lts lti
  | LT_icond_eval lts, Lwhile_false lti le =>
    leak_Is leak_I stk lts lti
  | LT_ifor lte ltiss, Lfor le ltss => [:: Lfor (leak_E stk lte le)
                                                (leak_Iss leak_I stk ltiss ltss) ]
  | LT_icall lte lte', Lcall le (f, lts) le' => [:: Lcall (leak_E stk lte le)
                                                          (f, (leak_Is leak_I stk (leak_Fun f) lts))
                                                          (leak_E stk lte' le') ]
  | LT_ifor_unroll ltiss, Lfor le ltss => 
    flatten (map (fun l => leak_assgn :: l) (leak_Iss leak_I stk ltiss ltss))
  | LT_icall_inline lc ltc', Lcall le (f, lts) le' => 
    (map (fun x => (Lopn (LSub [:: x; LEmpty]))) (get_seq_leak_e le) ++ 
     lc ++
     leak_Is leak_I stk (leak_Fun f) lts ++
    (map (fun y => (Lopn (LSub [:: LEmpty; y]))) (get_seq_leak_e le')))
 (* lowering *)
  | LT_icondl lti' lte ltt ltf, Lcond le b lti => 
     (leak_EI stk lti' le) ++ [:: Lcond (leak_E stk lte le) b (leak_Is leak_I stk (if b then ltt else ltf) lti) ]
  | LT_iwhilel lti lte ltis ltis', Lwhile_true lts le lts' lw => 
    [:: Lwhile_true ((leak_Is leak_I stk ltis lts) ++ (leak_EI stk lti le))
                     (leak_E stk lte le)
                     (leak_Is leak_I stk ltis' lts')
                     (head dummy_lit (leak_I stk lw lt))]
  | LT_iwhilel lti lte ltis ltis', Lwhile_false lts le => 
    [::Lwhile_false ((leak_Is leak_I stk ltis lts) ++ (leak_EI stk lti le)) (leak_E stk lte le)]
  | LT_icopn ltes, Lopn le => leak_ESI stk ltes (get_seq_leak_e (leak_E stk (LT_subi 0) le)) (get_seq_leak_e (leak_E stk (LT_subi 1) le))
  | LT_ilmov1, Lopn le => [:: Lopn (LSub [:: LSub [:: leak_E stk (LT_subi 0) le]; LSub [:: LEmpty]]) ; 
                                       Lopn (LSub [:: LSub [:: LEmpty]; LSub [:: leak_E stk (LT_subi 1) le]])]
  | LT_ilmov2, Lopn le => [:: Lopn (LSub [:: LSub [::];
                                        LSub[:: LEmpty; LEmpty; LEmpty; LEmpty; LEmpty; leak_E stk (LT_subi 1) le]])]
  | LT_ilmov3, Lopn le => [:: Lopn (LSub [:: LSub [::]; LSub [:: leak_E stk (LT_subi 1) le]])]
  | LT_ilmov4, Lopn le => [:: Lopn (LSub [:: LSub [:: leak_E stk (LT_subi 0) le]; 
                                             LSub [:: leak_E stk (LT_subi 1) le]])] 
  | LT_ilinc ltes, Lopn le => [:: Lopn (LSub [:: LSub (leak_ES stk ltes (leak_E stk (LT_subi 0) le)); 
                                            LSub [:: LEmpty; LEmpty; LEmpty; LEmpty; leak_E stk (LT_subi 1) le]])]
  | LT_ilcopn ltes, Lopn le => [:: Lopn (LSub [:: LSub (leak_ES stk ltes (leak_E stk (LT_subi 0) le));
                                                       LSub [:: leak_E stk (LT_subi 1) le]])]
  | LT_ild, Lopn le => [:: Lopn (LSub [:: LSub[:: LEmpty]; 
                                      LSub [:: LEmpty; LEmpty; LEmpty; LEmpty; leak_E stk (LT_subi 1) le]])]
  | LT_ildc, Lopn le => [:: Lopn (LSub [:: LSub[:: LEmpty; LEmpty]; 
                                       LSub [:: LEmpty; LEmpty; LEmpty; LEmpty; LEmpty; leak_E stk (LT_subi 1) le]])]
  | LT_ildcn, Lopn le => [:: Lopn (LSub [:: LSub [:: LEmpty]; LSub [:: LEmpty]]);
                             Lopn (LSub [:: LSub [:: LEmpty; LEmpty]; 
                                            LSub [:: LEmpty; LEmpty; LEmpty; LEmpty; LEmpty; leak_E stk (LT_subi 1) le]])]
  | LT_ileq ltes, Lopn le => [:: Lopn (LSub [:: LSub (leak_ES stk ltes (leak_E stk (LT_subi 0) le));
                                            LSub [:: LEmpty; LEmpty; LEmpty; LEmpty; leak_E stk (LT_subi 1) le]])]
  | LT_illt ltes, Lopn le => [:: Lopn (LSub [:: LSub (leak_ES stk ltes (leak_E stk (LT_subi 0) le));
                                      LSub [:: LEmpty; leak_E stk (LT_subi 1) le; LEmpty; LEmpty; LEmpty]])]
  | LT_ilif lti le', Lopn le => 
    ((leak_EI stk lti (get_nth_leak (get_seq_leak_e (leak_E stk (LT_subi 0) le)) 0)) ++ 
    [:: Lopn (LSub [:: LSub [:: leak_E stk le' (leak_E stk (LT_subi 0) (leak_E stk (LT_subi 0) le));
                            (leak_E stk (LT_subi 1) (leak_E stk (LT_subi 0) le));
                (leak_E stk (LT_subi 2) (leak_E stk (LT_subi 0) le))]; LSub [:: leak_E stk (LT_subi 1) le]])])
  | LT_ilea, Lopn le => [:: Lopn (LSub [:: LSub [:: leak_lea_exp]; LSub [:: leak_E stk (LT_subi 1) le]])]
  | LT_ilsc, Lopn le => [:: Lopn (LSub [:: leak_E stk (LT_subi 1) (leak_E stk (LT_subi 1) leak_lea_exp); 
                                       LSub [:: LEmpty; LEmpty; LEmpty; LEmpty; LEmpty; leak_E stk (LT_subi 1) le]])]
  | LT_ilmul lest ltes, Lopn le =>  (leak_ESI stk lest (get_seq_leak_e (leak_E stk ltes (LSub [:: LEmpty; LEmpty])))
                                              [:: LEmpty; LEmpty; LEmpty; LEmpty; LEmpty; leak_E stk (LT_subi 1) le])
  | LT_ilfopn lest lte, Lopn le => (leak_ESI stk lest (leak_ES stk lte (leak_E stk (LT_subi 0) le)) 
                                             [:: LEmpty; LEmpty; LEmpty; LEmpty; LEmpty; leak_E stk (LT_subi 1) le])
  | LT_ilds, Lopn le => [:: Lopn (LSub [:: LSub [:: nth LEmpty (get_seq_leak_e (leak_E stk (LT_subi 0) le)) 0]; LSub[:: LEmpty]])]
  | LT_ildus, Lopn le => [:: Lopn (LSub [:: LSub [:: LEmpty]; LSub[:: LEmpty]])]
  | LT_ildiv lti ltes, Lopn le =>  if eq_leak_i_tr lti LT_ilds 
    then [:: Lopn (LSub [:: LSub [:: nth LEmpty (get_seq_leak_e (leak_E stk (LT_subi 0) le)) 0]; LSub[:: LEmpty]])] ++ 
[:: Lopn (LSub [:: LSub [:: LEmpty; nth LEmpty (get_seq_leak_e (leak_E stk (LT_subi 0) le)) 0; nth LEmpty (get_seq_leak_e (leak_E stk (LT_subi 0) le)) 1]; LSub (leak_ES stk ltes (leak_E stk (LT_subi 1) le))])]
    else [:: Lopn (LSub [:: LSub [:: LEmpty]; LSub[:: LEmpty]])]  ++ [:: Lopn (LSub [:: LSub [:: LEmpty; nth LEmpty (get_seq_leak_e (leak_E stk (LT_subi 0) le)) 0; nth LEmpty (get_seq_leak_e (leak_E stk (LT_subi 0) le)) 1]; LSub (leak_ES stk ltes (leak_E stk (LT_subi 1) le))])]
                                 
    
  | LT_ilasgn, Lopn le=>  [:: Lopn (LSub [:: (leak_E stk (LT_subi 0) le);
                                                        leak_E stk (LT_subi 1) le])]

 (*| LT_icompose lt1 lt2 => leak_I (leak_I l lt1) lt2*) (* le = LSub [:: l; l'] *)
                                            
  | _, _ => [:: l]
  end.

End Leak_Call.

Notation leak_c_tr := (seq leak_i_tr).

Definition leak_f_tr := seq (funname * leak_c_tr).

Section Leak_Call_Imp.

Variable Fs: leak_f_tr.

Definition leak_Fun (f: funname) : leak_c_tr := odflt [::] (assoc Fs f).

End Leak_Call_Imp.

Fixpoint leak_compile (stk : pointer) (lts: seq leak_f_tr) (lf: leak_fun) := 
match lts with 
| [::] => lf.2
| x :: xs => let r := (leak_Is (leak_I (leak_Fun x)) stk (leak_Fun x lf.1)
                   lf.2) in 
             leak_compile stk xs (lf.1, r)
end.

Fixpoint leak_compiles (stk: pointer) (ltss: seq (seq leak_f_tr)) (lf: leak_fun) :=
match ltss with 
| [::] => lf.2
| x :: xs => let r := leak_compile stk x lf in 
              leak_compiles stk xs (lf.1, r)
end.

(** Leakage for intermediate-level **)

Inductive leak_il : Type :=
| Lempty : leak_il
| Lopnl : leak_e -> leak_il
| Lcondl : leak_e -> bool -> leak_il.

Notation leak_funl := (funname * seq leak_il).

Definition leak_cl := seq leak_il.

Inductive leak_i_il_tr : Type :=
| LT_ilremove : leak_i_il_tr
| LT_ilkeep : leak_i_il_tr
| LT_ilkeepa : leak_i_il_tr
| LT_ilcond_0 : leak_e_tr -> seq leak_i_il_tr -> leak_i_il_tr (*c1 is empty*)
| LT_ilcond_0' : leak_e_tr -> seq leak_i_il_tr -> leak_i_il_tr (*c2 is empty*)
| LT_ilcond : leak_e_tr -> seq leak_i_il_tr -> seq leak_i_il_tr -> leak_i_il_tr (* c1 and c2 are not empty *)
| LT_ilwhile_c'0 : align -> seq leak_i_il_tr -> leak_i_il_tr
| LT_ilwhile_f : seq leak_i_il_tr -> leak_i_il_tr
| LT_ilwhile : seq leak_i_il_tr -> seq leak_i_il_tr -> leak_i_il_tr.

Section Leak_IL.

  Variable leak_i_iL : pointer -> leak_i ->  leak_i_il_tr -> seq leak_il.

  Definition leak_i_iLs (stk : pointer) (lts : seq leak_i_il_tr) (ls : seq leak_i) : seq leak_il :=
    flatten (map2 (leak_i_iL stk) ls lts).

  Fixpoint ilwhile_c'0 (stk: pointer) (lti : seq leak_i_il_tr) (li : leak_i) : seq leak_il :=
    match li with 
      | Lwhile_false lis le => leak_i_iLs stk lti lis ++ [:: Lcondl le false]
      | Lwhile_true lis le lis' li' => leak_i_iLs stk lti lis ++ [:: Lcondl le true] ++
                                       ilwhile_c'0 stk lti li'
      | _ => [::]
    end.

  Fixpoint ilwhile (stk : pointer) (lts : seq leak_i_il_tr) (lts' : seq leak_i_il_tr) (li : leak_i) : seq leak_il :=
    match li with 
      | Lwhile_false lis le => leak_i_iLs stk lts lis ++ [:: Lcondl le false]
      | Lwhile_true lis le lis' li' => 
        leak_i_iLs stk lts lis ++ [:: Lcondl le true] ++ leak_i_iLs stk lts' lis' ++ [:: Lempty] ++ ilwhile stk lts lts' li'
      | _ => [::]
    end.

End Leak_IL.

(* Computes the leakage depending on alignment *) 
Definition get_align_leak_il a : seq leak_il :=
  match a with 
  | NoAlign => [::]
  | Align => [:: Lempty]
  end.

Fixpoint leak_i_iL (stk:pointer) (li : leak_i) (l : leak_i_il_tr) {struct li} : seq leak_il :=
match l, li with 
| LT_ilremove, _ => [:: Lempty]
| LT_ilkeepa, Lopn le => [:: Lopnl (LSub (map (fun x => LSub [:: x]) (get_seq_leak_e le)))]
| LT_ilkeep, Lopn le => [:: Lopnl le]
| LT_ilcond_0 lte lti, Lcond le b lis => [:: Lcondl (leak_E stk lte le) b] ++ 
  (if b then [::] else leak_i_iLs leak_i_iL stk lti lis ++ [:: Lempty])
| LT_ilcond_0' lte lti, Lcond le b lis => [:: Lcondl (leak_E stk lte le) (negb b)] ++ 
  (if (negb b) then [::] else leak_i_iLs leak_i_iL stk lti lis ++ [:: Lempty])
| LT_ilcond lte lti lti', Lcond le b lis => 
  [:: Lcondl (leak_E stk lte le) b] ++ if b then leak_i_iLs leak_i_iL stk lti lis ++ [:: Lempty]
                           else leak_i_iLs leak_i_iL stk lti' lis ++ [:: Lempty]
| LT_ilwhile_c'0 a lti, _ => get_align_leak_il a ++ [:: Lempty & ilwhile_c'0 leak_i_iL stk lti li]
| LT_ilwhile_f lti, Lwhile_false lis le => leak_i_iLs leak_i_iL stk lti lis
| LT_ilwhile lti lti', _ => [:: Lempty] ++ ilwhile leak_i_iL stk lti lti' li 
| _, _ => [::]
end.

Definition leak_f_lf_tr := seq (funname * seq leak_i_il_tr).

Section Leak_Call_Imp_L.

Variable Fs: leak_f_lf_tr.

Definition leak_Fun_L (f: funname) : seq leak_i_il_tr := odflt [::] (assoc Fs f).

End Leak_Call_Imp_L.

(** Leakage for assembly-level **)

Inductive leak_asm : Type :=
| Laempty
| Lacond of bool (* bool represents the condition in conditional jump *)
| Laop of seq pointer.

(* Extracts the sequence of pointers from leak_e *)
Fixpoint leak_e_asm (l : leak_e) : seq pointer :=
match l with 
| LEmpty => [::]
| LIdx i => [::]
| LAdr p => [:: p]
| LSub l => flatten (map leak_e_asm l)
end.

(* Transforms leakage for intermediate langauge to leakage for assembly *)
Definition leak_i_asm (l : leak_il) : leak_asm :=
match l with 
| Lempty => Laempty
| Lopnl le => Laop (leak_e_asm le)
| Lcondl le b => Lacond b
end.

Definition leak_compile_prog (stk: pointer) (ltss: leak_f_lf_tr * (seq leak_f_tr * (seq (seq leak_f_tr) * seq leak_f_tr))) (lf: leak_fun) : seq leak_il :=
let r := leak_compile stk ltss.2.1 lf in 
let rs := leak_compiles stk ltss.2.2.1 (lf.1, r) in 
leak_i_iLs leak_i_iL stk (leak_Fun_L ltss.1 lf.1) (leak_compile stk ltss.2.2.2 (lf.1, rs)). 

Definition leak_compile_x86 (stk: pointer) (ltss: leak_f_lf_tr * (seq leak_f_tr * (seq (seq leak_f_tr) * seq leak_f_tr))) (lf: leak_fun) : seq leak_asm := 
let r := leak_compile_prog stk ltss lf in
map (fun x=> leak_i_asm x) r.














