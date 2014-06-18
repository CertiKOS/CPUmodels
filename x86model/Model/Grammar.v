Require Import List.

Require Import ParserArg.
Import X86_PARSER_ARG.
Require Export Xform.

Require Import CommonTacs.
Set Implicit Arguments.

(** Our user-facing [grammar]s, indexed by a [type], reflecting the type of the
    semantic value returned by the grammar when used in parsing. *)
Inductive grammar : type -> Type := 
| gEps : grammar Unit_t
| gZero : forall t, grammar t
| gChar : char_p -> grammar Char_t
| gAny : grammar Char_t
| gCat : forall t1 t2, grammar t1 -> grammar t2 -> grammar (Pair_t t1 t2)
| gAlt : forall t1 t2, grammar t1 -> grammar t2 -> grammar (Sum_t t1 t2)
| gStar : forall t, grammar t -> grammar (List_t t)
| gMap : forall t1 t2, (interp t1 -> interp t2) -> grammar t1 -> grammar t2
| gXform : forall t1 t2, t1 ->> t2 -> grammar t1 -> grammar t2.
Extraction Implicit gZero [t].
Extraction Implicit gCat [t1 t2].
Extraction Implicit gAlt [t1 t2].
Extraction Implicit gStar [t].
Extraction Implicit gMap [t1 t2].
Extraction Implicit gXform [t1 t2].

(** * Denotation of Grammars *)
(** I'm a little annoyed that I had to break out so many equalities, but
    found this worked a little better for both inversion and proving. *)
Inductive in_grammar : forall t, grammar t -> list char_p -> (interp t) -> Prop := 
| IngEps : forall s v, s = nil -> v = tt -> in_grammar gEps s v
| IngChar : forall c s v, s = c::nil -> v = c -> in_grammar (gChar c) s v
| IngAny : forall c s v, s = c::nil -> v = c -> in_grammar gAny s v
| IngCat : forall t1 t2 (g1:grammar t1) (g2:grammar t2) s1 s2 v1 v2 s v, 
    in_grammar g1 s1 v1 -> in_grammar g2 s2 v2 -> 
    s = s1 ++ s2 -> v = (v1,v2) -> in_grammar (gCat g1 g2) s v
| IngAlt_l : forall t1 t2 (g1:grammar t1) (g2:grammar t2) s v1 v, 
    in_grammar g1 s v1 -> v = inl _ v1 -> in_grammar (gAlt g1 g2) s v
| IngAlt_r : forall t1 t2 (g1:grammar t1) (g2:grammar t2) s v2 v, 
    in_grammar g2 s v2 -> v = inr _ v2 -> in_grammar (gAlt g1 g2) s v
| IngStar_eps : forall t (g:grammar t) s v, s = nil -> v = nil ->
                                           in_grammar (gStar g) s v
| IngStar_cons : forall t (g:grammar t) s1 v1 s2 v2 s v, 
    in_grammar g s1 v1 -> in_grammar (gStar g) s2 v2 -> 
    s1 <> nil -> s = s1 ++ s2 -> v = v1::v2 -> in_grammar (gStar g) s v
| IngMap : forall t1 t2 (f:interp t1 -> interp t2) (g:grammar t1) s v1 v2, 
    in_grammar g s v1 -> v2 = f v1 -> in_grammar (@gMap t1 t2 f g) s v2
| IngXform : forall t1 t2 (f: t1 ->> t2) (g:grammar t1) s v1 v2,
    in_grammar g s v1 -> v2 = xinterp f v1 -> in_grammar (gXform f g) s v2.
Hint Constructors in_grammar.

(** * Optimizing constructors for grammars.  These try to reduce the
      grammar, but must make adjustments to the semantic actions.  We 
      use optimized transforms to get this effect. *)

(** g ++ 0 ==> g @ inl *)
Definition OptAlt_r t2 (g2:grammar t2) : 
  forall t1, grammar t1 -> grammar (Sum_t t1 t2) :=
  match g2 in grammar t2' return forall t1, grammar t1 -> grammar (Sum_t t1 t2') with
    | gZero t2 => fun t1 g1 => gXform Xinl g1
    | g2' => fun t1 g1 => gAlt g1 g2'
  end.
Extraction Implicit OptAlt_r [t2 t1].

(** 0 ++ g ==> g @ inr *)
Definition OptAlt_l t1 (g1:grammar t1) : 
  forall t2, grammar t2 -> grammar (Sum_t t1 t2) :=
  match g1 in grammar t1' return forall t2, grammar t2 -> grammar (Sum_t t1' t2) with
    | gZero t1 => fun t2 g2 => gXform Xinr g2
    | g1' => fun t2 g2 => OptAlt_r g2 g1'
  end.
Extraction Implicit OptAlt_l [t1 t2].

(** We would like to reduce (g ++ g) ==> g but this loses information and
    turns a potentially ambiguous grammar into one that is not.  More 
    importantly, we can't actually compare grammars for equality because
    we are trying to keep the [type] index computationally irrelevant.
*)
Definition OptAlt t1 t2 (g1:grammar t1) (g2:grammar t2) := OptAlt_l g1 g2.
Extraction Implicit OptAlt [t1 t2].

(** g $ 0 ==> 0 @ zero_to_t
    g $ eps ==> g @ add_unit_r *)
Definition OptCat_r t2 (g2:grammar t2) : forall t1, grammar t1 -> grammar (Pair_t t1 t2) :=
  match g2 in grammar t2' return forall t1, grammar t1 -> grammar (Pair_t t1 t2') with
    | gZero t2' => fun t1 (g2 : grammar t1) => gZero _
    | gEps => fun t1 (g1 : grammar t1) => gXform (Xpair Xid Xunit) g1
    | g2' => fun t1 (g1 : grammar t1) => gCat g1 g2'
  end.
Extraction Implicit OptCat_r [t2 t1].

(** 0 $ g ==> 0 @ zero_to_t
    eps $ g ==> g @ add_unit_l *)
Definition OptCat t1 (g1:grammar t1) : 
  forall t2, grammar t2 -> grammar (Pair_t t1 t2) :=
  match g1 in grammar t1' return forall t2, grammar t2 -> grammar (Pair_t t1' t2) with
    | gZero t1' => fun t2 (g2 : grammar t2) => gZero _
    | gEps => fun t2 (g2 : grammar t2) => gXform (Xpair Xunit Xid) g2
    | g1' => fun t2 (g2 : grammar t2) => OptCat_r g2 g1'
  end.
Extraction Implicit OptCat [t1 t2].

(** star (star g) ==> (star g) @ mklist
    star eps ==> eps @ to_empty_list
    star 0 ==> eps @ to_empty_list 
*)
Definition OptStar t (g:grammar t) : grammar (List_t t) := 
  match g in grammar t' return grammar (List_t t') with
    | gStar u g' => gXform (Xcons Xid Xempty) (gStar g')
    | gEps => gXform Xempty gEps
    | gZero t => gXform Xempty gEps
    | g' => gStar g'
  end.
Extraction Implicit OptStar [t].

(** 0 @ f ==> 0
    g @ f1 @ f2 ==> g @ (f1 o f2)
*)
Definition OptMap' t1 (g:grammar t1) : 
  forall t2, (interp t1 -> interp t2) -> grammar t2 := 
  match g in grammar t1' return forall t2, (interp t1' -> interp t2) -> grammar t2 with
    | gZero t => fun t2 f => gZero t2
    | gMap u1 u2 f' g' => fun t2 f => @gMap u1 t2 (fun x => f (f' x)) g'
    | g' => fun t2 f => @gMap _ t2 f g'
  end.
Extraction Implicit OptMap' [t1 t2].

Definition OptMap t1 t2 (f:interp t1 -> interp t2) (g:grammar t1) : grammar t2 := 
  @OptMap' t1 g t2 f.
Extraction Implicit OptMap [t1 t2].

Definition OptXform' t1 (g:grammar t1) : forall t2, t1->>t2 -> grammar t2 :=
  match g in grammar t1' return forall t2, t1'->>t2 -> grammar t2 with
    | gZero t => fun t2 x => gZero t2
    | gXform u1 u2 x' g' => fun t2 x => gXform (xcomp x' x) g'
    | g' => fun t2 x => gXform x g'
  end.
Extraction Implicit OptXform' [t1 t2].

Definition OptXform t1 t2 (x:t1->>t2) (g:grammar t1) := @OptXform' t1 g t2 x.
Extraction Implicit OptXform [t1 t2].

(** Explicit inversion principles for the grammars -- needed because
    of typing dependencies, though a little awkward that we can't just
    use [dependent inversion] to solve them. *)
Lemma inv_gEps : forall cs v, in_grammar gEps cs v -> cs = nil /\ v = tt.
Proof. intros. inversion H ; crush. Qed.

Lemma inv_gAny : forall cs v, in_grammar gAny cs v -> cs = v::nil.
Proof. intros. inversion H ; crush. Qed.

Lemma inv_gChar : 
  forall c cs v, in_grammar (gChar c) cs v -> cs = c::nil /\ v = c.
Proof. intros. inversion H ; crush. Qed.

Lemma inv_gCat : forall t1 t2 (g1:grammar t1) (g2:grammar t2) cs v, 
  in_grammar (gCat g1 g2) cs v -> 
  exists cs1, exists cs2, exists v1, exists v2, 
    in_grammar g1 cs1 v1 /\ in_grammar g2 cs2 v2 /\ cs = cs1++cs2 /\ v = (v1,v2).
Proof. intros. inversion H ; crush. repeat econstructor ; eauto. Qed.

Lemma inv_gAlt : forall t1 t2 (g1:grammar t1) (g2:grammar t2) cs v, 
  in_grammar (gAlt g1 g2) cs v -> 
  (exists v1, in_grammar g1 cs v1 /\ v = inl _ v1) \/
  (exists v2, in_grammar g2 cs v2 /\ v = inr _ v2).
Proof. intros ; inversion H ; crush. Qed.

Lemma inv_gStar : forall t (g:grammar t) cs v, 
  in_grammar (gStar g) cs v -> (cs = nil /\ v = nil) \/ 
  (exists cs1, exists v1, exists cs2, exists v2, 
    cs1 <> nil /\ in_grammar g cs1 v1 /\ in_grammar (gStar g) cs2 v2 /\ 
    cs = cs1 ++ cs2 /\ v = v1::v2).
Proof.
  intros ; inversion H ; clear H ; crush ; right ; exists s1 ; exists v1 ; 
  exists s2 ; exists v2 ; auto.
Qed.

Lemma inv_gMap : forall t1 t2 (f:interp t1 -> interp t2) (g:grammar t1) cs v,
  in_grammar (@gMap t1 t2 f g) cs v -> exists v', in_grammar g cs v' /\ v = f v'.
Proof. intros ; inversion H ; crush. Qed.

Lemma inv_gZero : forall t cs v, in_grammar (gZero t) cs v -> False.
Proof. intros ; inversion H. Qed.

Lemma inv_gXform : forall t1 t2 (x:t1->>t2) (g:grammar t1) cs v,
  in_grammar (gXform x g) cs v -> exists v', in_grammar g cs v' /\ v = xinterp x v'.
Proof. intros ; inversion H ; crush. Qed.

Lemma in_cat_eps : forall t (g:grammar t) s v1 v, 
  in_grammar g s v1 -> v = (v1,tt) -> in_grammar (gCat g gEps) s v.
Proof. intros ; econstructor ; eauto. apply app_nil_end. Qed.
Hint Resolve in_cat_eps.

Ltac local_simpl := 
  simpl in * ; intros ; 
    repeat 
      match goal with 
        | [ |- _ /\ _ ] => split
        | [ H : _ /\ _ |- _ ] => destruct H
        | [ |- context[ _ ++ nil ] ] => rewrite <- app_nil_end
        | [ H : exists x, _ |- _ ] => destruct H
        | [ H : _ \/ _ |- _] => destruct H
        | [ H : _ <-> _ |- _] => destruct H
        | [ |- _ <-> _ ] => split
        | [ H : _::_ = _::_ |- _] => injection H ; clear H
        | _ => idtac
      end ; auto.

(** Tactic for invoking inversion principles on a proof that some string
    and value are in the denotation of a grammar.  We don't unroll the 
    [gStar] case because that would loop. *)
Ltac in_grammar_inv := 
    match goal with 
      | [ H : in_grammar gEps _ _ |- _ ] => generalize (inv_gEps H) ; clear H
      | [ H : in_grammar gAny _ _ |- _ ] => generalize (inv_gAny H) ; clear H
      | [ H : in_grammar (gChar _) _ _ |- _ ] => generalize (inv_gChar H) ; clear H
      | [ H : in_grammar (gAlt _ _) _ _ |- _ ] => generalize (inv_gAlt H) ; clear H
      | [ H : in_grammar (gCat _ _) _ _ |- _ ] => generalize (inv_gCat H) ; clear H
      | [ H : in_grammar (gZero _) _ _ |- _ ] => contradiction (inv_gZero H)
      | [ H : in_grammar (gMap _ _ _) _ _ |- _ ] => generalize (inv_gMap H) ; clear H
      | [ H : in_grammar (gXform _ _) _ _ |- _ ] => 
        generalize (inv_gXform H) ; clear H
      | _ => local_simpl ; subst ; eauto
    end.

(** Correctness proofs for the optimizing grammar constructors. *)
Lemma opt_alt_corr : forall t1 t2 (g1:grammar t1) (g2:grammar t2) s v, 
    in_grammar (gAlt g1 g2) s v <-> in_grammar (OptAlt g1 g2) s v.
Proof. destruct g1 ; destruct g2; simpl; crush; repeat in_grammar_inv. Qed.

Lemma opt_cat_corr : forall t1 t2 (g1:grammar t1) (g2:grammar t2) s v,
  in_grammar (gCat g1 g2) s v <-> in_grammar (OptCat g1 g2) s v.
Proof.
  destruct g1 ; destruct g2 ; simpl ; try tauto ; repeat local_simpl;
    repeat in_grammar_inv.
Qed.

Lemma opt_map_corr : forall t1 t2 (f:interp t1 -> interp t2) (g:grammar t1) s v,
  in_grammar (@gMap t1 t2 f g) s v <-> in_grammar (@OptMap t1 t2 f g) s v.
Proof.
  destruct g ; simpl ; try tauto ; repeat local_simpl; repeat in_grammar_inv.
Qed.

Lemma opt_xform_corr : forall t1 t2 (x:t1->>t2) (g:grammar t1) s v,
  in_grammar (gXform x g) s v <-> in_grammar (OptXform x g) s v.
Proof.
  destruct g ; simpl ; try tauto ; repeat local_simpl ; repeat in_grammar_inv ;
  eapply IngXform ; eauto ; rewrite xcomp_corr ;auto.
Qed.

(* (** Conceptually, returns [Eps] if [g] accepts the empty string, and  *)
(*     [Zero] otherwise.  In practice, we won't get exactly [Eps] since *)
(*     we could have [Map]s, [Xform]s, etc. in there. *) *)
(* Fixpoint null t (g:grammar t) : grammar t :=  *)
(*   match g in grammar t' return grammar t' with *)
(*     | Zero t => Zero t *)
(*     | Eps => Eps *)
(*     | Any => Zero _ *)
(*     | Char c => Zero _ *)
(*     | Alt t1 t2 g1 g2 => OptAlt (null g1) (null g2) *)
(*     | Cat t1 t2 g1 g2 => OptCat (null g1) (null g2) *)
(*     | Map t1 t2 f g => OptMap t2 f (null g) *)
(*     | Xform t1 t2 x g => OptXform x (null g) *)
(*     | Star t g => Xform (Xempty _ _) Eps *)
(*   end. *)
(* Extraction Implicit null [t]. *)

(* (** Computes the derivative of [g] with respect to [c]. Denotationally, *)
(*     this is { (s,v) | (c::s,v) in_grammar[g] }. *) *)
(* Fixpoint deriv t (g:grammar t) (c:char_p) : grammar t :=  *)
(*   match g in grammar t' return grammar t' with *)
(*     | Zero t => Zero t *)
(*     | Eps => Zero Unit_t *)
(*     | Char c' => if char_dec c c' then Xform (Xchar _ c') Eps else Zero _ *)
(*     | Any => Xform (Xchar _ c) Eps *)
(*     | Alt t1 t2 g1 g2 => OptAlt (deriv g1 c) (deriv g2 c) *)
(*     | Map t1 t2 f g => OptMap t2 f (deriv g c) *)
(*     | Xform t1 t2 x g => OptXform x (deriv g c) *)
(*     | Cat t1 t2 g1 g2 =>  *)
(*         OptXform (Xmatch (Xid _) (Xid _)) *)
(*           (OptAlt (OptCat (deriv g1 c) g2) (OptCat (null g1) (deriv g2 c))) *)
(*     | Star t g =>  *)
(*         OptXform (Xcons (Xfst _ _) (Xsnd _ _)) (OptCat (deriv g c) (Star g)) *)
(*   end. *)
(* Extraction Implicit deriv [t]. *)

