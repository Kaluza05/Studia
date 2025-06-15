Require Import Utf8.
Require Import List.
Require Import Arith.
Import ListNotations.

Fixpoint insert (xs : list nat) (x : nat) : list nat :=
  match xs with
  | [] => [x]
  | y :: xs' =>
      if y <=? x then
        y :: insert xs' x
      else
        x :: xs
  end.

Fixpoint insert_sort (xs : list nat) := 
    match xs with
    | [] => []
    | x :: xs' => insert (insert_sort xs') x
    end.

Fixpoint sorted_non_desc (xs : list nat) : Prop :=
    match xs with
    | [] => True
    | x :: xs' => (forall y, In y xs' -> x <= y) /\ sorted_non_desc xs'
    end.

Lemma insert_spec1 (xs : list nat) (x : nat) (y : nat) :
    In y (insert xs x) -> y = x \/ In y xs.
Proof.
    induction xs. 
    +simpl. intro H. destruct H as [Heq | Hf].
        -rewrite Heq. tauto.
        -tauto.
    +simpl. intro H. destruct (a <=? x). simpl in H. destruct H as [Ha | Hrest]. tauto. 
    apply IHxs in Hrest. tauto.
    simpl in H. destruct H as [H1 | [H2 | H3]]. rewrite H1. tauto. rewrite H2. tauto. tauto.

Qed. 

Lemma insert_spec2 (xs : list nat) (x : nat) :
    sorted_non_desc xs -> sorted_non_desc (insert xs x).
Proof.
    induction xs;simpl.
    +tauto.
    +intro H. destruct H as [Hle Hsorted]. destruct (a <=? x) eqn:Hcmp.
     -apply Nat.leb_le in Hcmp. simpl. split. intros y R. apply insert_spec1 in R. destruct R as [Rl | Rr].
     rewrite Rl. assumption.
     apply Hle. assumption.
     apply IHxs. assumption.
     -apply Nat.leb_gt in Hcmp.
     simpl. split. intros y M. destruct M as [Ml | Mr]. rewrite <- Ml. apply Nat.lt_le_incl. assumption.
     assert (Ha_y := Hle y Mr).
     apply Nat.lt_le_incl.  
     apply Nat.lt_le_trans with (m := a).
     exact Hcmp.
     exact Ha_y.
     split. assumption. assumption.
Qed.


Theorem insert_sort_in_sorted (xs : list nat)  :
     sorted_non_desc (insert_sort xs).
Proof.
    induction xs.
    +simpl. tauto.
    +simpl. apply insert_spec2. assumption.
Qed.


Inductive perm {A : Type} : list A -> list A -> Prop :=
| perm_empty : perm [] []
| perm_extend : forall x xs ys,
    perm xs ys -> perm (x :: xs) (x :: ys)
| perm_transitivity : forall xs ys zs,
    perm xs ys -> perm ys zs -> perm xs zs
| perm_swap : forall x y xs, 
    perm (x :: y :: xs) (y :: x :: xs).


Lemma perm_reflex {A : Type} :
    forall (xs  : list A), perm xs xs.
Proof.
    induction xs.
    +exact perm_empty.
    +apply perm_extend. assumption.
Qed.
Lemma perm_trans {A : Type} :
    forall (xs ys zs : list A) , perm xs ys -> perm ys zs -> perm xs zs.
Proof.
    intros xs ys zs Hxy Hyz. 
    apply perm_transitivity with ys; assumption.
Qed.

Lemma perm_symm {A : Type} :
    forall (xs ys  : list A), perm xs ys -> perm ys xs.
Proof.
    intros xs ys H.
    induction H.
    +exact perm_empty.
    +apply perm_extend. assumption.
    +apply perm_transitivity with ys; assumption.
    +apply perm_swap.
Qed.

    
Lemma perm_spec1 (xs : list nat) (x :  nat) : 
    perm (x :: xs) (insert xs x).
Proof.
    induction xs.
    +simpl. apply perm_reflex.
    +simpl. destruct (a <=? x).
    apply perm_trans with (a :: x :: xs).
    apply perm_swap.
    apply perm_extend. assumption.
    -apply perm_reflex.
Qed.

Theorem insert_sort_perm_spec (xs : list nat)  :
    perm xs (insert_sort xs).
Proof.
    induction xs;simpl.
    + apply perm_reflex.
    + apply perm_trans with (a :: (insert_sort xs)).
    -apply perm_extend. assumption.
    -apply perm_spec1.
Qed.

(* Print perm_ind. *)