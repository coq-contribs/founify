Require Import is_in_quasiterm_term_subst.
Require Import term_unif.
Require Extraction.
Set Extraction AccessOpaque.
Extraction "unif.ml" Subst unif_proof.
