module ArrayTermCopy :
  functor(Hash : Simplehash_sig.SimpleHashSig) ->
  functor(FromTerm : Termmod_sig.TermModuleSig) ->
  functor(ToTerm : Termmod_sig.TermModuleSig) ->
sig
   type t

   val create : unit -> t
   val copy_term : t -> FromTerm.TermType.term -> ToTerm.TermType.term
   val copy_meta_term : t -> FromTerm.TermType.meta_term -> ToTerm.TermType.meta_term
   val copy_term_single : FromTerm.TermType.term -> ToTerm.TermType.term
   val copy_meta_term_single : FromTerm.TermType.meta_term -> ToTerm.TermType.meta_term
end

type normalize
type denormalize

val create_norm : unit -> normalize
val create_denorm : unit -> denormalize
val normalize_term : normalize -> Refiner_std_verb.Refiner.TermType.term -> Refiner.Refiner.TermType.term
val normalize_meta_term : normalize -> Refiner_std_verb.Refiner.TermType.meta_term -> Refiner.Refiner.TermType.meta_term
val denormalize_term : denormalize -> Refiner.Refiner.TermType.term -> Refiner_std_verb.Refiner.TermType.term
val denormalize_meta_term : denormalize -> Refiner.Refiner.TermType.meta_term -> Refiner_std_verb.Refiner.TermType.meta_term
val normalize_term_single : Refiner_std_verb.Refiner.TermType.term -> Refiner.Refiner.TermType.term
val normalize_meta_term_single : Refiner_std_verb.Refiner.TermType.meta_term -> Refiner.Refiner.TermType.meta_term
val denormalize_term_single : Refiner.Refiner.TermType.term -> Refiner_std_verb.Refiner.TermType.term
val denormalize_meta_term_single : Refiner.Refiner.TermType.meta_term -> Refiner_std_verb.Refiner.TermType.meta_term


