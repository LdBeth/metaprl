(*
 * Use the appropriate SEQ_SET module as specified in mk/config
 *)
IFDEF SEQ_SET_Lm_array_linear_set THEN
   type 'a linear_set = 'a Lm_array_linear_set.linear_set
   module Make = Lm_array_linear_set.Make
ELSE IFDEF SEQ_SET_Lm_splay_linear_set THEN
   type 'a linear_set = 'a Lm_splay_linear_set.linear_set
   module Make = Lm_splay_linear_set.Make
ENDIF ENDIF
