(*
 * Use the appropriate SEQ_SET module as specified in mk/config
 *)
IFDEF SEQ_SET_Lm_array_linear_set THEN
   type 'a linear_set = 'a Lm_array_linear_set.linear_set
   module Make = Lm_array_linear_set.Make
ELSIFDEF SEQ_SET_Lm_larray_linear_set THEN
   type 'a linear_set = 'a Lm_larray_linear_set.linear_set
   module Make = Lm_larray_linear_set.Make
END
