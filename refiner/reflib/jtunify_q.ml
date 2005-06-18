open Refiner.Refiner.TermType

open Jlogic_sig

open Jtunify
open Jordering

let jprover_bug = Invalid_argument "Jprover bug (Jall module)"

module JTUnify_Q (JLogic : JLogicSig) =
struct

   module JOrdering = JOrdering(JLogic)
   open JOrdering

let rec combine ov oslist = function
   [] -> [],[]
 | ((v, slist) as f) :: r ->
      let com_element = com_subst ov oslist slist in
      let rest_vlist,rest_combine = combine ov oslist r in
         if com_element == slist then
            (rest_vlist,(f::rest_combine))
         else
            (v::rest_vlist),((v,com_element)::rest_combine)

let compose (n,subst) ((ov,oslist) as one_subst) =
   let trans_vars,com = combine ov oslist subst in
(* begin
   print_endline "!!!!!!!!!test print!!!!!!!!!!";
   print_subst [one_subst];
   print_subst subst;
   print_endline "!!!!!!!!! END test print!!!!!!!!!!";
*)
   if List.mem one_subst subst then
      (trans_vars,(n,com))
   else
(* ov may multiply as variable in subst with DIFFERENT values *)
(* in order to avoid explicit atom instances!!! *)
      (trans_vars,(n,(com @ [one_subst])))
(* end *)

let rec apply_element v slist fs ft =
   match (fs,ft) with
      [], [] ->
         [], []
    | [], (ft_first::ft_rest) ->
         let _, ft = apply_element v slist [] ft_rest in
         [], (if ft_first = v then slist @ ft else ft_first :: ft)
    | ((fs_first::fs_rest),[]) ->
         let fs, _ = apply_element v slist fs_rest [] in
         (if fs_first = v then slist @ fs else fs_first :: fs), []
    | ((fs_first::fs_rest),(ft_first::ft_rest)) ->
         let fs, ft = apply_element v slist fs_rest ft_rest in
         (if fs_first = v then slist @ fs else fs_first :: fs),
         (if ft_first = v then slist @ ft else ft_first :: ft)

let rec shorten us ut =
   match (us,ut) with
      (fs::rs), (ft::rt) when fs = ft ->
         shorten rs rt
    | usut ->
         usut

let rec apply_subst_list eq_rest v slist =
   match eq_rest with
      [] ->
         []
    | (atomnames,(fs,ft))::r ->
         let (n_fs,n_ft) = apply_element v slist fs ft in
         let (new_fs,new_ft) = shorten n_fs n_ft in (* delete equal first elements *)
         match (new_fs,new_ft) with
            [],[] ->
               let new_eq_rest = apply_subst_list r v slist in
               ((atomnames,([],[]))::new_eq_rest)
          | [],(fft::_) ->
               if (is_const fft) then
                  raise Not_unifiable
               else
                  let new_eq_rest = apply_subst_list r v slist in
                  ((atomnames,([],new_ft))::new_eq_rest)
          | (ffs::rfs),[] ->
               if (is_const ffs) then
                  raise Not_unifiable
               else
                  let new_eq_rest = apply_subst_list r v slist in
                  ((atomnames,(new_fs,[]))::new_eq_rest)
          | (ffs::rfs),(fft::rft) ->
               if (is_const ffs) & (is_const fft) then
                  raise Not_unifiable
     (* different first constants cause local fail *)
               else
     (* at least one of firsts is a variable *)
                  let new_eq_rest = apply_subst_list r v slist in
                  ((atomnames,(new_fs,new_ft))::new_eq_rest)

let apply_subst eq_rest v slist atomnames =
   if (List.mem v atomnames) then (* don't apply subst to atom variables !! *)
      eq_rest
   else
      apply_subst_list eq_rest v slist

let all_variable_check eqlist = false   (* needs some discussion with Jens! -- NOT done *)

(*
let rec all_variable_check eqlist =
   match eqlist with
      [] -> true
    | ((_,(fs,ft))::rest_eq) ->
         if (fs <> []) & (ft <> []) then
            let fs_first = List.hd fs
            and ft_first = List.hd ft
            in
            if (is_const fs_first) or (is_const ft_first) then
               false
            else
               all_variable_check rest_eq
            else
               false
*)

let rec tunify_list eqlist init_sigma orderingQ atom_rel =
   let rec tunify atomnames fs ft rt rest_eq sigma ordering =
      let apply_r1 fs ft rt rest_eq sigma =
	  (* print_endline "r1"; *)
         tunify_list rest_eq sigma ordering atom_rel

      in
      let apply_r2 fs ft rt rest_eq sigma =
      (* print_endline "r2"; *)
         tunify atomnames rt fs ft rest_eq sigma ordering

      in
      let apply_r3 fs ft rt rest_eq sigma =
      (* print_endline "r3"; *)
         let rfs =  (List.tl fs)
         and rft =  (List.tl rt) in
         tunify atomnames rfs ft rft rest_eq sigma ordering

      in
      let apply_r4 fs ft rt rest_eq sigma =
      (* print_endline "r4"; *)
         tunify atomnames rt ft fs rest_eq sigma ordering

      in
      let apply_r5 fs ft rt rest_eq sigma =
      (* print_endline "r5"; *)
         let v = (List.hd fs) in
         let compose_vars, new_sigma = compose sigma (v,ft) in
         let new_rest_eq = apply_subst rest_eq v ft atomnames in
         let new_ordering = build_orderingJ (v::compose_vars) ft ordering atom_rel in
            tunify atomnames (List.tl fs) rt rt new_rest_eq new_sigma new_ordering



      in
      let apply_r6 fs ft rt rest_eq sigma =
      (* print_endline "r6"; *)
         let v = (List.hd fs) in
         let _, new_sigma = compose sigma (v,[]) in
         let new_rest_eq = apply_subst rest_eq v [] atomnames in
         (* no relation update since [] has been replaced for v *)
            tunify atomnames (List.tl fs) ft rt new_rest_eq new_sigma ordering









      in
      let apply_r7 fs ft rt rest_eq sigma =
      (* print_endline "r7"; *)
         let v = List.hd fs in
         let c1 = List.hd rt in
         let c2t =List.tl rt in
         let ft_c1 = ft @ [c1] in
         let compose_vars,new_sigma = compose sigma (v,ft_c1) in
         let new_rest_eq = apply_subst rest_eq v ft_c1 atomnames in
         let new_ordering = build_orderingJ (v::compose_vars) ft_c1 ordering atom_rel in
            tunify atomnames (List.tl fs) []  c2t new_rest_eq new_sigma new_ordering


      in
      let apply_r8 fs ft rt rest_eq sigma =
      (* print_endline "r8"; *)
         tunify atomnames rt [(List.hd fs)] (List.tl fs) rest_eq sigma ordering

      in
      let apply_r9 fs ft rt rest_eq (max,subst) =
      (* print_endline "r9"; *)
         let v = List.hd fs in
         let v_new = ("vnew"^(string_of_int max)) in
         let ft_vnew = ft @ [v_new] in
         let compose_vars,new_sigma = compose ((max+1),subst) (v,ft_vnew) in
         let new_rest_eq = apply_subst rest_eq v ft_vnew atomnames in
         let new_ordering = build_orderingJ (v::compose_vars) ft_vnew ordering atom_rel in
         tunify atomnames rt [v_new] (List.tl fs) new_rest_eq new_sigma new_ordering

      in
      let apply_r10 fs ft rt rest_eq sigma =
      (* print_endline "r10"; *)
         let x = List.hd rt in
         tunify atomnames fs (ft @ [x]) (List.tl rt) rest_eq sigma ordering

      in
      if r_1 fs ft rt then
         apply_r1 fs ft rt rest_eq sigma
      else if r_2 fs ft rt then
         apply_r2 fs ft rt rest_eq sigma
      else if r_3 fs ft rt then
         apply_r3 fs ft rt rest_eq sigma
      else if r_4 fs ft rt then
         apply_r4 fs ft rt rest_eq sigma
      else if r_5 fs rt then
         apply_r5 fs ft rt rest_eq sigma
      else if r_6 fs ft rt then
         (try
            apply_r6 fs ft rt rest_eq sigma
         with Not_unifiable ->
            if r_7 fs rt then (* r7 applicable if r6 was and tr6 = C2t' *)
               (try
                  apply_r7 fs ft rt rest_eq sigma
               with Not_unifiable ->
                  apply_r10 fs ft rt rest_eq sigma (* r10 always applicable if r6 was *)
               )
            else
      (* r10 could be represented only once if we would try it before r7.*)
      (* but looking at the transformation rules, r10 should be tried at last in any case *)
               apply_r10 fs ft rt rest_eq sigma  (* r10 always applicable r6 was *)
         )
      else if r_7 fs rt then  (* not r6 and r7 possible if z <> [] *)
         (try
            apply_r7 fs ft rt rest_eq sigma
         with Not_unifiable ->
            apply_r10 fs ft rt rest_eq sigma  (* r10 always applicable if r7 was *)
         )
      else if r_8 fs ft rt then
         (try
            apply_r8 fs ft rt rest_eq sigma
         with Not_unifiable ->
            if r_10 fs rt then (* r10 applicable if r8 was and tr8 <> [] *)
               apply_r10 fs ft rt rest_eq sigma
            else
               raise Not_unifiable (* simply back propagation *)
         )
      else if r_9 fs ft rt then
         (try
            apply_r9 fs ft rt rest_eq sigma
         with Not_unifiable ->
            if r_10 fs rt then (* r10 applicable if r9 was and tr9 <> [] *)
               apply_r10 fs ft rt rest_eq sigma
            else
               raise Not_unifiable (* simply back propagation *)
         )
      else if r_10 fs rt then  (* not ri, i<10, and r10 possible if for instance *)
                         (* (s=[] and x=v1) or (z<>[] and xt=C1V1t') *)
         apply_r10 fs ft rt rest_eq sigma
      else  (* NO rule applicable *)
         raise Not_unifiable
   in
   match eqlist with
      [] ->
         init_sigma,orderingQ
    | f::rest_eq ->
         begin
(*  open_box 0;
   print_equations [f];
   print_flush ();
*)
            let (atomnames,(fs,ft)) = f in
            tunify atomnames fs [] ft rest_eq init_sigma orderingQ
         end

let rec test_apply_eq atomnames eqs eqt subst =
   match subst with
      [] -> (eqs,eqt)
    | (f,flist)::r ->
         let (first_appl_eqs,first_appl_eqt) =
            if List.mem f atomnames then
               eqs, eqt
            else
               apply_element f flist eqs eqt
         in
         test_apply_eq atomnames first_appl_eqs first_appl_eqt r

let rec test_apply_eqsubst eqlist subst =
   match eqlist with
      [] -> []
    | (atomnames,(eqs,eqt))::r ->
         let applied_element = test_apply_eq atomnames eqs eqt subst in
         (atomnames,applied_element)::(test_apply_eqsubst r subst)

let ttest us ut ns nt eqlist orderingQ atom_rel =
   let (short_us,short_ut) = shorten us ut in (* apply intial rule R3 *)
                                           (* to eliminate common beginning *)
   let new_element = ([ns;nt],(short_us,short_ut)) in
   let full_eqlist =
      if List.mem new_element eqlist then
         eqlist
      else
         new_element::eqlist
   in
   let sigma, _ = tunify_list full_eqlist (1,[]) orderingQ atom_rel in
   let _, subst = sigma in
   let test_apply = test_apply_eqsubst full_eqlist subst in
   begin
      print_endline "";
      print_endline "Final equations:";
      print_equations full_eqlist;
      print_endline "";
      print_endline "Final substitution:";
      print_tunify sigma;
      print_endline "";
      print_endline "Applied equations:";
      print_equations test_apply
   end

let do_stringunify us ut ns nt equations fo_eqlist orderingQ atom_rel qmax =
   let (short_us,short_ut) = shorten us ut in (* apply intial rule R3 to eliminate common beginning *)
   let new_element = ([ns;nt],(short_us,short_ut)) in
   let full_eqlist =
      if List.mem new_element equations then
         equations @ fo_eqlist
      else
         (new_element::equations) @ fo_eqlist
   in
(*  print_equations full_eqlist; *)
   try
(* max-1 new variables have been used for the domain equations *)
      let new_sigma, new_ordering = tunify_list full_eqlist (1,[]) orderingQ atom_rel in
(* sigmaQ will not be returned in eqlist *)
      new_sigma, (qmax, full_eqlist), new_ordering
   with Not_unifiable ->
      raise Failed            (* new connection please *)

end
