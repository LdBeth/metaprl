


open Refiner.Refiner
open Term
open TermOp
open TermType
open TermSubst
open Opname

open Jlogic_sig


module JProver (JLogic : JLogicSig) =

struct
type polarity = I | O

type connective = And | Or | Neg | Imp | All | Ex | At | Null


type ptype = Alpha | Beta | Gamma | Delta | Phi | Psi | PNull

type stype = Alpha_1 | Alpha_2 | Beta_1 | Beta_2 | Gamma_0 | Delta_0 | 
             Phi_0 | Psi_0 | PNull_0


type pos  = {name : string; 
	     address : int list;
             op :  connective;
             pol : polarity; 
             pt : ptype; 
             st : stype; 
             label : term}



type 'pos ftree = Empty | 
                  NodeAt of 'pos |
		  NodeA of 'pos * ('pos ftree) array;;



type atom  = {aname : string; 
	      aaddress : int list;
              aprefix : string array;
              apredicate :  operator;
              apol : polarity; 
              ast : stype;
              aarguments : bound_term list}




type atom_relations = atom * atom list * atom list
(* all atoms except atom occur in alpha_set and beta_set of atom*)                      











module OrderedAtom = 
 struct 
  type t = atom 
  let compare a1 a2 = if (a1.aname) = (a2.aname) then 0 else 
                       if (a1.aname) < (a2.aname) then -1 else 1
 end;;


module AtomSet = Set.Make(OrderedAtom);;


(*****************************************************************)



(************* printing function *************************************)




(************ printing T-string unifiers ****************************)


let rec list_to_string s = 
 match s with 
  [] -> "" 
 |f::r -> 
  f^"."^(list_to_string r)


let rec print_subst sigma = 
 match sigma with 
  [] -> 
   print_endline ""
 |f::r -> 
  let (v,s) = f in
   let ls = list_to_string s in 
  begin
   print_endline (v^" = "^ls); 
   print_subst r
  end


let print_tunify sigma = 
 let (n,subst) = sigma in 
 begin
  print_endline ("MaxVar = "^(string_of_int (n-1)));
  print_endline " ";
  print_endline "Substitution:";
  print_subst subst;
  print_endline " "
 end




(********* printing atoms and their relations ***********************)




let print_stype st = 
 match st with 
   Alpha_1 -> Format.print_string "Alpha_1"
  |Alpha_2 -> Format.print_string "Alpha_2"
  |Beta_1  -> Format.print_string "Beta_1" 
  |Beta_2  -> Format.print_string "Beta_2" 
  |Gamma_0 -> Format.print_string "Gamma_0" 
  |Delta_0 -> Format.print_string "Delta_0" 
  |Phi_0   -> Format.print_string "Phi_0" 
  |Psi_0   -> Format.print_string "Psi_0" 
  |PNull_0 -> Format.print_string "PNull_0" 




let print_pol pol = 
  if pol = O then 
    Format.print_string "O"
  else
    Format.print_string "I"



let rec print_address int_list = 
 match int_list with 
   [] ->   
    Format.print_string ""
  |hd::rest -> 
    begin 
     Format.print_int hd;
     print_address rest
    end 



let rec print_prefix prefix_list = 
  match prefix_list with 
    [] -> Format.print_string ""
   |f::r -> 
    begin
     Format.print_string f;
     print_prefix r
    end


let print_atom at tab = 
 let ({aname=x; aaddress=y; aprefix=z; apredicate=p; apol=a; ast=b; aarguments=tlist}) = at in 
  begin
   Format.print_string ("{aname="^x^"; address=");
   print_address y;
   Format.print_string "; ";
   Format.force_newline (); 
   Format.print_break (tab+1) (tab+1);
   Format.print_string "prefix=";
   print_prefix (Array.to_list z);
   Format.print_string "; predicate=<abstr>; ";
   Format.print_break (tab+1) (tab+1);
   Format.print_break (tab+1) (tab+1);
   Format.print_string "pol=";
   print_pol a;
   Format.print_string "; stype=";
   print_stype b;
   Format.print_string "; arguments=[<abstr>]";
   Format.print_string "}"  
  end



let rec print_atom_list set tab = 
   match set with 
    []  -> Format.print_string ""
   |(f::r) -> 
     begin 
      Format.force_newline ();
      Format.print_break (tab) (tab);
      print_atom f tab;
      print_atom_list r (tab)
     end


let rec print_atom_info atom_relation = 
  match atom_relation with 
    [] -> Format.print_string ""
   |(a,b,c)::r -> 
     begin
      Format.print_string "atom:";
      Format.force_newline ();
      Format.print_break 3 3;
      print_atom a 3;
      Format.force_newline ();
      Format.print_break 0 0;
      Format.print_string "alpha_set:";
      print_atom_list b 3;
      Format.force_newline ();
      Format.print_break 0 0;
      Format.print_string "beta_set:";
      print_atom_list c 3;
      Format.force_newline ();
      Format.force_newline ();
      Format.print_break 0 0;
      print_atom_info r
    end 




(*************** print formula tree, tree ordering etc. ***********)



let print_ptype pt = 
 match pt with 
   Alpha -> Format.print_string "Alpha"
  |Beta  -> Format.print_string "Beta" 
  |Gamma -> Format.print_string "Gamma" 
  |Delta -> Format.print_string "Delta" 
  |Phi   -> Format.print_string "Phi" 
  |Psi   -> Format.print_string "Psi" 
  |PNull -> Format.print_string "PNull" 




let print_op op = 
 match op with 
  At   -> Format.print_string "Atom"
 |Neg  -> Format.print_string "Neg"
 |And  -> Format.print_string "And"
 |Or   -> Format.print_string  "Or"
 |Imp  -> Format.print_string  "Imp"
 |Ex   -> Format.print_string "Ex"
 |All  -> Format.print_string "All"
 |Null -> Format.print_string "Null"


    

let print_position position tab = 
 let ({name=x; address=y; op=z; pol=a; pt=b; st=c; label=t}) = position in 
  begin
   Format.print_string ("{name="^x^"; address=");
   print_address y;
   Format.print_string "; ";
   Format.force_newline (); 
   Format.print_break (tab+1) 0;
(*   Format.print_break 0 3; *)
   Format.print_string "op=";
   print_op z;
   Format.print_string "; pol=";
   print_pol a;
   Format.print_string "; ptype=";
   print_ptype b;
   Format.print_string "; stype=";
   print_stype c;
   Format.print_string ";";
   Format.force_newline (); 
   Format.print_break (tab+1) 0;
   Format.print_string "label=";
   Format.print_break 0 0;
   Format.force_newline (); 
   Format.print_break tab 0;
   print_term stdout t;
   Format.print_string "}"  
  end
       






let rec pp_ftree_list tree_list tab = 

let rec pp_ftree ftree new_tab = 
 let dummy = String.make (new_tab-2) ' ' in 
  match ftree with 
   Empty -> Format.print_string ""
  |NodeAt(position) ->
     begin 
      Format.force_newline (); 
      Format.print_break new_tab 0;
      print_string (dummy^"AtomNode: "); 
(*      Format.force_newline (); 
      Format.print_break 0 3;
*)
      print_position position new_tab;
      Format.force_newline (); 
      Format.print_break new_tab 0
     end
  |NodeA(position,subtrees) -> 
    let tree_list = Array.to_list subtrees in 
     begin
      Format.force_newline (); 
      Format.print_break new_tab 0;
      Format.print_break 0 0;
      print_string (dummy^"InnerNode: "); 
      print_position position new_tab;
      Format.force_newline ();
      Format.print_break 0 0;
      pp_ftree_list tree_list (new_tab-3)
     end

in
 let new_tab = tab+5 in 
  match tree_list with 
   [] -> Format.print_string ""
  |first::rest ->  
    begin
     pp_ftree first new_tab;
     pp_ftree_list rest tab
    end





let print_ftree ftree = 
 begin
  Format.open_box 0;
  Format.print_break 3 0;
  pp_ftree_list [ftree] 0;
  Format.print_flush ()
 end;;


let rec print_ordering ordering = 
 match ordering with 
  [] -> Format.print_string ""
 |(a,b)::rest -> 
   begin
    Format.print_break 1 1;
    Format.print_string ("("^a^","^b^")");
    print_ordering rest
   end

let print_pos_n  pos_n =
  Format.print_int pos_n




let print_formula_info ftree ordering pos_n = 
begin
 print_ftree ftree;
 Format.open_box 0;
 Format.force_newline ();
 print_ordering ordering;
 Format.force_newline ();
 Format.force_newline ();
 Format.print_string "number of positions: ";
 print_pos_n pos_n;
 print_endline "";
 print_endline "";
 Format.print_flush ()
end
 

(************ END printing functions  *********************************)



(************ T-STRING UNIFICATION *********************************)



let is_const name  = 
  (String.get name 0) = 'c'


let is_var name  = 
  (String.get name 0) = 'v'



let r_1 s ft rt = 
  (s = []) & (ft = []) & (rt = []) 





let r_2 s ft rt = 
  (s = []) & (ft = []) & (List.length rt >= 1) 





let r_3 s ft rt = 
  if ft=[] then 
    if (List.length s >= 1) &  (List.length rt >= 1) then 
     let x = List.hd s 
     and y = List.hd rt in 
       x=y 
    else 
     false 
  else 
   false 




let r_4 s ft rt = 
  if ft=[] then 
    if (List.length s >= 1) &  (List.length rt >= 1) then 
     let c = List.hd s 
     and v = List.hd rt in 
       (is_const c) & (is_var v) 
    else 
     false 
  else 
   false



 let r_5 s ft rt = 
  if rt=[] then 
    if (List.length s >= 1) then 
     let v = List.hd s in 
       (is_var v) 
    else 
     false
  else 
   false




let r_6 s ft rt = 
  if ft=[] then 
    if (List.length s >= 1) &  (List.length rt >= 1) then 
     let v = List.hd s 
     and c1 = List.hd rt in 
       (is_var v) & (is_const c1) 
    else 
     false
  else 
   false




let r_7 s ft rt = 
    if (List.length s >= 1) &  (List.length rt >= 2) then 
     let v = List.hd s 
     and c1 = List.hd rt 
     and c2 = (List.hd (List.tl rt)) in 
       (is_var v) & (is_const c1) & (is_const c2) 
    else 
     false






let r_8 s ft rt = 
  if ft=[] then 
    if (List.length s >= 2) &  (List.length rt >= 1) then 
     let v = List.hd s 
     and v1 = List.hd rt in 
       (is_var v) & (is_var v1) & (v <> v1)
    else 
     false
  else 
   false





let r_9 s ft rt = 
    if (List.length s >= 2) & (List.length ft >= 1) & (List.length rt >= 1) then 
     let v = (List.hd s)
     and v1 = (List.hd rt) in 
       (is_var v) & (is_var v1) & (v <> v1)
    else 
      false



let r_10 s ft rt = 
    if (List.length s >= 1) &  (List.length rt >= 1) then 
     let v = List.hd s 
     and x = List.hd rt in 
       (is_var v) & (v <> x) & 
        (((List.tl s) =[]) or (is_const x) or ((List.tl rt) <> []))
    else 
     false

 




let rec com_subst slist (ov,ovlist) = 
 match slist with 
  [] -> raise (Failure "Invalid argument")
  |f::r -> 
   if f = ov then 
    (ovlist @ r) 
   else 
    f::(com_subst r (ov,ovlist))


let rec combine subst (ov,oslist)  = 
 match subst with 
  [] -> []
 |f::r -> 
  let (v,slist) = f in  
   if (List.mem ov slist) & (not (List.mem v oslist)) then 
    (v,(com_subst slist (ov,oslist)))::(combine r (ov,oslist))
   else
    f::(combine r (ov,oslist))



let compose sigma one_subst =  (* the composition could be optimized for *) 
                               (* practical reasons *)
  let (n,subst)=sigma 
  and (ov,oslist) = one_subst in 
   let com = combine subst (ov,oslist) 
    in
(*    begin
     print_endline "!!!!!!!!!test print!!!!!!!!!!";
     print_subst [one_subst];
     print_subst subst;
     print_endline "!!!!!!!!! END test print!!!!!!!!!!";
*)
     if List.mem one_subst subst then 
       (n,com)
     else 
(* ov may multiply as variable in subst with DIFFERENT values *)
(* in order to avoid explicit atom instances!!! *) 
      (n,(com @ [one_subst]))
(*   end *)
  



let rec compute_all fs ft rt sigma = 

let apply_r1 fs ft rt sigma = 
 begin
(*  print_endline "r1 -- SUCEED WITH AN UNIFIER!!!"; 
  print_tunify sigma;
*)
   [sigma] 
 end

in
let apply_r2 fs ft rt sigma = 
(*  print_endline "r2"; *)
  compute_all rt fs ft sigma

in
let apply_r3 fs ft rt sigma =
(*  print_endline "r3";  *)
  compute_all (List.tl fs) ft (List.tl rt) sigma

in
let apply_r4 fs ft rt sigma = 
(*  print_endline "r4"; *)
  compute_all rt ft fs sigma 

in
let apply_r5 fs ft rt sigma = 
(*  print_endline "r5"; *)
 let v = (List.hd fs) in 
   let sigma_new = (compose sigma (v,ft)) in 
     compute_all (List.tl fs) rt rt sigma_new

in
let apply_r6 fs ft rt sigma = 
(*  print_endline "r6"; *)
 let v = (List.hd fs) in 
  let sigma_new = (compose sigma (v,[])) in 
    compute_all (List.tl fs) ft rt sigma_new

in
let apply_r7 fs ft rt sigma = 
(*  print_endline "r7"; *)
 let v = (List.hd fs) 
 and c1 = (List.hd rt) 
 and c2t =(List.tl rt) in 
  let sigma_new = (compose sigma (v,(ft @ [c1]))) in 
    compute_all (List.tl fs) [] c2t sigma_new

in
let apply_r8 fs ft rt sigma = 
(*  print_endline "r8"; *)
  compute_all rt [(List.hd fs)] (List.tl fs) sigma


in
let apply_r9 fs ft rt sigma = 
(*  print_endline "r9"; *)
 let v = (List.hd fs) 
 and (max,subst) = sigma in 
  let v_new = ("vnew"^(string_of_int max)) in 
    let sigma_new = (compose ((max+1),subst) (v,(ft @ [v_new]))) in 
      compute_all rt [v_new] (List.tl fs) sigma_new

in
let apply_r10 fs ft rt sigma = 
(*  print_endline "r10"; *)
 let x = List.hd rt in 
 compute_all fs (ft @ [x]) (List.tl rt) sigma

in
  if r_1 fs ft rt then 
   let sigma_1_list = apply_r1 fs ft rt sigma in 
    sigma_1_list
  else 
  if r_2 fs ft rt then 
   let sigma_1_list = apply_r2 fs ft rt sigma in 
    sigma_1_list
  else 
  if r_3 fs ft rt then 
   let sigma_1_list = apply_r3 fs ft rt sigma in 
    sigma_1_list
  else 
  if r_4 fs ft rt then 
   let sigma_1_list = apply_r4 fs ft rt sigma in 
    sigma_1_list
  else 
  if r_5 fs ft rt then 
   let sigma_1_list = apply_r5 fs ft rt sigma in  
    sigma_1_list
  else 
  if r_6 fs ft rt then 
   let sigma_1_list = apply_r6 fs ft rt sigma 
   and sigma_2_list = apply_r10 fs ft rt sigma   in (* r10 always applicable *)
                                                    (*if r6 was *)
      if r_7 fs ft rt then (* r7 applicable if r6 was and tr6 = C2t' *)
       let sigma_3_list = apply_r7 fs ft rt sigma in
         (sigma_1_list @ sigma_2_list @ sigma_3_list)
      else 
         (sigma_1_list @ sigma_2_list)
  else
  if r_7 fs ft rt then  (* not r6 and r7 possible if z <> [] *) 
   let sigma_1_list = apply_r7 fs ft rt sigma 
   and sigma_2_list = apply_r10 fs ft rt sigma in (* r10 always applicable *)
                                                  (* if r7 was *)
      (sigma_1_list @ sigma_2_list)
  else
  if r_8 fs ft rt then  
    let sigma_1_list = apply_r8 fs ft rt sigma in 
      if r_10 fs ft rt then (* r10 applicable if r8 was and tr8 <> [] *)
       let sigma_2_list = apply_r10 fs ft rt sigma in 
         (sigma_1_list @ sigma_2_list)
      else 
        sigma_1_list 
  else
  if r_9 fs ft rt then  
    let sigma_1_list = apply_r9 fs ft rt sigma in 
      if r_10 fs ft rt then (* r10 applicable if r9 was and tr9 <> [] *)
       let sigma_2_list = apply_r10 fs ft rt sigma in 
         (sigma_1_list @ sigma_2_list)
      else 
         sigma_1_list 
  else
  if r_10 fs ft rt then  (* not ri, i<10, and r10 possible if for instance *)
                         (* (s=[] and x=v1) or (z<>[] and xt=C1V1t') *)
    let sigma_1_list = apply_r10 fs ft rt sigma in 
      sigma_1_list
  else  (* NO rule applicable *) 
   begin 
(*    print_endline "FAIL BRANCH!!!"; *)
      []   (* global failure unifier *)
    end
  




let rec apply_subst subst us ns = 
 match us with 
   [] -> [] 
  |f::r -> 
   if (is_var f) & (f<>ns) & (List.mem_assoc f subst) then (* don't apply atom instances!! *)
     (List.assoc f subst) @ (apply_subst subst r ns)
   else
     f::(apply_subst subst r ns)


let apply_univ sigma us ns= 
(*  print_endline "apply old sigma"; *)
 let (n,subst) = sigma in 
  if subst  = [] then
   us
  else
   apply_subst subst us ns
   

 
let tunify us ut ns nt sigma =  
 let s = (apply_univ sigma us ns) 
 and t = (apply_univ sigma ut nt) in 
(* print_endline "go compute"; *)
  compute_all s [] t sigma



let stringunify ext_atom try_one sigma logic = 
 if logic = "C" then 
  [sigma]
 else
  let us  = (Array.to_list ext_atom.aprefix)
  and ut  = (Array.to_list try_one.aprefix) 
  and ns = ext_atom.aname 
  and nt = try_one.aname in 
  tunify us ut ns nt sigma




(* #use "/home/steph/meta-prl/refiner/reflib/jstring_unifyfy.ml" *)


(* type of unifier: (int * (string,string) list) list *)
(* global failure: 0,[] *)





(**************************************** add multiplicity *********************************) 



let rec subst_replace subst_list t = 
 match subst_list with 
  [] -> t
 |(old_t,new_t)::r -> 
  let inter_term = var_subst t  old_t "dummy"  in 
   let new_term = subst inter_term  [new_t] ["dummy"]  in 
    subst_replace r new_term 



let rename_pos x m = 
 let pref = String.get x 0 in 
    (Char.escaped pref)^(string_of_int m) 



let update_position position m replace_n subst_list mult = 
  let ({name=x; address=y; op=z; pol=p; pt=a; st=b; label=t}) = position in 
    let nx = rename_pos x m in 
     let nsubst_list = 
      if b=Gamma_0 then 
        let vx = mk_var_term x
        and vnx = mk_var_term nx in 
         (vx,vnx)::subst_list 
      else
      if b=Delta_0 then 
        let sx = mk_string_term (make_opname []) x
        and snx = mk_string_term (make_opname []) nx in 
         (sx,snx)::subst_list 
      else
        subst_list
       in
       let nt = subst_replace nsubst_list t in 
        let add_array = Array.of_list y in 
         let _ = (add_array.(replace_n) <- mult) in 
          let new_add = Array.to_list add_array in 
          ({name=nx; address=new_add; op=z; pol=p; pt=a; st=b; label=nt},m,nsubst_list)





let rec copy_and_rename_tree last_tree replace_n predecessor ordering pos_n mult subst_list =

 let rec rename_subtrees tree_list nposition s_ordering s_pos_n nsubst_list =  
    match tree_list with 
     [] -> ([||],s_ordering,s_pos_n) 
    | f::r ->
     let (f_subtree,f_ordering,f_pos_n) = 
       copy_and_rename_tree f replace_n nposition s_ordering s_pos_n  mult nsubst_list in 
      let (r_subtrees,r_ordering,r_pos_n) = rename_subtrees r nposition f_ordering f_pos_n nsubst_list in 
        ((Array.append [|f_subtree|] r_subtrees),r_ordering,r_pos_n)

 in
 match last_tree with  
  Empty -> raise (Failure "Invalid argument")
  |NodeAt(position) ->   (* can never be a Gamma_0 position -> no replacements *)
      let (nposition,npos_n,_) = update_position position (pos_n+1) replace_n subst_list mult in 
        ((NodeAt(nposition)),((predecessor.name,nposition.name)::ordering),npos_n)
  |NodeA(position, suctrees) -> 
      let (nposition,npos_n,nsubst_list) = update_position position (pos_n+1) replace_n subst_list mult in 
        let (new_suctrees, new_ordering, new_pos_n) = 
          rename_subtrees (Array.to_list suctrees) nposition ordering npos_n nsubst_list in 
           ((NodeA(nposition,new_suctrees)),((predecessor.name,nposition.name)::new_ordering),new_pos_n)
  





let rec add_multiplicity ftree ordering pos_n  mult = 

  let rec parse_subtrees tree_list s_ordering s_pos_n =  
    match tree_list with 
     [] -> ([||],s_ordering,s_pos_n) 
    |f::r ->
     let (f_subtree,f_ordering,f_pos_n) = add_multiplicity f s_ordering s_pos_n  mult in 
      let (r_subtrees,r_ordering,r_pos_n) = parse_subtrees r f_ordering f_pos_n in 
        ((Array.append [|f_subtree|] r_subtrees),r_ordering,r_pos_n)
     
  in
  match ftree with   
   Empty -> raise (Failure "Invalid argument")
   |NodeAt(_) -> (ftree, ordering, pos_n) 
   |NodeA(pos,suctrees) -> 
    let (new_suctrees, new_ordering, new_pos_n) = parse_subtrees (Array.to_list suctrees) ordering pos_n in 
        if (((pos.pt = Phi) & (pos.op <> At))  (* no explicit atom-instances *)
        or ((pos.pt = Gamma) & (pos.st <> Phi_0))) then   (* universal quantifiers are copied *) 
	                                                        (* at their Phi positions *)
         let replace_n = (List.length pos.address)  (* points to the following argument in the array_of_address *)
         and last = (Array.length new_suctrees) - 1 in 
          let last_tree = new_suctrees.(last) in 
           let (new_tree,final_ordering,final_pos_n) = 
             copy_and_rename_tree last_tree replace_n  pos new_ordering new_pos_n mult [] in
            let final_suctrees = Array.append new_suctrees [|new_tree|] in 
             ((NodeA(pos,final_suctrees)),final_ordering,final_pos_n)
        else       
         ((NodeA(pos,new_suctrees)),new_ordering,new_pos_n)




(**************  Path checker   ******************************************************************************)





let rec get_sets atom atom_sets = 
 match atom_sets with
  [] -> failwith "atom not found"
 |f::r -> 
   let (a,b,c) = f in 
   if atom = a then f 
   else
    get_sets atom r





let rec get_connections a alpha tabulist =  
  match alpha with 
   [] -> [] 
  |f::r -> 
    if (a.apredicate = f.apredicate) & (a.apol <> f.apol) & (not (List.mem f tabulist)) then 
      (a,f)::(get_connections a r tabulist)
    else 
      (get_connections a r tabulist)



let rec connections atom_rel tabulist = 
  match atom_rel with 
    [] -> [] 
  |f::r -> 
    let (a,alpha,beta) = f in
      (get_connections a alpha tabulist) @ (connections r (a::tabulist)) 



let check_alpha_relation atom set atom_sets = 
  let (a,alpha,beta) = get_sets atom atom_sets in 
   AtomSet.subset set alpha
   
 
let rec extset  atom_sets path closed =
 match atom_sets with 
   [] -> AtomSet.empty
 | f::r -> 
    let (at,alpha,beta) = f in 
     if (AtomSet.subset path alpha) & (AtomSet.subset closed beta) then 
       AtomSet.add at (extset r path closed)
     else
       (extset r path closed)



let rec check_ext_list ext_list fail_set atom_sets =  (* fail_set consists of one atom only *)
 match ext_list with 
   [] -> AtomSet.empty
  |f::r -> 
   if (check_alpha_relation f fail_set atom_sets) then 
    AtomSet.add f (check_ext_list r fail_set  atom_sets)
   else
    (check_ext_list r fail_set atom_sets)



let fail_ext_set ext_atom ext_set atom_sets = 
 let ext_list = AtomSet.elements ext_set 
 and fail_set  = AtomSet.add ext_atom AtomSet.empty in 
   check_ext_list ext_list fail_set atom_sets 
       

let rec ext_partners con path ext_atom atom_sets = 
 match con with
  [] -> AtomSet.empty 
 |f::r -> 
   let (a,b) = f in 
    if List.mem ext_atom [a;b] then 
     let ext_partner =  
       if ext_atom = a then b else a 
     in 
      if (AtomSet.mem ext_partner path) or 
         (check_alpha_relation ext_partner path atom_sets) then 
         AtomSet.add ext_partner (ext_partners r path ext_atom atom_sets)
      else
        ext_partners r path ext_atom atom_sets
    else 
      ext_partners r path ext_atom atom_sets
     
     

let rec print_set_list set_list = 
 match set_list with 
  [] -> "" 
 |f::r -> 
   (f.aname)^" "^(print_set_list r)

let print_set  set = 
 let set_list = AtomSet.elements set in 
  if set_list = [] then "empty"
  else 
   print_set_list set_list 




let rec add_arrows (v,vlist) ordering = 
 match vlist with 
  [] -> [] 
 |f::r -> 
   if ((String.get f 0)='c') & (not (List.mem (f,v) ordering)) then 
     (f,v)::(add_arrows (v,r) ordering) 
   else 
     add_arrows (v,r) ordering
  
  
let rec check_subst subst ordering atom_rel = 
 match subst with 
  [] -> ordering
 |(v,vlist)::r -> 
   if (String.get v 1 = 'n') (* don't integrate new variables *)
      or (List.exists (fun (x,_,_) -> (x.aname = v)) atom_rel) then   (* no reduction ordering at atoms *)
    (check_subst r ordering atom_rel)   
   else
    (add_arrows (v,vlist) ordering) @ (check_subst r ordering atom_rel)
    


let add_subst tau ordering atom_rel = 
 let (n,subst) = tau in 
   check_subst subst ordering atom_rel



let path_checker atom_rel atom_sets ordering logic =  
  let con = connections atom_rel [] in 
(*   print_endline "";
   print_endline ("number of connections: "^(string_of_int (List.length con)));
*)


(* sigma global in provable ?? *)
   let rec provable path closed sigma ordering = 
  
     let rec check_unifiers ext_atom try_one sigmalist = 
      match sigmalist with
       [] -> failwith "fail1"
      |tau::rest -> 
        (try 
        let new_ordering = add_subst tau ordering atom_rel in 
         let new_closed = AtomSet.add ext_atom closed in  
           let (subst,next_ordering,subproof) = 
             if AtomSet.mem try_one path then 
              let (tau_1,nordering,p0) = 
                 provable path new_closed tau new_ordering in 
                  (tau_1,nordering,p0)
             else 
              let new_path = AtomSet.add ext_atom path
              and extension = AtomSet.add try_one AtomSet.empty in 
               let (tau_1,nordering,p1) = 
                 provable new_path extension tau new_ordering in 
                 let (tau_2,nnordering,p2) = 
		   provable path new_closed tau_1 nordering in 
                    (tau_2,nnordering, (p1 @ p2))
      (* first the extension subgoals = depth first; then other subgoals in same clause *)
           in 
            (subst,next_ordering,(((ext_atom.aname),(try_one.aname))::subproof))
         with 
           Failure("fail3next") ->                   (* go to next unifier *)
            print_endline "fail3next";
            check_unifiers ext_atom try_one rest
         | Failure("fail3skip") ->                   (* go to next connection  -> fail1 *)
            print_endline "fail3skip";         
            check_unifiers ext_atom try_one []) 
     in  
 
     let rec check_connections ext_partners ext_atom  =  
      if ext_partners = AtomSet.empty then failwith "fail2" 
      else 
       let try_one = AtomSet.choose ext_partners in 
        let try_set = AtomSet.add try_one AtomSet.empty in        (* try_set consisting of one atom *)
         print_endline ("connection partner "^(try_one.aname));
(*       print_endline ("partner path "^(print_set path));
*)
        (try 
         let sigmalist = stringunify ext_atom try_one sigma logic in 
           check_unifiers ext_atom try_one sigmalist
         with Failure("fail1") -> 
(*          print_endline ("new connection for "^(ext_atom.aname)); *)
           print_endline ("fail1");
           check_connections (AtomSet.diff ext_partners try_set) ext_atom)

     in 

(* failflag says if the current subgoals -- including recursion through fail2 -- *)
(* had ever any connections in ext_partners. If so, then backtracking over other *) 
(* tunifiers of the actual entry connection is required: failwith fail3. *)
(* Otherwise, we can cut the search  and go directly to a new entry connection: failwith fail1. *)
     let rec check_extension  extset failflag = 
      if extset = AtomSet.empty then 
       if failflag = true then 
         failwith "fail3next"           (* try other tunifiers of the actual entry connection *)
       else
         failwith "fail3skip"             (* go directly to a new entry connection *)
      else 
       let select_one = AtomSet.choose extset in 
        print_endline ("extension literal "^(select_one.aname));
(*        print_endline ("extension path "^(print_set path));*)
        let ext_partners = ext_partners con path select_one atom_sets in 
         let newflag = 
          if (failflag = true) or (ext_partners <> AtomSet.empty) then 
           true 
          else 
           false
        in 
         (try
           check_connections ext_partners select_one
          with Failure("fail2") -> 
(*         print_endline ("no connections for subgoal "^(select_one.aname)); *)
           print_endline ("fail2");
           let fail_ext_set = fail_ext_set select_one extset atom_sets in 
           check_extension fail_ext_set newflag)
     in 
   
     let extset = extset atom_sets path closed in 
      if extset = AtomSet.empty then (sigma,ordering,[])
      else 
        check_extension extset false
  in    
   provable AtomSet.empty AtomSet.empty (1,[]) ordering







(*************************** prepare and init prover *******************************************************)



let rec list_to_set list = 
 match list with 
  [] -> AtomSet.empty
 |f::r -> 
   let rest_set = list_to_set r in 
    AtomSet.add f rest_set




let rec make_atom_sets atom_rel = 
 match atom_rel with 
  [] -> []
 |f::r -> 
  let (a,alpha,beta) =  f in 
    (a,(list_to_set alpha),(list_to_set beta))::(make_atom_sets r)




let rec predecessor address_1 address_2 ftree = 
   match ftree with 
    Empty -> PNull            (* should not occur since every pair of atoms have a common predecessor *)  
   |NodeAt(position) -> PNull (* should not occur as above *)
   |NodeA(position,suctrees) -> 
     match address_1,address_2 with 
      [],_ -> raise (Failure "Invalid argument")
     |_,[] -> raise (Failure "Invalid argument")
     | (f1::r1),(f2::r2) -> 
      if f1 = f2 then 
        predecessor r1 r2 (suctrees.(f1-1))
      else 
        position.pt       


let rec compute_sets element ftree alist = 
  match alist with 
   [] -> [],[]
  |first::rest -> 
    if first = element then  
      compute_sets element ftree rest    (* element is neithes alpha- nor beta-related to itself*)
    else 
     let (alpha_rest,beta_rest) = compute_sets element ftree rest in  
      if predecessor (element.aaddress) (first.aaddress) ftree = Beta then 
        (alpha_rest,(first::beta_rest))
      else
        ((first::alpha_rest),beta_rest)



let rec compute_atomlist_relations worklist ftree alist =  (* last version of alist for total comparison *)

 let rec compute_atom_relations element ftree alist = 
  let alpha_set,beta_set = compute_sets element ftree alist in 
   (element,alpha_set,beta_set)
 in
  match worklist with 
   [] -> []
  |first::rest -> 
   let first_relations = compute_atom_relations first ftree alist in 
     first_relations::(compute_atomlist_relations rest ftree alist)
 


let dest_atom atom = 
 let dat = dest_term atom in 
   ((dat.term_op),(dat.term_terms))



let atom_record position prefix = 
 let aname = (position.name) in 
  let aprefix = (Array.append prefix [|aname|]) in (* atom position is last element in prefix *)
   let (aop,aterms) = (dest_atom (position.label)) in 
     ({aname=aname; aaddress=(position.address); aprefix=aprefix; apredicate=aop;  
       apol=(position.pol); ast=(position.st); aarguments=aterms})
  
  

let rec select_atoms_treelist treelist prefix = 

 let rec select_atoms ftree prefix = 
  match ftree with 
   Empty -> []
  |NodeAt(position) -> 
   [(atom_record position prefix)]
  |NodeA(position,suctrees) -> 
    let treelist = Array.to_list suctrees in 
     let prefix_element = 
      if List.mem (position.st) [Psi_0;Phi_0] then 
       [|(position.name)|]
      else
       [||]
     in
      select_atoms_treelist treelist (Array.append prefix prefix_element)

 in  
 match treelist with 
  [] -> []
 |first::rest -> 
   List.append (select_atoms first prefix) (select_atoms_treelist rest prefix) 





let prepare_prover ftree =  
  let alist = select_atoms_treelist [ftree] [||] in 
    compute_atomlist_relations alist ftree alist



(* #use "/home/steph/meta-prl/refiner/reflib/jprepare.ml" *)




(* ************************ Build intial formula tree  and relations *********************************** *)
(* Building a formula tree and the tree ordering from the input formula, i.e. OCaml term *)





let make_position_name stype pos_n =  
 let prefix = 
   if List.mem stype [Phi_0;Gamma_0] 
     then "v"
   else
    if List.mem stype [Psi_0;Delta_0] 
      then "c"
    else 
      "a"
 in 
  prefix^(string_of_int pos_n)




let dual_pol pol = 
 if pol = O then I else O



let check_subst_term (variable,old_term) pos_name stype = 
    if (List.mem stype [Gamma_0;Delta_0]) then 
     let new_variable = 
       if stype = Gamma_0 then (mk_var_term pos_name) 
         else
          (mk_string_term (make_opname []) pos_name)
     in
      (subst old_term [new_variable] [variable]) (* replace variable (non-empty) in t by pos_name *)
         (* pos_name is either a variable term or a constant, f.i. a string term *)
         (* !!! check unification module how handling eingenvariables as constants !!! *)
    else 
       old_term




let rec build_ftree (variable,old_term) pol stype address pos_n predecessor = 
 let pos_name = make_position_name stype pos_n in 
  let term = check_subst_term (variable,old_term) pos_name stype in
    if JLogic.is_and_term term then 
     let s,t = JLogic.dest_and term in
       let ptype,stype_1,stype_2 = 
        if pol = O 
          then Beta,Beta_1,Beta_2 
        else 
          Alpha,Alpha_1,Alpha_2
       in 
        let position = {name=pos_name; address=address; op=And; pol=pol; pt=ptype; st=stype; label=term} in
         let subtree_left,ordering_left,posn_left = build_ftree ("",s) pol stype_1 (address@[1]) (pos_n+1) position in 
          let subtree_right,ordering_right,posn_right = build_ftree ("",t) pol stype_2 (address@[2]) 
                                                                    (posn_left+1) position in
          (NodeA(position,[|subtree_left;subtree_right|]),
           (predecessor.name,position.name)::(ordering_left@ordering_right),   
           posn_right
          )
  else 
   if JLogic.is_or_term term then 
    let s,t = JLogic.dest_or term in
      let ptype,stype_1,stype_2 = 
       if pol = O 
        then Alpha,Alpha_1,Alpha_2 
      else 
        Beta,Beta_1,Beta_2
     in 
      let position = {name=pos_name; address=address; op=Or; pol=pol; pt=ptype; st=stype; label=term} in 
       let subtree_left,ordering_left,posn_left = build_ftree ("",s) pol stype_1 (address@[1]) (pos_n+1) position in 
        let subtree_right,ordering_right,posn_right = build_ftree ("",t) pol stype_2 (address@[2]) 
                                                                  (posn_left+1) position in
          (NodeA(position,[|subtree_left;subtree_right|]),
           (predecessor.name,position.name)::(ordering_left@ordering_right),   
           posn_right
          )
   else
   if JLogic.is_implies_term term then 
    let s,t = JLogic.dest_implies term in
      let ptype_0,stype_0,ptype,stype_1,stype_2 = 
       if pol = O 
        then Psi,Psi_0,Alpha,Alpha_1,Alpha_2 
      else 
        Phi,Phi_0,Beta,Beta_1,Beta_2
     in 
      let pos2_name = make_position_name stype_0 (pos_n+1) in 
      let sposition = {name=pos_name; address=address; op=Imp; pol=pol; pt=ptype_0; st=stype; label=term} 
      and position = {name=pos2_name; address=address@[1]; op=Imp; pol=pol; pt=ptype; st=stype_0; label=term} in
       let subtree_left,ordering_left,posn_left = build_ftree ("",s) (dual_pol pol) stype_1 (address@[1;1]) 
                                                              (pos_n+2) position in 
        let subtree_right,ordering_right,posn_right = build_ftree ("",t) pol stype_2 (address@[1;2]) 
                                                                  (posn_left+1) position in
          (NodeA(sposition,[|NodeA(position,[|subtree_left;subtree_right|])|]),
	   [(predecessor.name,sposition.name);(sposition.name,position.name)]@(ordering_left@ordering_right),   
           posn_right
          )
   else
   if JLogic.is_not_term term then 
    let s = JLogic.dest_not term in
      let ptype_0,stype_0,ptype,stype_1=
       if pol = O 
        then Psi,Psi_0,Alpha,Alpha_1
      else 
        Phi,Phi_0,Alpha,Alpha_1
      in
      let pos2_name = make_position_name stype_0 (pos_n+1) in 
      let sposition = {name=pos_name; address=address; op=Neg; pol=pol; pt=ptype_0; st=stype; label=term} 
      and position = {name=pos2_name; address=address@[1]; op=Neg; pol=pol; pt=ptype; st=stype_0; label=term} in
       let subtree_left,ordering_left,posn_left = build_ftree ("",s) (dual_pol pol) stype_1 (address@[1;1]) 
                                                                 (pos_n+2) position in 
          (NodeA(sposition,[|NodeA(position,[|subtree_left|])|]),
	   [(predecessor.name,sposition.name);(sposition.name,position.name)]@(ordering_left),
           posn_left
          )
   else
   if JLogic.is_exists_term term then 
    let v,s,t = JLogic.dest_exists term in  (* s is type of v and will be supressed here *)
      let ptype,stype_1 = 
       if pol = O 
        then Gamma,Gamma_0
      else 
        Delta,Delta_0
     in 
      let position = {name=pos_name; address=address; op=Ex; pol=pol; pt=ptype; st=stype; label=term} in
        let subtree_left,ordering_left,posn_left = build_ftree (v,t) pol stype_1 (address@[1]) (pos_n+1) position in 
          (NodeA(position,[|subtree_left|]),
           (predecessor.name,position.name)::(ordering_left),
           posn_left
          )
   else
   if JLogic.is_all_term term then 
    let v,s,t = JLogic.dest_all term in     
       (* s is type of v and will be supressed here *) 
      let ptype_0,stype_0,ptype,stype_1=
       if pol = O 
        then Psi,Psi_0,Delta,Delta_0
      else 
        Phi,Phi_0,Gamma,Gamma_0
      in
      let pos2_name = make_position_name stype_0 (pos_n+1) in 
      let sposition = {name=pos_name; address=address; op=All; pol=pol; pt=ptype_0; st=stype; label=term} 
      and position = {name=pos2_name; address=address@[1]; op=All; pol=pol; pt=ptype; st=stype_0; label=term} in
       let subtree_left,ordering_left,posn_left = build_ftree (v,t) pol stype_1 (address@[1;1]) 
                                                                 (pos_n+2) position in 
          (NodeA(sposition,[|NodeA(position,[|subtree_left|])|]),
	   [(predecessor.name,sposition.name);(sposition.name,position.name)]@(ordering_left),
           posn_left
          )
   else      (* finally, term is atomic *)
    let ptype_0,stype_0 = 
       if pol = O
         then Psi,Psi_0
       else 
         Phi,Phi_0
    in
     let pos2_name = make_position_name stype_0 (pos_n+1) in 
     let sposition = {name=pos_name; address=address; op=At; pol=pol; pt=ptype_0; st=stype; label=term} 
     and position = {name=pos2_name; address=address@[1]; op=At; pol=pol; pt=PNull; st=stype_0; label=term} in
     (NodeA(sposition,[|NodeAt(position)|]),
      [(predecessor.name,sposition.name);(sposition.name,position.name)],
      pos_n+1
     )


    

let construct_ftree term = 
  print_endline "tree in";
 let new_root = {name="w"; address=[]; op=Null; pol=O; pt=Psi; st=PNull_0; label=term} in 
   let tree,ordering,pos_n = build_ftree ("",term) O Psi_0 [1] 1 new_root in 
     print_endline "tree out";
      NodeA(new_root,[|tree|]),ordering,pos_n
                      



(*************************** Main LOOP ************************************)




let init_prover ftree = 
     let atom_relation = prepare_prover ftree in 
(*      print_atom_info atom_relation; *)
        let atom_sets = make_atom_sets atom_relation in 
         (atom_relation,atom_sets)
          


let rec try_multiplicity ftree ordering pos_n mult logic = 
 (try 
    let (atom_relation,atom_sets) = init_prover ftree in 
      path_checker atom_relation atom_sets ordering logic 
  with 
   fail_multiplicity -> 
    let new_mult = mult+1 in 
     begin 
       print_endline "!!! Multiplicity Fail !!!!";
       print_endline ("Try new multiplicity: "^(string_of_int new_mult));
       let (new_ftree,new_ordering,new_pos_n) = add_multiplicity ftree ordering pos_n new_mult in 
(*       print_formula_info new_ftree new_ordering new_pos_n;  *)
       try_multiplicity new_ftree new_ordering new_pos_n new_mult logic
     end
 )




let prove term logic = 
 let (ftree,ordering,pos_n) = construct_ftree term in (* pos_n = number of positions without new root "w" *) 
(*   print_formula_info ftree ordering pos_n;  *)
    try_multiplicity ftree ordering pos_n 1 logic 



(**************** Test multiplicity increase!! ***********************)


let testt term = 
 let (ftree,ordering,pos_n) = construct_ftree term in (* pos_n = number of positions without new root "w" *) 
(*   print_formula_info ftree ordering pos_n; *)
   (ftree,ordering,pos_n)


let testm  ftree ordering pos_n new_mult = 
    let (new_ftree,new_ordering,new_pos_n) = add_multiplicity ftree ordering pos_n new_mult in 
(*     print_formula_info new_ftree new_ordering new_pos_n; *)
      (new_ftree,new_ordering,new_pos_n)

(**************** Test multiplicity increase!! END ***********************)



let test term logic = 
  let (unifier,ordering,ext_proof) = prove term logic in 
      begin
	Format.open_box 0;
        print_endline "";
        print_endline "";
        print_endline "Extension proof:";
        print_ordering ext_proof;            (* print list of type (string * string) list *) 
        print_endline "";
        print_endline "";
	Format.print_flush ();
        print_endline "";
        print_endline "";
        print_endline ("Length of extension proof: "^((string_of_int (List.length ext_proof))));
        print_endline "";
        print_endline "";
        print_tunify unifier;
        print_endline "";
        print_endline "";
        print_ordering ordering
      end


(* #use "/home/steph/meta-prl/refiner/reflib/jtree.ml" *)


end (* of struct *)
