(* $Id$
 * ----------------------------------------------------------------------
 * Search & Replace
 *)


exception Replace_phrase of (int * string);;


let index_of_substring_from s k_left substr =
  let l = String.length s in
  let lsub = String.length substr in
  let k_right = l - lsub in
  let c = if substr <> "" then substr.[0] else ' ' in
  let rec search k =
    if k <= k_right then begin
      if String.sub s k lsub = substr then
	k
      else
	let k_next = String.index_from s (k+1) c in
	search k_next
    end
    else raise Not_found
  in
  if substr = "" then k_left else search k_left
;;


let rindex_of_substring_from s k_right substr =
  let l = String.length s in
  let lsub = String.length substr in
  let c = if substr <> "" then substr.[0] else ' ' in
  let rec search k =
    if k >= 0 then begin
      if String.sub s k lsub = substr then
	k
      else
	let k_next = String.rindex_from s (k-1) c in
	search k_next
    end
    else raise Not_found
  in
  if substr = "" then k_right else search k_right
;;


let index_of_substring s substr =
  index_of_substring_from s 0 substr;;

let rindex_of_substring s substr =
  rindex_of_substring_from s (String.length s - String.length substr) substr;;


let contains_substring s substr =
  try
    let _ = index_of_substring s substr in true
  with
    Not_found -> false
;;


let contains_substring_from s k_left substr = 
  try
    let _ = index_of_substring_from s k_left substr in true
  with
    Not_found -> false
;;


let rcontains_substring_from s k_right substr = 
  try
    let _ = rindex_of_substring_from s k_right substr in true
  with
    Not_found -> false
;;


let indexlist_of_substring s substr =
  let rec enumerate k =
    try
      let pos = index_of_substring_from s k substr in
      pos :: enumerate (pos+1)
    with
      Not_found -> []
  in
  enumerate 0
;;


let rev_concat sep sl =
  (* = String.concat sep (List.rev sl), but more efficient *)

  let lsep = String.length sep in
  let rec get_len v sl =
    match sl with
      [] -> v
    | s :: sl' ->
	get_len (v + lsep + String.length s) sl'
  in

  let len = 
    if sl = [] then 0 else get_len 0 sl - lsep in

  let t = String.create len in
  
  let rec fill_in k sl =
    match sl with
      [] -> ()
    | [ s ] ->
	let s_len = String.length s in
	String.blit s 0 t (k-s_len) s_len
    | s :: sl' ->
	let s_len = String.length s in
	let k' = k - s_len in
	let k'' = k' - lsep in
	String.blit s 0 t k' s_len;
	String.blit sep 0 t k'' lsep;
	fill_in k'' sl'
  in

  fill_in len sl;
  t
;;


let replace_char s rule =
  let l = String.length s in
  let rec replace coll k_last k =
    if k < l then begin
      let c = s.[k] in
      try
	let s' = rule c k in
	raise (Replace_phrase (1,s'))
	  (* Alternatively, we could directly invoke 'replace' with some
	   * parameters. But this would be a true recursion, without the
	   * chance to be eliminated.
	   * Would lead to Stack_overflow for large strings.
	   *)
      with
	Match_failure(_,_,_) ->
	  replace coll k_last (k+1)
      |	Not_found ->
	  replace coll k_last (k+1)
      |	Replace_phrase (length, s') ->
	  replace (s' :: String.sub s k_last (k-k_last) :: coll) (k+length) (k+length)
    end
    else
      String.sub s k_last (k-k_last) :: coll
  in
  rev_concat "" (replace [] 0 0)
;;


let replace_substring s substrlist rule =
  let characters =
      (List.map
	 (fun substr ->
	   if substr = "" then
	     failwith "replace_substring"
	   else
	     substr.[0])
	 substrlist) in

  let l = String.length s in

  let rec find k sl =
    match sl with
      [] -> raise Not_found
    | sub :: sl' ->
	let lsub = String.length sub in
	if k <= l - lsub & String.sub s k lsub = sub then
	  let replacement = rule sub k in
	  raise (Replace_phrase(lsub, replacement))
	else
	  raise Not_found
  in

  let rule' c k =
    if List.mem c characters then 
      find k substrlist
    else
      raise Not_found
  in

  let rule'' c0 c k =
    if c = c0 then find k substrlist else raise Not_found in

  if List.length substrlist = 1 then
    replace_char s (rule'' (List.hd substrlist).[0])
  else
    replace_char s rule'
;;


(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 1.1  2003/02/17 08:56:18  granicz
 * Added Phobos (with a few features stripped out) to MetaPRL.
 *
 * Revision 1.1  2001/07/31 07:32:17  granicz
 * *** empty log message ***
 *
 * Revision 1.1  1999/06/27 23:03:38  gerd
 * 	Initial revision.
 *
 * 
 *)
