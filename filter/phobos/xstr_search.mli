(* $Id$
 * ----------------------------------------------------------------------
 * Search & Replace
 *)


exception Replace_phrase of (int * string);;
  (* see 'replace_char' and 'replace_string' *)

val index_of_substring_from : string -> int -> string -> int
    (* index_of_substring_from s k_left substr:
     * finds the leftmost index >= k_left where 'substr' occurs within s
     * or raises Not_found.
     *)

val rindex_of_substring_from : string -> int -> string -> int
    (* eindex_of_substring_from s k_right substr:
     * finds the rightmost index <= k_right where 'substr' occurs within s
     * or raises Not_found.
     *)

val index_of_substring : string -> string -> int
    (* index_of_substring s substr:
     * finds the leftmost index where 'substr' occurs within s
     * or raises Not_found.
     *)

val rindex_of_substring : string -> string -> int
    (* eindex_of_substring s substr:
     * finds the rightmost index where 'substr' occurs within s
     * or raises Not_found.
     *)

val contains_substring : string -> string -> bool
    (* contains_substring s substr:
     * true iff substr occurs in s
     *)

val contains_substring_from : string -> int -> string -> bool
    (* contains_substring_from s k_left substr:
     * true iff substr occurs in s at index k_left or later
     *)

val rcontains_substring_from : string -> int -> string -> bool
    (* rcontains_substring_from s k_right substr:
     * true iff substr occurs in s at index k_right or earlier
     *)

val indexlist_of_substring : string -> string -> int list
    (* indexlist_of_substring s substr:
     * Returns a list of all indexes of substrings substr in s
     *)

val rev_concat : string -> string list -> string
    (* rev_concat s l = String.concat s (List.rev l) *)

val replace_char : string -> (char -> int -> string) -> string
    (* replace_char s rule:
     * replaces characters in s according to rule.
     * rule c k = s' means: replace character c where c = s.[k] by s'
     * The rule may raise Match_failure or Not_found in which case
     * the character is not replaced.
     * It may raise Replace_phrase (l,s') which means that the l
     * characters at k should be replaced by s'.
     *
     * EXAMPLE:
     *
     * - replace '<', '>', '&' in an HTML document by CDATA entities:
     *   replace_char s (fun c k -> 
     *                     match c with
     *                       '<' -> "&lt;"
     *                     | '>' -> "&gt;"
     *                     | '&' -> "&amp;"
     *                  )
     * - replace backslashes by double-backslashes and precede quotes with
     *   backslashes:
     *   replace_char s (fun c k ->
     *                     match c with
     *                       '\\' -> "\\\\"
     *                       '"'  -> "\\\"")
     * - the reverse function (remove backslashes):
     *   replace_char s (fun c k ->
     *                     match c with
     *                       '\\' -> begin
     *                                 if k+1 < String.length s then
     *                                   raise 
     *                                     (Replace_phrase 
     *                                       (2, String.make 1 (s.[k+1])))
     *                                 else
     *                                   raise Not_found
     *                               end)
     *)

val replace_substring : string -> string list -> (string -> int -> string)
                          -> string
    (* replace_substring s substrlist rule:
     * replaces all occurences of substrings in 's' which are enumerated 
     * in 'substrlist' by applying 'rule'.
     * rule t k = t': means that substring t at position k is replaced by t'
     * The rule may raise Match_failure or Not_found in which case
     * the character is not replaced.
     * It may raise Replace_phrase (l,s') which means that the l
     * characters at k should be replaced by s'.
     *
     * EXAMPLE:
     *
     * - Interpret CDATA entities of HTML:
     *   replace_substring s 
     *                     [ "&lt;"; "&gt;"; "&amp;" ]
     *                     (fun s k -> match s with
     *                                   "&lt;" -> "<"
     *                                 | "&gt;" -> ">"
     *                                 | "&amp;" -> "&")
     *)


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
