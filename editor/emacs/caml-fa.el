;;
;; This compiles finite automata for CAML-mode.
;;

(require 'caml-util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Munge functions on the syntax description.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Get a list of all the terminals.
;;
(defun caml-get-all-terminals (l)
  "Get all the terminals in the syntax specification"
  (let (all)
    (while l
      (setq all (caml-get-all-terminals-aux all (cdr (car l))))
      (setq l (cdr l)))
    (mapcar 'symbol-name all)))

(defun caml-get-all-terminals-aux (all l)
  "Get all the terminals in a production specification"
  (cond ((symbolp l)
	 ;; Add symbols to the list of terminals
	 (if (memq l '(& &- &+))
	     all
	   (let* ((str (symbol-name l))
		  (len (length str))
		  (strx (substring str 1)))
	     (if (and (> len 0) (= (aref str 0) ?&) (= (string-to-number strx) 0))
		 (setq l (intern strx))))
	   (if (memq l all) all (cons l all))))

	((consp l)
	 ;; Don't add the first arg in lists
	 (setq l (cdr l))
	 (while l
	   (setq all (caml-get-all-terminals-aux all (car l)))
	   (setq l (cdr l)))
	 all)

	;; Ignore other things
	(t all)))

;;
;; Get precedence of terminals.
;;
(defun caml-get-terminal-precedences (l)
  "Get the precedence of each terminal"
  (let (precs prec terminals token)
    (while l
      (setq prec (car (car l)))
      (setq terminals (caml-get-all-terminals-aux nil (cdr (car l))))
      (while terminals
	(setq token (car terminals))
	(if (not (assq token precs))
	    (setq precs (cons (cons token prec) precs)))
	(setq terminals (cdr terminals)))
      (setq l (cdr l)))
    precs))

;;
;; Get the terminals that cause indentation when starting a production.
;;
(defun caml-get-terminal-indenters (l)
  "Get the terminals that start an indentation block"
  (let (all new pattern)
    (while l
      (setq pattern (car l))
      (when (nth 1 pattern)
	    (setq pattern (nthcdr 2 pattern))
	    (setq new nil)
	    (while (and (not new) pattern)
	      (setq new (caml-get-all-terminals-aux nil (car pattern)))
	      (if new (setq all (nconc all new)))
	      (setq pattern (cdr pattern))))
      (setq l (cdr l)))
    all))
		    
;;
;; Get the terminals that start a production.
;;
(defun caml-get-initial-terminals (l)
  "Get the terminals that start a production"
  (let (all)
    (while l
      (let (new pattern (prod (cdr (cdr (car l)))))
	(while (not new)
	  (setq pattern (car prod))
	  (setq new (caml-get-all-terminals-aux nil pattern))
	  (setq prod (cdr prod)))
	(if new (setq all (nconc all new))))
      (setq l (cdr l)))
    all))
		    
;;
;; Put all the terminals into a regex string.
;;
(defun caml-squash-terminals (l &optional suffix)
  "Concat all the terminals in CAML mode into a regex string"
  (if (not suffix) (setq suffix ""))
  (let (all)
    (when l
	  (setq all (concat (caml-quote-regex-name (car l)) suffix))
	  (setq l (cdr l)))
    (while l
      (setq all (concat (caml-quote-regex-name (car l)) suffix "\\|" all))
      (setq l (cdr l)))
    all))

(defun caml-quote-regex-name (name)
  "Turn a literal string match into a regular expression"
  (let ((l (length name))
	(i 0)
	new c)
    (while (< i l)
      (setq c (elt name i))
      (if (memq c '(?[ ?] ?. ?* ?+ ?? ?^ ?$ ?\\))
	  (setq new (cons c (cons ?\\ new)))
	(setq new (cons c new)))
      (setq i (1+ i)))
    (concat (reverse new))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Union of lists of integers.
;; Invariant: lists are always sorted
;;
(defun caml-list-union (&rest args)
  "Take the union of a list of sorted integer lists"
  (if args
      (let ((new (car args)))
	(setq args (cdr args))
	(while args
	  (setq new (caml-list-union-aux new (car args)))
	  (setq args (cdr args)))
	new)))

(defun caml-list-union-aux (list1 list2)
  "Take the union of two sorted lists of integers"
  (let (item1 item2 new)
    (while (and list1 list2)
      (setq item1 (car list1))
      (setq item2 (car list2))
      (cond ((< item1 item2)
	     (setq new (cons item1 new))
	     (setq list1 (cdr list1)))
	    ((> item1 item2)
	     (setq new (cons item2 new))
	     (setq list2 (cdr list2)))
	    (t
	     (setq new (cons item1 new))
	     (setq list1 (cdr list1))
	     (setq list2 (cdr list2)))))
    (nconc (nreverse new) list1 list2)))

;;
;; List substraction of sorted lists of integers.
;;
(defun caml-list-subtract (list1 list2)
  "Subtract two set of sorted integers."
  (let (new item1 item2)
    (while (and list1 list2)
      (setq item1 (car list1))
      (setq item2 (car list2))
      (cond ((< item1 item2)
	     (setq new (cons item1 new))
	     (setq list1 (cdr list1)))
	    ((> item1 item2)
	     (setq list2 (cdr list2)))
	    (t
	     (setq list1 (cdr list1))
	     (setq list2 (cdr list2)))))
    (if list1
	(nconc (nreverse new) list1)
      (nreverse new))))

;;
;; See if two lists intersect.
;;
(defun caml-list-isect-p (list1 list2)
  "Determine if two lists of sorted integers intersect at all"
  (let (flag item1 item2)
    (while (and list1 list2 (not flag))
      (setq item1 (car list1))
      (setq item2 (car list2))
      (cond ((< item1 item2)
	     (setq list1 (cdr list1)))
	    ((> item1 item2)
	     (setq list2 (cdr list2)))
	    (t (setq flag t))))
    flag))

;;
;; Insert an integer into a sorted list of integers.
;;
(defun caml-list-insert (item l)
  "Insert an integer into a sorted list of integers, destructively."
  (if l
      (let ((s l) (flag t) lp item2)
	;; Search for the insertion point
	(while (and l flag)
	  (setq item2 (car l))
	  (if (>= item2 item)
	      (setq flag nil)
	    (setq lp l)
	    (setq l (cdr l))))

	;; Insert it
	(if l
	    (cond ((= item (car l)))
		  (lp (setcdr lp (cons item l)))
		  (t (setq s (cons item s))))
	  (setcdr lp (cons item nil)))
	s)

    ;; If the list is empty, just make a singleton list
    (cons item nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUTOMATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; We compile patterns into nondeterministic finite automata.
;; Each NFA is represented as a vector.  Each entry in the vector
;; corresponds to one state of the automaton.  The initial state is
;; state 0, and the final state is the last state.
;;
;; Each state is an association list containing edges:\.
;;    Each edge has a:
;;       car: label (a symbol)
;;       cdr: states to move to on accepting the label
;;
;; There are also production labels, which are cons cells containing:
;;       car: if t, then always indent
;;            if nil, then never indent
;;            otherwise, don't indent on the given label
;;       cdr: precedence of this production
;;
;; To compile a regular expression, we use the following rules
;; to construct the automaton:
;;    word : automaton accepting the given word:\
;;    &    : indented production
;;    &-   : unindented production
;;    &sym : production, indented except if the next token is 'sym
;;    (seq pattern1 ... patternN):
;;        Recursively construct the automata,
;;        concatenate their states into a new vector,
;;        and to all of the final states in each machine
;;        add the initial state of the next machine.
;;    (alt pattern1 ... patternN)
;;        Recursively construct the automata.
;;        Concatentate their states into a new vector, except
;;        for the first state of each.  For the new first state,
;;        combine the first states of each of the automata.
;;        Take the union of all the final states.
;;    (* pattern1 ... patternN)
;;        Construct the sequence automaton.
;;        Add the initial state to all the final states.
;;    (+ pattern1 ... patternN)
;;       Construct (seq pattern1 ... patternN) (* pattern1 ... patternN)
;;    (opt pattern1 ... patternN)
;;       Construct (seq pattern1 ... patternN)
;;       Make the initial state final.
;;
;;
(defun caml-compile-pattern (prec pattern)
  "Compile an NFA from a pattern"
  (cond ((eq pattern '&+)
	 ;; Three states.  Later, final states will be linked to the $
	 (vector nil (cons (list '$ 2) nil) nil))

	((and (symbolp pattern)
	      ;; In this case, if the symbol starts with &, then it
	      ;; is a new production.
	      (let* ((str (symbol-name pattern))
		     (len (length str))
		     code)
		(when (and (> len 0) (= (aref str 0) ?&))
		      (setq str (substring str 1))
		      (setq code
			    (cond ((equal str "") '(1))
				  ((equal str "-") nil)
				  (t
				   (let ((i (string-to-number str)))
				     (if (> i 0) (cons i nil) (intern str))))))
		      (vector `(((,code . ,prec) 1)) nil)))))

	((symbolp pattern)
	 ;; Single word automaton
	 (vector `((,pattern 1)) nil))

	((listp pattern)
	 ;; Complex construction
	 (let ((command (car pattern))
	       (rest (cdr pattern)))
	   (cond ((eq command 'seq)
		  (caml-compile-seq-pattern prec rest))
		 ((eq command 'alt)
		  (caml-compile-alt-pattern prec rest))
		 ((eq command '*)
		  (caml-compile-*-pattern prec rest))
		 ((eq command '+)
		  (caml-compile-+-pattern prec rest))
		 ((eq command 'opt)
		  (caml-compile-opt-pattern prec rest)))))

	(t
	 ;; Null automaton
	 (vector nil nil))))

;;
;; Compile a pattern that is the sequence of other patterns.
;; Take the final state of each automaton, and add an epsilon
;; edge to the initial state of the next automaton.
;;
(defun caml-compile-seq-pattern (prec patterns)
  "Compile a finite automaton from the sequence of others"
  (caml-sequence-nfa (mapcar '(lambda (x) (caml-compile-pattern prec x)) patterns)))

(defun caml-sequence-nfa (automata)
  "Compute the sequence of a NFAs"
  (cond ((null automata) (vector nil nil))
	((null (cdr automata)) (car automata))
	(t
	 ;; Construct a list of automata to be catted
	 (let (nfa nfa2 len index newautomata)
	   ;; Get initial automaton
	   (setq nfa (car automata))
	   (setq len (length nfa))
	   (setq index len)
	   (setq newautomata (cons nfa nil))

	   ;; Walk through the remaining automata
	   (setq automata (cdr automata))
	   (while automata
	     (setq nfa2 (car automata))
	     (setq automata (cdr automata))

	     ;; Add the epsilon edge to the final state, and push it
	     (caml-add-final-epsilon-edge nfa index)

	     ;; Shift the next automaton
	     (setq nfa (caml-shift-nfa nfa2 index))
	     (setq len (length nfa))
	     (setq index (+ index len))
	     (setq newautomata (cons nfa newautomata)))

	   ;; Cat them
	   (apply 'vconcat (nreverse newautomata))))))

;;
;; Compile a pattern that is the alternate of other patterns.
;; This cats all the automata and adds new initial and final states.
;;
(defun caml-compile-alt-pattern (prec patterns)
  "Compile a finite automaton from the alternate of others"
  (caml-alternate-nfa (mapcar '(lambda (x) (caml-compile-pattern prec x)) patterns)))

(defun caml-alternate-nfa (automata)
  "Compile a finite automaton from the alternate of others"
  (cond ((null automata) (vector nil))
	((null (cdr automata)) (car automata))
	(t
	 (let (nfa len index final newautomata istates l)
	   ;; Figure out the initial states, and the max state
	   (setq final 1)
	   (setq l automata)
	   (while l
	     (setq istates (cons final istates))
	     (setq final (+ final (length (car l))))
	     (setq l (cdr l)))

	   ;; Shift and add all the automata
	   (setq index 1)
	   (setq newautomata `((((t ,@(nreverse istates))))))
	   (while automata
	     ;; Shift it, and add epsilon edge
	     (setq nfa (caml-shift-nfa (car automata) index))
	     (setq nfa (caml-add-final-epsilon-edge nfa final))
	     (setq newautomata (cons nfa newautomata))

	     (setq index (+ index (length nfa)))
	     (setq automata (cdr automata)))

	   ;; Add final state
	   (setq newautomata (cons (cons nil nil) newautomata))

	   ;; Cat them
	   (apply 'vconcat (nreverse newautomata))))))

;;
;; Compile a Kleene closure.
;; Just add an epsilon edge from the initial to the final state,
;; and from the final state to the initial.
;;
(defun caml-compile-*-pattern (prec patterns)
  "Compile the Kleene closure of a sequence.  This is performed in-place."
  (let* ((nfa (caml-compile-seq-pattern prec patterns))
	 (final (1- (length nfa))))
    (if (= final 0)
	nfa
      (setq nfa (caml-add-epsilon-edge nfa 0 final))
      (caml-add-epsilon-edge nfa final 0))))

;;
;; Compile a pattern one-or-more-times.
;; Just add an epsilon edge from the final state to the initial.
;;
(defun caml-compile-+-pattern (prec patterns)
  "Compile a pattern one-or-more-times"
  (let ((nfa (caml-compile-seq-pattern prec patterns)))
    (caml-add-final-epsilon-edge nfa 0)))

;;
;; Compile a pattern zero-or-one times.
;; In this case, add an epsilon edge from the initial state to the final.
;;
(defun caml-compile-opt-pattern (prec patterns)
  "Compile a pattern zero-or-one times"
  (let* ((nfa (caml-compile-seq-pattern prec patterns))
	 (final (1- (length nfa))))
    (caml-add-epsilon-edge nfa 0 final)))

;;
;; Add an offset to the automaton.
;;
(defun caml-shift-nfa (nfa index)
  "Shift the NFA by the specified amount.  This is performed in-place."
  (if (= index 0)
      nfa
    (let* ((len (length nfa))
	   (i 0))

      ;; Shift the edges
      (while (< i len)
	(caml-shift-nfa-entry (aref nfa i) index)
	(setq i (1+ i)))

      nfa)))

(defun caml-shift-nfa-entry (entry index)
  "Shift a NFA edge list by the specified amount"
  (let ((l entry) edge)
    (while l
      (setq edge (car l))
      (setcdr edge (mapcar '(lambda (i) (+ i index)) (cdr edge)))
      (setq l (cdr l)))
    entry))

;;
;; Link all final states to all $ edges with epsilon edges.
;;
(defun caml-link-final-states (nfa)
  "Link all final states to &+ patterns."
  (let* ((len (length nfa))
	 (final (1- len))
	 (i 0)
	 edge)
    ;; Find all $ states
    (while (< i len)
      (setq edge (assq '$ (aref nfa i)))
      (when edge
	    (caml-add-epsilon-edge nfa final i))
      (setq i (1+ i)))
    nfa))

;;
;; Add epsilon edges.
;;
(defun caml-add-epsilon-edge (nfa i j)
  "Add an epsilon edge to the NFA from state i to state j.
This operation is destructive"
  (let* ((entry (aref nfa i))
	 (edge (assq 't entry)))
    (if edge
	(setcdr edge (caml-list-insert j (cdr edge)))
      (aset nfa i (cons (list t j) entry)))
    nfa))

(defun caml-add-final-epsilon-edge (nfa j)
  "Add an epsilon edge from the final state"
  (let ((final (1- (length nfa))))
    (caml-add-epsilon-edge nfa final j)))

;;
;; Remove the epsilon edges in the NFA.  Basically, for each state,
;; compute the closure of that state recursively.  This algorithm
;; is quadratic.
;;
(defun caml-remove-epsilon-edges (nfa)
  "Remove the epsilon edges from the NFA destructively"
  (let* ((len (length nfa))
	 (table (make-vector len nil))
	 (i 0))
    ;; Initialize the table
    (while (< i len)
      (aset table i (cdr-safe (assq 't (aref nfa i))))
      (setq i (1+ i)))

    ;; Close it
    (setq i 0)
    (while (< i len)
      (caml-compute-epsilon-closure-edge table i)
      (setq i (1+ i)))

    ;; Now convert all the states
    (setq i 0)
    (while (< i len)
      (caml-epsilon-convert-edge nfa table i)
      (setq i (1+ i)))

    ;; New initial state
    (cons (aref table 0) nfa)))

;;
;; Close a particular entry, assuming all previous
;; entries have been explored completely.
;;
(defun caml-compute-epsilon-closure-edge (table i)
  "Compute the closure of a particular edge in the NFA"
  (let ((states (aref table i))
	(nstates (cons i nil))
	state)
    ;; Loop through all these states
    (while states
      (setq state (car states))
      (setq states (cdr states))
      (setq nstates (caml-list-insert state nstates))
      (cond ((= state i))
	    ((< state i)
	     ;; Lower entries are fully expanded
	     (setq nstates (caml-list-union nstates (aref table state))))
	    (t
	     ;; Add all the states we haven't examined before
	     (setq states (caml-list-union states
					   (caml-list-subtract (aref table state)
							       nstates))))))

    ;; Save the result
    (aset table i nstates)))

;;
;; Given the equivalence, expand the edge in the NFA.
;;
(defun caml-epsilon-convert-edge (nfa table i)
  "Expand a particular edge in the NFA"
  (let ((entry (aref nfa i))
	edge label states nentry)
    ;; Expand all the edges
    (while entry
      (setq edge (car entry))
      (setq label (car edge))
      (unless (eq label t)
	      (setq states (mapcar '(lambda (state) (aref table state)) (cdr edge)))
	      (setq states (apply 'caml-list-union states))
	      (setq nentry (cons (cons label states) nentry)))
      (setq entry (cdr entry)))

    ;; Set the result
    (aset nfa i nentry)))

;;
;; Convert the NFA to a DFA.
;; We do this by creating new states for each possible combination
;; of states reachable from the initial state.  In the process we
;; use a list of (state list * edges) and create a new state
;; whenever a new combination is used.
;;
;; "new" is the DFA description, which is an alist of
;; state combinations * edges.  Its cdr is modified
;; by caml-expand-dfa-state when new states are
;; created.
;;
(defun caml-dfa-of-nfa (nfa)
  "Convert the NFA to a DFA."
  (let (state entries new index)
    (setq state (car nfa))
    (setq entries (cdr nfa))
    (setq new (cons (cons state nil) nil))
    (setq index new)
    (while index
      (caml-expand-dfa-state entries new index)
      (setq index (cdr index)))

    ;; Convert the specification into a DFA
    (caml-convert-dfa-spec new)))

;;
;; Expand the dfa state by figuring all possible edge combinations.
;;
(defun caml-expand-dfa-state (entries new index)
  "Expand the DFA state specified by (car index)"
  (let ((states (car (car index)))
	l state final)

    ;; Determine if this is a final state
    (setq final (if (member (1- (length entries)) states) t nil))

    ;; Combine edge lists
    (setq l (caml-combine-edge-lists
	     (mapcar '(lambda (state) (aref entries state)) states)))

    ;; Save the new state description
    (setcdr (car index) (cons final l))

    ;; Record the new state combinations
    (while l
      (setq state (cdr (car l)))
      (unless (assoc state new)
	      (setcdr index (cons (cons state nil) (cdr index))))
      (setq l (cdr l)))))

;;
;; Take a list of edge lists, and combine all the
;; lists with the same labels.
;;
(defun caml-combine-edge-lists (entries)
  "Combine the edge lists into a single edge list"
  (if entries
      (let ((entry (car entries))
	    nentry edge label oedge)

	;; Loop through the entries
	(setq entries (cdr entries))
	(while entries
	  (setq nentry (car entries))

	  ;; Loop through the new entry
	  (while nentry
	    (setq edge (car nentry))
	    (setq label (car edge))
	    (setq oedge (assoc label entry))
	    (if oedge
		(setcdr oedge (caml-list-union (cdr edge) (cdr oedge)))
	      (setq entry (cons edge entry)))
	    (setq nentry (cdr nentry)))
	  (setq entries (cdr entries)))
	entry)))

;;
;; Convert the DFA spec into a DFA vector.
;;
(defun caml-convert-dfa-spec (spec)
  "Convert the DFA spec into a DFA"
  (let (len dfa i l entry final edges)
    ;; Create the DFA vector
    (setq len (length spec))
    (setq dfa (make-vector len nil))

    ;; Look at all the states
    (setq i 0)
    (setq l spec)
    (while l
      (setq entry (cdr (car l)))
      (setq final (car entry))
      (setq edges (cdr entry))
      (aset dfa i (cons final (mapcar '(lambda (edge)
					 (caml-convert-dfa-spec-edge spec edge))
				      edges)))
      (setq l (cdr l))
      (setq i (1+ i)))

    dfa))

(defun caml-convert-dfa-spec-edge (spec edge)
  "Convert the edge spec into DFA form"
  (let ((label (car edge))
	(states (cdr edge))
	(i 0))

    ;; Search for the state
    (while (not (equal states (car (car spec))))
      (setq spec (cdr spec))
      (setq i (1+ i)))

    ;; Make the new edge
    (cons label i)))

;;
;; The final step is to minimize the automaton.
;; The minimization is as described in "Introduction to
;; Automata Theory, Languages, and Computation", by
;; Hopcroft & Ullman, Addison-Wesley, 1979, page 70.
;;
;; The algorithm is as follows:
;;    1. Build a table containing a flag for each pair of states
;;    2. Mark each pair containing one final and one non-final state
;;       as different, but unexplored
;;     
;;    3. Repeat the following until no more progress:
;;       a. For each pair of states p, q that are different
;;          but unexplored, find all:
;;             s: symbols
;;             x: states that lead to p on s
;;             y: states that lead to q on s
;;          mark all the x ass different from all the y,
;;          but unexplored.
;;       b. mark p, q as different, and explored
;;
;; We allocate a square table, but in fact we only use
;; half of it.  In the table:
;;    caml-unexplored: means the pair has not been explored
;;    caml-different: means the pair is different but unexplored
;;    caml-explored: means pair is different and explored
;;
(defun caml-minimize-dfa (dfa)
  "Optimize the DFA using the algorithm from Hopcroft & Ullman"
  (let ((len (length dfa))
	(rdfa (caml-reverse-dfa dfa))
	table i j entryi finali edgesi entryj finalj edgesj
	entry equals index)

    ;; Sort the edges in the DFA
    (caml-sort-edge-lists dfa)

    ;; Make the table
    (setq table (make-vector len nil))
    (setq i 0)
    (while (< i len)
      (aset table i (make-string i 0))
      (setq i (1+ i)))

    ;; Loop through all the states
    (setq i 0)
    (while (< i len)
      (setq j 0)
      (setq entryi (aref dfa i))
      (setq finali (car entryi))
      (setq edgesi (cdr entryi))
      (while (< j i)
	(setq entryj (aref dfa j))
	(setq finalj (car entryj))
	(setq edgesj (cdr entryj))
	(if (or (not (eq finali finalj)) (not (equal edgesi edgesj)))
	    (caml-mark-different-states dfa rdfa table i j))
	(setq j (1+ j)))
      (setq i (1+ i)))

    ;; Compute the equality table
    (setq equals (make-vector len nil))
    (setq i 0)
    (setq index 0)
    (while (< i len)
      (setq entry (aref table i))
      (setq j 0)
      (while (and (< j i) (/= (aref entry j) 0))
	(setq j (1+ j)))
      (aset equals i
	    (if (= j i)
		(- j index)
	      (setq index (1+ index))
	      (aref equals j)))
      (setq i (1+ i)))

    ;; Compute the new DFA
    (caml-squash-dfa dfa equals (- len index))))

;;
;; Sort the edges in the DFA.
;;
(defun caml-sort-edge-lists (dfa)
  "Sort the edges in the DFA"
  (let ((len (length dfa))
	(i 0)
	entry)
    (while (< i len)
      (setq entry (aref dfa i))
      (setcdr entry (sort (cdr entry) 'caml-compare-edges))
      (setq i (1+ i)))
    dfa))

(defun caml-compare-edges (edge1 edge2)
  "Compare two edges.  We base this first on the destination state,
then second on the label.  Symbols always come before lists."
  (let ((label1 (car edge1))
	(label2 (car edge2))
	(state1 (cdr edge1))
	(state2 (cdr edge2)))
    (cond ((< state1 state2) t)
	  ((> state1 state2) nil)
	  (t
	   (if (symbolp label1)
	       (if (symbolp label2)
		   (string< (symbol-name label1) (symbol-name label2))
		 t)
	     (if (symbolp label2)
		 nil
	       (caml-compare-edges label1 label2)))))))

;;
;; "Reverse" the automaton.  That is, for each state and
;; each symbol, find out which states lead into that state.
;;
(defun caml-reverse-dfa (dfa)
  "\"Reverse\" the DFA"
  (let ((len (length dfa))
	i table edges edge label state aentry)
    ;; Make a vector of alists
    (setq table (make-vector len nil))

    ;; Go through all the edges
    (setq i 0)
    (while (< i len)
      (setq edges (cdr (aref dfa i)))
      (while edges
	(setq edge (car edges))
	(setq label (car edge))
	(setq state (cdr edge))
	(setq aentry (assoc label (aref table state)))
	(if aentry
	    (setcdr aentry (caml-list-insert i (cdr aentry)))
	  (aset table state (cons (list label i) (aref table state))))
	(setq edges (cdr edges)))
      (setq i (1+ i)))

    table))

;;
;; Given a reversed DFA, mark all the states that differ
;; due to the initial states being different.
;;
;; l is a list of pair of state lists
;; In each pair, all the states in the first list
;; differ from alll the states in the second list.
;;
(defun caml-mark-different-states (dfa rdfa table i j)
  "Mark the states in the DFA that differ"
  (caml-mark-different-states-aux dfa rdfa table `(((,i) . (,j)))))

(defun caml-mark-different-states-aux (dfa rdfa table l)
  "Mark states in the DFA that differ"
  (while l
    (let ((statesa (car (car l)))
	  (statesb (cdr (car l)))
	  statea stateb label edgesa edgesb edgea edgeb sa sb tmp lb)
      (setq l (cdr l))

      ;; Loop through al combinations
      (while statesa
	(setq statea (car statesa))
	(setq lb statesb)
	(while lb
	  (setq stateb (car lb))

	  ;; Sort the states
	  (when (< statea stateb)
		(setq tmp statea)
		(setq statea stateb)
		(setq stateb tmp))

	  ;; Not different already
	  (when (= (aref (aref table statea) stateb) 0)
		(aset (aref table statea) stateb 1)

		;; Recursively mark different states
		(setq edgesa (aref rdfa statea))
		(setq edgesb (aref rdfa stateb))

		;; Loop through different states
		(while edgesa
		  (setq edgea (car edgesa))
		  (setq label (car edgea))
		  (setq sa (cdr edgea))
		  (setq edgeb (assoc label edgesb))
		  (when edgeb
			(setq sb (cdr edgeb))
			(setq l (cons (cons sa sb) l)))
		  (setq edgesa (cdr edgesa))))

	  (setq lb (cdr lb)))
	(setq statesa (cdr statesa))))))
	
;;
;; Squash bogus states out of the DFA.
;;
(defun caml-squash-dfa (dfa equals len2)
  "Squash unused states out of the DFA"
  (let ((dfa2 (make-vector len2 nil))
	(len (length dfa))
	(i 0)
	j entry)
    (while (< i len)
      (setq j (aref equals i))
      (unless (aref dfa2 j)
	      (setq entry (aref dfa i))
	      (setcdr entry (mapcar '(lambda (edge)
				       (cons (car edge) (aref equals (cdr edge))))
				    (cdr entry)))
	      (aset dfa2 j entry))
      (setq i (1+ i)))
    
    dfa2))

;;
;; Get the non-production edge labels.
;;
(defun caml-get-edge-labels (entry)
  "Get the non-production edge labels from the entry"
  (let (edge all label)
    (while entry
      (setq label (car (car entry)))
      (if (and (symbolp label) (not (memq label '(t $ nil))))
	  (setq all (cons label all)))
      (setq entry (cdr entry)))
    all))

;;
;; Adjust precedences for production that do
;; not lead to final states.
;;
(defun caml-adjust-precedences (dfa)
  "Adjust precedences of production edges and compute forward terminals."
  (let ((len (length dfa))
	(i 0)
	final entry edge entry2)
    (while (< i len)
      (setq entry (cdr (aref dfa i)))
      (while entry
	(setq edge (car entry))
	(when (consp (car edge))
	      ;; This is a production edge
	      (setq entry2 (aref dfa (cdr edge)))
	      (setcar edge `(,(car (car edge))
			     ,(if (car entry2) (cdr (car edge)) 0)
			     . ,(caml-get-edge-labels (cdr entry2)))))
	(setq entry (cdr entry)))
      (setq i (1+ i)))
    dfa))

;;
;; Compile the syntax description into a pattern.
;;
(defun caml-compile-syntax (syntax)
  "Compile the CAML syntax description into a NFA"
  (let ((automata (mapcar '(lambda (x)
			     (let ((prec (car x))
				   (pattern (cons 'seq (nthcdr 2 x))))
			       (caml-compile-pattern prec pattern)))
			  syntax))
	dfa nfa)
    (setq nfa (caml-alternate-nfa automata))
    (setq nfa (caml-link-final-states nfa))
    (setq nfa (caml-remove-epsilon-edges nfa))
    (setq dfa (caml-dfa-of-nfa nfa))
    (setq dfa (caml-minimize-dfa dfa))
    (caml-adjust-precedences dfa)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRAILER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'caml-fa)
