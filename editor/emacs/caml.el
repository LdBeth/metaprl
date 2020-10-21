;;
;; This is a major mode for editing CAML files.
;;

(require 'caml-util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Variables for the indentation
;;
(defvar caml-indent-level 3
  "Indentation of blocks in CAML.")

(defvar caml-pipe-indent 1
  "Extra indentation for lines beginning with |.")

;;
;; Hooks
;;
(defvar caml-mode-hook nil
  "Hooks to run when entering CAML mode")

(defvar caml-load-hook nil
  "Hooks to run when CAML mode is loaded")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; We allow this mode to be restricted to a portion of the buffer.
;;
(defvar caml-point-min nil
  "Beginning of buffer region for caml mode")

(defvar caml-point-max nil
  "End of buffer region for caml mode")

;;
;; Syntax table
;;
(defvar caml-mode-syntax-table nil
  "Syntax table used while in CAML mode.")

(if caml-mode-syntax-table
    ()
  (setq caml-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_  "w"    caml-mode-syntax-table)
  (modify-syntax-entry ?\( "()1"  caml-mode-syntax-table)
  (modify-syntax-entry ?\) ")(4"  caml-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23" caml-mode-syntax-table)
  (modify-syntax-entry ?\' "."    caml-mode-syntax-table)

  ;; Make all these symbols part of punctuation class.
  ;; "Operators" are a sequence of punctuation chars.
  (modify-syntax-entry ?= "." caml-mode-syntax-table)
  (modify-syntax-entry ?< "." caml-mode-syntax-table)
  (modify-syntax-entry ?> "." caml-mode-syntax-table)
  (modify-syntax-entry ?@ "." caml-mode-syntax-table)
  (modify-syntax-entry ?^ "." caml-mode-syntax-table)
  (modify-syntax-entry ?| "." caml-mode-syntax-table)
  (modify-syntax-entry ?& "." caml-mode-syntax-table)
  (modify-syntax-entry ?~ "." caml-mode-syntax-table)
  (modify-syntax-entry ?+ "." caml-mode-syntax-table)
  (modify-syntax-entry ?- "." caml-mode-syntax-table)
  (modify-syntax-entry ?/ "." caml-mode-syntax-table)
  (modify-syntax-entry ?$ "." caml-mode-syntax-table)
  (modify-syntax-entry ?% "." caml-mode-syntax-table)
  (modify-syntax-entry ?\; "." caml-mode-syntax-table))

;;
;; No abbreviations in this mode.
;;
(defvar caml-mode-abbrev-table nil
  "Abbrev table used while in CAML mode.")
(define-abbrev-table 'caml-mode-abbrev-table ())

;;
;; Key definitions
;;
(defvar caml-mode-map () "Keymap used while in CAML mode.")

(if caml-mode-map
    ()
  (setq caml-mode-map (make-sparse-keymap))
  (define-key caml-mode-map "\t" 'caml-indent-line)
  (define-key caml-mode-map "\M-\C-\\" 'caml-indent-region)
  (define-key caml-mode-map "\M-|" 'caml-electric-pipe)
  (define-key caml-mode-map "\r"   'caml-electric-cr)
  (define-key caml-mode-map "\M-q" 'caml-justify-block))

;;
;; Start CAML mode.
;;
(defun caml-mode ()
  "\
Mode for \"editing\" CAML code.  You can move using the usual
cursor motion commands.\\<caml-mode-map>.

Customization variables (rename this buffer and type \\[describe-variable] on each line
for more info):

   caml-indent-level  (amount of indentation per level)
   caml-pipe-indent   (extra indentation for lines beginning with a |)

Hooks (use \\[describe-variable] to see their documentation):

  caml-mode-hook
  caml-load-hook

Keybindings:
\\{caml-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map caml-mode-map)
  (setq mode-name "CAML")
  (setq major-mode 'caml-mode)
  (setq local-abbrev-table caml-mode-abbrev-table)
  (set-syntax-table caml-mode-syntax-table)
  (setq indent-tabs-mode nil)
  (setq case-fold-search nil)

  ;; Restrict to a region
  (make-local-variable 'caml-point-min)
  (make-local-variable 'caml-point-max)
  (setq caml-point-min (point-min-marker))
  (setq caml-point-max (point-max-marker))

  ;; How may reset the markers
  (run-hooks 'caml-mode-hook)

  ;; First pass over file
  (caml-highlight-tokens))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REGION RESTRICTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Narrow to the CAML region.
;; It is an error if the point is outside the region.
;;
(defun caml-narrow-to-region ()
  "Narrow to the CAML region.  Signal an error if the
point is outside the region."
  (let ((here (point)))
    (if (and (>= here caml-point-min) (<= here caml-point-max))
	(narrow-to-region caml-point-min caml-point-max)
      (error "Point is outside CAML region."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INDENTATION COMMANDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; To indent a line, calculate the ammount of
;; indentation, then add that many space before the line
;;
(defun caml-indent-line ()
  "Indent current line of CAML code."
  (interactive)
  (let ((indent (caml-calculate-indentation)))
    (caml-indent-to indent)
    (caml-indent-early-point)))

;;
;; May want to use this function to insert a pipe at
;; the correct indentation.
;;
(defun caml-electric-pipe ()
  "Insert and indent a pipe."
  (interactive)
  (save-restriction
    (caml-narrow-to-region)
    (if (save-excursion
	  (skip-chars-backward "\t ")
	  (bolp))
	(insert "| ")
      (insert "\n| "))
    (caml-indent-line)))

;;
;; Use this to indent the line on a cr.
;;
(defun caml-electric-cr ()
  "Insert and indent a CR."
  (interactive)
  (save-restriction
    (caml-narrow-to-region)
    (caml-indent-line)
    (insert "\n")))

;;
;; Set the indentation of the current line.
;;
(defun caml-indent-to (indent)
  "Set the indentation of the current line"
  (if (/= (current-indentation) indent)
      (save-excursion
	(let ((beg (progn (beginning-of-line) (point))))
	  (skip-chars-forward "\t ")
	  (delete-region beg (point))
	  (indent-to indent)))))

;;
;; Set the point to the current indentation if it
;; is before that.
;;
(defun caml-indent-early-point ()
  "Set the point at the current indentation"
  (if (< (current-column) (current-indentation))
      (skip-chars-forward "\t ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SYNTAX SPECIFICATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; This specifications provide a rough syntax of the
;; language.  Each entry in this table is essentially a
;; prduction in the language, simplified to use a simplified
;; regular expression syntax.
;;
;; Each entry list is a production.  The first elements in the list is the
;; precedence of the production, the second is the whether this production
;; start an indentation block.  If it doesn't, the indentation falls back
;; to the innermost block that specifies indentation.
;;
;; The remaining elements specify the syntax of the production.
;;    1. The symbol & specifies an indented slot
;;    2. The symbol &- specifies an unindented slot
;;    3. The symbol &sym specifies a slot that is not
;;       indented if it begins with the specified symbol
;;    4. Each word specifies a terminal
;;    5. Each list specifies a group with the command:
;;       seq: the remaining args are matched in order
;;       alt: one of the remaining args should match
;;       *: the args must match in order zero or more times
;;       +: the args must match in order one or more times
;;       opt: the args must match in order zero or one times
;;
;; Each terminal in the production has the same indentation
;;
(defconst caml-syntax
  '((10 nil &+ = &)
    (10 nil &+ == &)
    (10 nil &+ := &)
    (10 nil &+ <- &)
    (10 nil &+ @ &)
    (10 nil &+ % &)
    (10 nil &+ \(**\) &)
    (10 nil &+ ^ &)
    (6  nil &+ \; &-)
    (0  nil &+ \;\;)
    (20 t   (opt &+) word)
    (20 t   (opt &+) \( & \))
    (20 t   (opt &+) \(| & |\))
;   (20 t   (opt &+) \[ & \])
    (20 t   (opt &+) \[| & |\])
    (20 t   (opt &+) { & })
    (20 t   (opt &+) (alt \(\) \[\]))

    ; This is problematic
    (4  t   &+ with &)

    ;; Caml productions
    (5  t   (opt &+) let & = & (* and & = &) (opt in &let))
    (5  t   (opt &+) method & (alt = :) &)
    (5  t   (opt &+) type & (opt (alt = ==) & (* | &)) (* and & (opt (alt = ==) & (* | &))))
    (5  t   try & with & (* | &) -> &2 (* (+ | &) -> &2))
    (5  t   match & with & (* | &) -> &2 (* (+ | &) -> &2))
    (5  nil (alt function fun) & (* | &) -> &2 (* (+ | &) -> &2))
    (8  t   if & then & (opt else &if))
    (1  t   IFDEF & THEN & END)
    (1  t   for & (alt to downto) & do & done)
    (1  t   while & do & done)
    (5  t   (opt &+) (alt val open extends include exception external) &)
    (4  t   (opt &+) module (opt type) word (* \( & : & \)) (opt : &) (opt = &))
    (4  t   (opt &+) class (opt type) (opt &) (opt = &) (* and & = &))
    (5  t   (opt &+) magic_block word = &)
    (1  nil sig & end)
    (1  nil struct & end)
    (1  nil object \( & \) & end)
    (10 nil &+ -> &)
    (20 nil (opt &+) begin & end)
    (20 nil (opt &+) termbegin & end)

    ;; Extra syntax for PRL mode
    (10 nil &+ :: &)
    (15 nil &+ : &)
    (21 nil &+ \. &)
    (6  nil &+ (alt --> <--> <--) &)
    (5  t   (alt dform condition) & (opt = &))
    (5  t   (alt prec infix lex_token production parser) &)
    (5  t   (opt &+) declare & (opt end))
    (5  t   ml_rewrite & : & (opt == &))
    (5  t   (alt define rewrite axiom primrw rule) & : &)
    (4  t   (alt all exst) & : & \. &)
    (5  t   (alt prim interactive ml_rule) & : & : & = &)
    (5  t   mlterm & = & | &)

    ;; Extra syntax for Camlp4
    (1   t   (opt &+) EXTEND & END)
    (20  t   (opt &+) \[ & (opt -> &) (* | & (opt -> &)) \]))
  "This list is a description of the syntax of the program")

(defconst caml-initial-terminals
  '(let type val value open extends include exception module class method
	dform condition prec declare define rewrite axiom primrw prim
	infix external magic_block mlterm lex_token production parser)
  "Tokens that may start a top level production")

(defconst caml-special-terminals
  '(\(**\) \(| \[| {| |\) |\] |} \(\) \[\])
  "Tokens that are not words")

;;
;; Comment syntax
(defconst caml-comment-start "(\\*"
  "Beginning of a comment")

(defconst caml-comment-end "\\*)"
  "End of a comment")

(defconst caml-char-constant-pattern
  "'\\(.\\|\\\\.\\|...\\)'"
  "Pattern that matches character constants")

;;
;; Quotation syntax
;;
(defconst caml-quotation-start "<<\\|<:[A-z0-9_]+<"
  "Beginning of a quotation")

(defconst caml-quotation-end ">>"
  "End of a quotation")

;;
;; Syntax tables are computed at compile time.
;;
(eval-when-compile (require 'caml-fa))

(defconst caml-terminals (eval-when-compile
			   (caml-get-all-terminals caml-syntax))
  "All the terminals in CAML syntax")

(defconst caml-terminal-exp (eval-when-compile
			      (caml-squash-terminals caml-terminals))
  "The regex for all the terminals in the language")

(defconst caml-special-terminal-exp
  (eval-when-compile
    (caml-squash-terminals
     (mapcar 'symbol-name caml-special-terminals)))
  "The regex for the special terminals")

(defconst caml-initial-terminal-exp
  (eval-when-compile
    (concat "^\\("
	    (caml-squash-terminals
	     (mapcar 'symbol-name caml-initial-terminals)
	     "\\b")
	    "\\)"))
  "The regex for all the terminals in the language")

(defconst caml-terminal-precedences (eval-when-compile
				      (caml-get-terminal-precedences caml-syntax))
  "Precedence of each token")

;;
;; Calculate symbols that will cause indentation when starting a production.
;;
(defconst caml-terminal-indenters (eval-when-compile
				    (caml-get-terminal-indenters caml-syntax))
  "Tokens that cause indentation when beginning a production")

;;
;; Here is the pushdown automaton for the program.
;;
(defconst caml-pa (eval-when-compile
		     (caml-compile-syntax caml-syntax))
  "This the the pushdown automaton for CAML mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LEXING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; We assume for all the lexing commands that narrowing is in place.
;; That is, somewhere above (caml-narrow-to-region) has been called.
;;

;;
;; Get the next token in the file.
;; We scan normal words until something different is found,
;; then we return the start of the new token, and the token.
;;
(defun caml-next-token ()
  "Return the next token in the file and move past it.
Return nil if there are no more tokens"
  (let (flag token pos spec tag end over)
    (while (not flag)
      (skip-syntax-forward " ")
      (setq pos (point))
      (setq spec (caml-looking-at-token-end))
      (setq tag (car spec))
      (setq end (cdr spec))

      ;; If its a token or op, intern it
      (cond ((eq tag 'eof) (setq flag 'eof))
	    ((memq tag '(token op))
	     (setq token (buffer-substring pos end))
	     (set-text-properties 0 (- end pos) nil token)
	     (setq flag (cond ((member token caml-terminals)
			       (caml-highlight-token pos end 'font-lock-keyword-face)
			       (intern token))
			      (t 'word))))
	    ((eq tag 'comment)
	     (setq over (caml-highlight-token pos end 'font-lock-comment-face 2))
	     (overlay-put over 'comment pos))
	    (t
	     (setq over (caml-highlight-token pos end 'font-lock-string-face))
	     (overlay-put over 'constant pos)
	     (setq flag 'word))))
    (cons pos flag)))

;;
;; Simplified version of caml-next-token that is just intended to
;; highligh all the tokens in the file.
;;
(defun caml-highlight-tokens ()
  "Highlight all the tokens in the file."
  (save-excursion
    (goto-char caml-point-min)
    (save-restriction
      (caml-narrow-to-region)

      ;; Loop through the file
      (let (flag pos spec tag end token over)
	(while (not flag)
	  (skip-syntax-forward " ")
	  (setq pos (point))
	  (setq spec (caml-looking-at-token-end))
	  (setq tag (car spec))
	  (setq end (cdr spec))

	  ;; Switch cases
	  (cond ((eq tag 'eof) (setq flag t))
		((memq tag '(token op))
		 (setq token (buffer-substring pos end))
		 (set-text-properties 0 (- end pos) nil token)
		 (if (member token caml-terminals)
		     (caml-highlight-token pos end 'font-lock-keyword-face)))
		((eq tag 'comment)
		 (setq over (caml-highlight-token pos end 'font-lock-comment-face 2))
		 (overlay-put over 'comment pos))
		(t
		 (setq over (caml-highlight-token pos end 'font-lock-string-face))
		 (overlay-put over 'constant pos))))))))

(defun caml-highlight-token (pos end face &optional priority)
  "Highlight the specified text in the buffer with an overlay."
  (let ((over (make-overlay pos end)))
    (overlay-put over 'caml t)
    (overlay-put over 'face face)
    (overlay-put over 'evaporate t)
    (overlay-put over 'priority (or priority 1))
    over))

;;
;; Find the token at the current point.
;; This is essentailly a lexer for the next token
;; in the file.  We lex the following items:
;;    1. Comments
;;    2. String constants
;;    3. Char constants
;;    4. Words
;;    5. Operators (punctuation syntax)
;;
;; We are not looking at whitespace.
;;
(defun caml-looking-at-token-end ()
  "Find the length of the next token in the file"
  (let ((here (point))
	next)
    ;; Literals
    (cond ((eobp) (cons 'eof here))
	  ((looking-at caml-char-constant-pattern)
	   (cons 'char (caml-char-constant-end)))
	  ((looking-at "\"")
	   (cons 'string (caml-string-constant-end)))
	  ((looking-at caml-special-terminal-exp)
	   (re-search-forward caml-special-terminal-exp)
	   (cons 'token (point)))
	  ((looking-at caml-comment-start)
	   (cons 'comment (caml-comment-end)))
	  ((looking-at caml-quotation-start)
	   (cons 'comment (caml-quotation-end)))
	  ((or (looking-at "^#") (looking-at "\\\\$"))
	   (end-of-line)
	   (cons 'comment (point)))
	  (t
	   ;; Try reading an operator
	   (skip-syntax-forward ".")
	   (if (> (point) here)
	       (cons 'op (point))
	     ;; Try reading a word
	     (skip-syntax-forward "w_'")
	     (if (> (point) here)
		 (cons 'token (point))

	       ;; Try a match with a ketword
	       (cond ((looking-at caml-terminal-exp)
		      (re-search-forward caml-terminal-exp)
		      (cons 'token (point)))
		     (t
		      ;; Just return this as a char
		      (forward-char 1)
		      (if (> (point) here)
			  (cons 'op (point))
			(cons 'eof (point)))))))))))

;;
;; Find the end of a char constant.
;; Be careful about escaped values.
;;
(defun caml-char-constant-end ()
  "Move to the end of the char constant at the point."
  (if (looking-at "'")
      (forward-char 1))
  (while (progn
	   (skip-chars-forward "^\\'")
	   (cond ((looking-at "'")
		  (forward-char 1)
		  nil)
		 (t
		  (forward-char 2)
		  t))))
  (point))

;;
;; Find the end of the string constant.
;; Don't have to be as careful as with chars because
;; emacs knows about string syntax.
;;
(defun caml-string-constant-end ()
  "Move to the end of this string constant"
  (if (looking-at "\"")
      (forward-char 1))
  (while (progn
	   (skip-syntax-forward "^\\\"")
	   (cond ((looking-at "\"")
		  (forward-char 1)
		  nil)
		 ((looking-at "'")
		  (forward-char 1)
		  t)
		 (t
		  (forward-char 2)
		  t))))
  (point))

;;
;; Find the end of the comment.
;; The problem here is that comments may be nested, and
;; char and string lterals matter.
;;
(defun caml-comment-end ()
  "Move to the end of this comment"
  (interactive)
  (if (looking-at caml-comment-start)
      (re-search-forward caml-comment-start))
  (let ((level 1)
	(search (concat "\"\\|'\\|\\("
			caml-comment-start "\\)\\|\\("
			caml-comment-end "\\)"))
	(end (point-max)))
    (while (progn
	     (when (re-search-forward search nil 0)
		   (setq end (point))
		   (goto-char (match-beginning 0)))
	     (cond ((looking-at "'")
		    (if (looking-at caml-char-constant-pattern)
			(goto-char (caml-char-constant-end))
		      (forward-char 1)))
		   ((looking-at "\"")
		    (goto-char (caml-string-constant-end)))
		   ((looking-at caml-comment-start)
		    (setq level (1+ level))
		    (goto-char end))
		   ((looking-at caml-comment-end)
		    (setq level (1- level))
		    (goto-char end)))
	     (not (or (= level 0) (eobp)))))
    (point)))

;;
;; Find the end of the quotation.
;; The problem here is that quotations may be nested, and
;; char and string literals matter.
;;
(defun caml-quotation-end ()
  "Move to the end of this quotation"
  (interactive)
  (if (looking-at caml-quotation-start)
      (re-search-forward caml-quotation-start))
  (let ((level 1)
	(search (concat "\"\\|'\\|\\("
			caml-quotation-start "\\)\\|\\("
			caml-quotation-end "\\)"))
	(end (point-max)))
    (while (progn
	     (when (re-search-forward search nil 0)
		   (setq end (point))
		   (goto-char (match-beginning 0)))
	     (cond ((looking-at "'")
		    (if (looking-at caml-char-constant-pattern)
			(goto-char (caml-char-constant-end))
		      (forward-char 1)))
		   ((looking-at "\"")
		    (goto-char (caml-string-constant-end)))
		   ((looking-at caml-quotation-start)
		    (setq level (1+ level))
		    (goto-char end))
		   ((looking-at caml-quotation-end)
		    (setq level (1- level))
		    (goto-char end)))
	     (not (or (= level 0) (eobp)))))
    (point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Parse the entire file.
;;
(defun caml-parse-file ()
  "Parse the entire CAML file."
  (interactive)
  (save-excursion
    (save-restriction
      (caml-narrow-to-region)
      (caml-remove-overlays)
      (goto-char (point-min))
      (let (flag code)
	(while (not flag)
	  (setq code (caml-parse-production))
	  (cond (code (caml-add-production code))
		(t (setq flag t))))))))

;;
;; Add the specified production.
;;
(defun caml-add-production (code)
  "Add the production specified by the code returned by caml-pare-production."
  (let ((stack code)
	over)
    (setq code (cdr (car code)))
    (setq over (make-overlay (car code) (cdr code)))
    (overlay-put over 'priority 0)
    (overlay-put over 'face 'default)
    (overlay-put over 'caml t)
    (overlay-put over 'production t)
    (overlay-put over 'modification-hooks (list 'caml-modify-production-hook))
    (if stack (overlay-put over 'stack stack))
    over))

;;
;; We implement a pushdown automaton to parse input text.
;; Keep the stack, and end this production when another production
;; is started.  Return a list of stacks, together with the bounds
;; of the text for the stack.
;;
(defun caml-parse-production (&optional max save justify)
  "Parse the next production in the CAML file"
  (let ((stack (if save (car (car save)) '((0 0 0))))
	savepos flag code start end bol pos token)

    ;; Get the first token
    (setq code  (caml-next-token))
    (setq pos   (car code))
    (setq token (cdr code))
    (setq savepos (+ pos 1000))
    (cond ((and max (> (point) max))
	   ;; First token is past max point
	   ;(cons (cons stack (cons pos pos)) save))
	   save)

	  ((eq token 'eof) nil)

	  (t (setq start pos)
	     (setq code  (caml-accept stack pos token justify))
	     (setq bol   (car code))
	     (setq stack (cdr code))
	     (and justify bol
		  (caml-justify pos (if (eq token '|) (+ bol caml-pipe-indent) bol)))

	     ;; Loop through the rest of the production
	     (cond (stack
		    (setq bol nil)
		    (while (and stack (not flag))
		      (setq end (point))

		      ;; Checkpoint?
		      (cond ((> end savepos)
			     (setq save (cons (cons stack (cons start (point-marker))) save))
			     (setq savepos (+ end 1000))))

		      ;; Look at the next token
		      (setq code (caml-next-token))
		      (cond ((and max (> (point) max))
			     (setq flag t))

			    (t
			     (setq pos   (car code))
			     (setq token (cdr code))

			     ;; Possibly accept the next token
			     (setq code  (caml-accept stack pos token justify))
			     (setq bol   (car code))
			     (if (and (eq bol 0) (member token caml-initial-terminals))
				 ;; This token belongs to the next production
				 (setq flag t)

			       ;; Collect this token
			       (setq stack (cdr code))
			       (and justify bol
				    (caml-justify pos (if (eq token '|)
							  (+ bol caml-pipe-indent)
							bol)))))))

		    ;; Return saved stack
		    (goto-char end)
		    (cons (cons stack (cons start (point-marker))) save))

		   ;; End of the production
		   (t nil))))))

;;
;; This justification is easy:
;; Given the indentation we want, and the actual indentation,
;; adjust the indentation.
;;
(defun caml-justify (pos bol)
  "Change justification of the current line"
  (save-excursion
    (goto-char pos)
    (caml-indent-to bol)))

;;
;; Process the next token.
;; This function takes a stack from caml-parse-production, and
;; computes the next state given a token.  There is a lot going on here:
;;    1. If the token can't be accepted by the top production,
;;       because of a syntax error, or because the token has
;;       lower precedence, reduce the top production and try again.
;;    2. Calculate the indentation:
;;       If the token can be accepted, indent if the token belongs
;;       to the indenters, and if the machine is in the
;;       initial state, or if it was in the final state.
;;
(defun caml-accept (stack pos token &optional justify)
  "Process the next token in the input file"
  (let (newstack top state prec entry final edge
		 entry2 edge2 new newprec
		 push bol col colx shift syms
		 token-prec)

    ;; Loop until a production will take the symbol
    (while (and (not newstack) stack)
      ;; Get state info
      (setq top (car stack))
      (setq state (car top))
      (setq prec (nth 1 top))
      (setq entry (aref caml-pa state))
      (setq final (car entry))
      (setq edge (assq token (cdr entry)))
      (setq push (not (numberp (nth 2 top))))
      (setq syms (nthcdr 3 top))

      ;; determine shift or reduce
      (if edge
	  ;; Shift this symbol
	  (setq new (cdr edge))
	;; Can't shift directly, check for a binary operator
	(cond (final
	       (setq entry (aref caml-pa (cdr (assq '$ (cdr entry)))))
	       (setq edge (assq token (cdr entry)))
	       (setq token-prec (cdr (assq token caml-terminal-precedences)))
	       (if (or (not edge)
		       (memq token syms)
		       (< token-prec prec)
		       (and (= token-prec prec) (memq token caml-initial-terminals)))
		   ;; Reduce the top production, and continue
		   (if (not (setq stack (cdr stack)))
		       ;; Stack is empty, so do not accept this token
		       (progn (message "Syntax error at %d" pos) (goto-char pos)))
		 ;; Continue with this production
		 (setq shift t)
		 ;(setq push t)
		 (setq new (cdr edge))))
	      (t
	       ;; This production is finished (syntax error)
	       (message "Syntax error at %d" pos)
	       (if (not (setq stack (cdr stack)))
		   ;; Stack is empty, so do not accept this token
		   (goto-char pos)))))

      ;; Moved to a new state
      (when new
	    ;; Compute the new column
	    (setq bol (caml-first-token-p pos))
	    (if justify
		;; Also calculate the expected indentation
		(cond (push
		       (setq col (caml-new-indentation stack bol pos token))
		       (if bol (setq colx col)))
		      (bol
		       (setq colx (nth 2 top))
		       (setq col colx))
		      (t
		       (setq colx nil)
		       (setq col (nth 2 top))))
	      ;; Reset the current column
	      (cond (push
		     (setq col (caml-set-indentation stack bol pos token)))
		    (bol
		     (setq col (if (eq token '|) (- bol caml-pipe-indent) bol)))
		    (t
		     (setq col (nth 2 top)))))

	    ;; If a production, add a new state
	    (setq edge (car (cdr (aref caml-pa new))))
	    (cond ((consp (car edge))
		   ;; This is a new production
		   (setq new (cdr edge))
		   (setq newprec (nth 1 (car edge)))

		   ;; This statement makes all productions left-recursive
		   (if (and (>= newprec prec) shift) (setq newprec (1+ newprec)))

		   ;; New production
		   (setq newstack
			 `((0 ,newprec ,(car (car edge)) . ,(nthcdr 2 (car edge)))
			   (,new ,prec ,col . ,syms)
			   . ,(cdr stack))))

		  (t
		   ;; New state
		   (setq newstack `((,new ,prec ,col . ,syms) . ,(cdr stack)))))))

    (cons colx newstack)))

;;
;; This function is just for testing the automaton on a list
;; of tokens.
;;
(defun caml-test (tokens)
  "This function tests the automaton on a list of tokens."
  (beginning-of-line)
  (let ((stack '(0 (0 0 1)))
	(pos (point))
	token)
    (while tokens
      (setq token (car tokens))
      (cond ((numberp token) (setq pos (+ (point) token)))
	    (t
	     (setq stack (caml-accept (cdr stack) pos token))
	     (setq pos (+ pos 4))))
      (setq tokens (cdr tokens)))
    stack))

;;
;; Calculate the expected indentation setting when a new production
;; is shifted.
;;
;; If the token is an indentor, then it sets the current
;; indentation.  Otherwise, the indentation is inherited from the
;; previous production.
;;
(defun caml-new-indentation (stack bol pos token)
  "Calculate the new indentation setting for the new production."
  (if stack
      (let ((col (nth 2 (car stack)))
	    (indent (if (cdr stack) (nth 2 (car (cdr stack))) 0)))
	(if bol
	    (if (or (eq col nil) (eq col token) (not (memq token caml-terminal-indenters)))
		(if (consp col) (+ indent (* (1- (car col)) caml-indent-level)) indent)
	      (+ indent (if (consp col) (* (car col) caml-indent-level) caml-indent-level)))
	  (if (or (eq col nil) (eq col token) (not (memq token caml-terminal-indenters)))
	      indent
	    (save-excursion (goto-char pos) (current-column)))))
    0))

;;
;; Take the indentation from the stack and the position of the current
;; token.
;;
(defun caml-set-indentation (stack bol pos token)
  "Set the indentation from the current token"
  (if stack
      (let ((col (nth 2 (car stack)))
	    (indent (if (cdr stack) (nth 2 (car (cdr stack))) 0)))
	(if bol
	    (if (or (eq col nil) (eq col token) (not (memq token caml-terminal-indenters)))
		indent
	      bol)
	  (if (or (eq col nil) (eq col token) (not (memq token caml-terminal-indenters)))
	      indent
	    (save-excursion (goto-char pos) (current-column)))))
    0))

;;
;; Check if the position is at the beginning of a line.
;; If so, return the column.
;;
(defun caml-first-token-p (pos)
  "Check if POS is at the beginning of the line"
  (save-excursion
    (goto-char pos)
    (skip-chars-backward " \t")
    (cond ((bolp)
	   (goto-char pos)
	   (save-restriction
	     (widen)
	     (current-column)))
	  (t nil))))

;;
;; Remove all the caml overlays in the file.
;;
(defun caml-remove-overlays (&optional start end)
  "Remove all the caml overlays in the specified range"
  (if (not start) (setq start (point-min)))
  (if (not end) (setq end (point-max)))
  (let (over l (flag t))
    (while (and (<= start end) flag)
      (setq l (overlays-at start))
      (while l
	(setq over (car l))
	(if (overlay-get over 'caml)
	    (delete-overlay over))
	(setq l (cdr l)))
      (if (= start end)
	  (setq flag nil)
	(setq start (next-overlay-change start))))))

;;
;; This hook is called when a production is modified.
;; We basically reparse the production up to the point of
;; modification, and leave the rest alone until the next indentation
;; command.
;;
(defvar *modify-count* 0)

(defun caml-modify-production-hook (over flag &rest args)
  "This hook is called when a production is modified"
  (setq *modify-count* (1+ *modify-count*))
  (if flag
      ;; Move the production back
      (save-restriction
	(caml-narrow-to-region)
	(caml-repeal-production over (car args)))))

;;
;; Given a position, find the production, and repeal it before the position.
;;
(defun caml-repeal-current-production ()
  "Repeal the production overlay surrounding the current point."
  (let ((l (overlays-at (point))) over)
    (while l
      (setq over (car l))
      (cond ((and (overlay-get over 'caml) (overlay-get over 'production))
	     (caml-repeal-production over (point))
	     (setq l nil))
	    (t (setq l (cdr l)))))))

;;
;; Move the overlay back before the specified position.
;; Look in the production for a previous stack not
;; too far away.
;;
(defun caml-repeal-production (over pos)
  "Move the overlay back before the specified position"
  (save-excursion
    (let (start info)
      ;; Get a previous stack if it exists
      (setq info (overlay-get over 'stack))
      (while (and info (> (cdr (cdr (car info))) pos))
	(setq info (cdr info)))

      ;; Have a previous stack?
      (cond (info
	     ;; Repeal to the previous point
	     (setq start (cdr (cdr (car info))))
	     (goto-char start)
	     (caml-remove-faces pos)
	     (overlay-put over 'stack info)
	     (setq info (caml-parse-production pos info)))

	    (t
	     ;; Go back a reparse the entire production
	     (setq start (overlay-start over))
	     (goto-char start)
	     (caml-remove-faces pos)
	     (setq info (caml-parse-production pos))))

      ;; Save the new stack
      (cond (info
	     (move-overlay over (overlay-start over) (cdr (cdr (car info))))
	     (overlay-put over 'stack info))
	    (t
	     (delete-overlay over))))))

;;
;; Delete the faces up to the specified position.
;;
(defun caml-remove-faces (end)
  "Find faces in the specified overlay."
  (let ((start (point))
	over l)

    ;; Find them
    (while (<= start end)
      (setq l (overlays-at start))
      (while l
	(setq over (car l))
	(if (overlay-get over 'caml)
	    (if (not (overlay-get over 'production))
		(delete-overlay over)))
	(setq l (cdr l)))
      (setq start (next-overlay-change start)))))

;;
;; Calculate the indentation for the current line.
;; Several steps:
;;    1. If we are in a protected production, repeal it to the last line.
;;    2. Search for the end of the last partial production.
;;    3. Extend it to the last line
;;    4. Get the indentation and indent this line
;;
(defun caml-calculate-indentation ()
  "Calculate the indentation of the current line."
  (let (over stack col code pos token here)
    ;; Split the current production
    (save-excursion
      (save-restriction
	(caml-narrow-to-region)
	(beginning-of-line)
	(setq here (point))
	(forward-char -1)

	;; Split the current production
	(caml-repeal-current-production)

	;; Find the previous production
	(if (setq over (caml-find-previous-production))
	    ;; Extend it to the current position
	    (setq stack (caml-extend-production over)))

	;; Check if in a constant or comment
	(goto-char here)
	(let ((l (overlays-at (point))))
	  (while (and (not col) l)
	    (setq over (car l))
	    (if (overlay-get over 'caml)
		(cond ((setq col (overlay-get over 'comment))
		       (setq col (save-excursion
				   (goto-char col)
				   (current-column)))
		       (if (not (looking-at caml-comment-start))
			   (setq col (1+ col))))
		      ((overlay-get over 'constant)
		       (setq col 0))))
	    (setq l (cdr l))))

	;; Comment or constant?
	(if col
	    col

	  ;; Otherwise, get the next token
	  ;; HACK: for non-caml, 'let should be 'word
	  (setq code (caml-next-token-on-line))
	  (if (not code) (setq code (cons (point) 'let)))

	  ;; Have the current column
	  (setq pos (car code))
	  (setq token (cdr code))
	  (setq stack (caml-accept stack pos token t))
	  (setq col (car stack))
	  (if (not (numberp col)) (setq col 0))
	  (if (eq token '|) (+ col caml-pipe-indent) col))))))

;;
;; Get the next token on this line
;;
(defun caml-next-token-on-line ()
  "Get the next token on the current line"
  (save-excursion
    (let (code eol)
      (end-of-line)
      (setq eol (point))
      (beginning-of-line)
      (setq code (caml-next-token))
      (if (or (> (point) eol) (eq (cdr code) 'eof))
	  nil
	(goto-char (car code))
	code))))

;;
;; Find the most recent production.
;;
(defun caml-find-previous-production ()
  "Find the previous production."
  (save-excursion
    (let ((here (point))
	  min over l item pos)

      ;; Bound the search
      (setq min (if (looking-at caml-initial-terminal-exp)
		    here
		  (re-search-backward caml-initial-terminal-exp nil 0)
		  (point)))

      ;; Search back
      (setq pos here)
      (while (and (not over) (> pos min))
	(setq l (overlays-at pos))
	(while l
	  (setq item (car l))
	  (cond ((and (overlay-get item 'caml) (overlay-get item 'production))
		 (setq over item)
		 (setq l nil))
		(t (setq l (cdr l)))))
	(setq pos (previous-overlay-change pos)))

      ;; Use an old one or construct a new one
      (if over
	  over
;	  (if (or (overlay-get over 'stack) (>= (overlay-end over) here))
;	      over
;	    ;; The previous production is complete, so make a new one
;	    (goto-char (overlay-end over))
;	    (caml-remove-faces here)
;	    (caml-add-production (caml-parse-production here)))

	;; Compute a new one
	(goto-char min)
	(caml-remove-faces here)
	(let ((stack (caml-parse-production here)))
	  (if stack
	      (setq over (caml-add-production stack))
	    nil))))))

;;
;; Extend the previous production to the current point.
;;
(defun caml-extend-production (over)
  "Extend the specified production to the current point"
  (save-excursion
    (let ((stack (overlay-get over 'stack))
	  (end (overlay-end over))
	  (here (point))
	  code)
      ;; Extend it
      (goto-char end)
      (caml-remove-faces here)
      (setq code (caml-parse-production here stack))

      ;; Enlarge the overlay
      (cond (code
	     (setq end (cdr (cdr (car code))))
	     (move-overlay over (overlay-start over) end)
	     (overlay-put over 'stack code)
	     (car (car code)))
	    (t nil)))))

;;
;; Justify the current block.
;;
(defun caml-justify-block ()
  "Justify the block enclosing the point."
  (interactive)
  (save-excursion
    (save-restriction
      (caml-narrow-to-region)
      (let (over code)
	;; Find the previous production
	(setq over (caml-find-previous-production))

	;; Get start of the block
	(goto-char (overlay-start over))
	(caml-remove-faces (overlay-end over))
	(caml-repeal-current-production)

	;; Now justify it and record it
	(setq code (caml-parse-production nil nil t))
	(if code (caml-add-production code))))))

;;
;; For debugging, compute the stack at the cursor.
;;
(defvar *caml-stack* nil
  "This holds the stack at the current point during debugging.")

(defun caml-stack ()
  "Calculate the indentation of the current line."
  (let (over)
    ;; Split the current production
    (save-excursion
      (save-restriction
	(caml-narrow-to-region)

	;; Split the current production
	(caml-repeal-current-production)

	;; Find the previous production
	(setq over (caml-find-previous-production))

	;; Extend it to the current position
	(setq *caml-stack* (caml-extend-production over))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRAILER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'caml)

(run-hooks 'caml-load-hook)

