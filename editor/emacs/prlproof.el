;;
;; This is a major mode that turns caml-mode into
;; a mode for editing proofs.  The main issue here is
;; to narrow caml-mode to the rule box, and
;; protect the main goal and the subgoals.
;;
;; $Log$
;; Revision 1.1  1997/04/28 15:50:47  jyh
;; This is the initial checkin of Nuprl-Light.
;; I am porting the editor, so it is not included
;; in this checkin.
;;
;; Directories:
;;     refiner: logic engine
;;     filter: front end to the Ocaml compiler
;;     editor: Emacs proof editor
;;     util: utilities
;;     mk: Makefile templates
;;
;; Revision 1.2  1996/09/02 19:34:52  jyh
;; Semi working package management.
;;
;; Revision 1.1  1996/05/21 02:26:38  jyh
;; This is a semi-working version before Wisconsin vacation.
;;
;;

(require 'caml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Run after loading this file
(defvar prlproof-load-hook nil
  "Run after loading prlproof.
You can customize key bindings or load extensions with this.")

;; Run after changing the mode
(defvar prlproof-mode-hook nil
  "Run at the very end of prlproof-mode.")

;; This is the function to which output is sent
(defvar prlproof-output-function nil
  "Function + arg to call on PRLproof view command")

(defvar prlproof-view-function nil
  "Function + arg to call on PRLproof view command")

;;
;; This is a list of markers.  The first marker is
;; for the goal, and the remaining markers specify the
;; subgoals.
;;
(defvar prlproof-markers nil
  "The cursor is not allowed before this position")

;;
;; These are the delimiters for the text.
;;
(defvar prlproof-main-start "^-<main>-$"
  "Delimiter for the start of main")

(defvar prlproof-rule-start "^-<beginrule>-$"
  "Delimiter for the start of the rule box")

(defvar prlproof-rule-end "^-<endrule>-$"
  "Delimiter for the end of the rule box")

(defvar prlproof-subgoal-start "^-<subgoal>-$"
  "Delimiter for the beginning of a subgoal")

(defvar prlproof-rule-prompt "^BY "
  "Prompt for rule in rule box")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode and key bindings
;;
;; We use the caml tables for the rule box,
;; but we use a different keymap table for the goal
;; and the subgoals.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Special keymap for goal and subgoals.
;;
(defvar prlproof-mode-map nil "Local keymap for prlproof-mode buffers.")

(or prlproof-mode-map
    (let ((map (make-keymap)))
      (suppress-keymap map)

      ;; Navigation
      (define-key map "v"	'prlproof-view-goal)
      (define-key map "^"	'prlproof-quit-goal)
      (define-key map "q"	'prlproof-quit-goal)
      (define-key map "u"	'prlproof-quit-goal)
      (define-key map "n"	'prlproof-next-goal)
      (define-key map "\M-n"	'prlproof-next-goal)
      (define-key map "j"	'prlproof-next-goal)
      (define-key map "p"	'prlproof-prev-goal)
      (define-key map "\M-p"	'prlproof-prev-goal)
      (define-key map "k"	'prlproof-prev-goal)
      (define-key map "\C-x<"	'prlproof-goto-rule-box)
      (define-key map "\C-z"	'prlproof-submit-rule)

      (setq prlproof-mode-map map)))

;;
;; Extend the caml-mode map.
;;
(defvar prlproof-caml-mode-map nil
  "Keymap for editing the rule box in prlproof-mode")

(or prlproof-caml-mode-map
    (let ((map (cons 'keymap caml-mode-map)))
      ;; New commands
      (define-key map "\C-x<"	'prlproof-goto-rule-box)
      (define-key map "\M-n"	'prlproof-next-goal)
      (define-key map "\M-p"	'prlproof-prev-goal)
      (define-key map "\C-z"	'prlproof-submit-rule)

      ;; Save it
      (setq prlproof-caml-mode-map map)))

;;
;; PRLproof mode is suitable only for specially formatted data.
;;
(put 'prlproof-mode 'mode-class 'special)

;;
;; Start the mode.
;;
(defun prlproof-mode ()
  "\
Mode for \"editing\" NuPRL-Light proofs.
In prlproof, you are \"editing\" a proof step, and the buffer
is divided into three parts.  The goal is displayed in the first
part, then come the rule box, which is edited in CAML mode,
and then the subgoals are listed.

You can move using the usual cursor motion commands.\\<prlproof-mode-map>.
To navigate up the proof tree, type \\[prlproof-view-goal] over
the goal.  To move down the subtree, type \\[prlproof-view-goal] over
the subgoal you wish to examine.

Customization variables (rename this buffer and type \\[describe-variable] on each line
for more info):

   (Actually, no local variables right now -- jyh 1/17/96)

Hooks (use \\[describe-variable] to see their documentation):

  prlproof-mode-hook
  prlproof-load-hook

Keybindings:
\\{prlproof-mode-map}

See the documentation for CAML mode for more detail on editing
the rule box."
  (interactive)

  ;; Start CAML mode as the default mode for this buffer
  (let ((modified (buffer-modified-p))
	(caml-mode-hook caml-mode-hook))
    (add-hook 'caml-mode-hook 'prlproof-set-markers)
    (caml-mode)

    ;; Change the buffer's status
    (use-local-map prlproof-caml-mode-map)
    (setq major-mode 'prlproof-mode
	  mode-name "PRLproof"
	  case-fold-search nil
	  mode-line-buffer-identification '("PRLproof: %17b"))

    ;; Local values
    (make-local-variable 'prlproof-output-function)
    (setq prlproof-output-function prlproof-view-function)

    ;; Modification
    (if (not modified) (set-buffer-modified-p nil))

    ;; Fire up
    (run-hooks 'prlproof-mode-hook)

    ;; Move the point
    (prlproof-goto-rule-box)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Delimit all the text in the buffer.
;;
(defun prlproof-set-markers ()
  "Find the locations of the rule box"
  ;; Remove existing properties
  (let ((inhibit-read-only t))
    (set-text-properties (point-min) (point-max) nil))

  ;; Now look for the markers
  (let ((start (point-min))
	markers)

    ;; Get the goal
    (goto-char start)
    (re-search-forward prlproof-main-start)
    (forward-char 1)
    (add-text-properties (match-beginning 0) (point) '(invisible t))
    (setq markers (cons (point-marker) markers))

    ;; Get the rule box
    (re-search-forward prlproof-rule-start)
    (forward-char 1)
    (setq start (match-beginning 0))
    (add-text-properties start (point) '(invisible t))
    (re-search-forward prlproof-rule-prompt)
    (add-text-properties (match-beginning 0) (point) '(face bold))
    (add-text-properties start (point) '(intangible t))
    (setq start (point))
    (add-text-properties (point-min) start `(local-map ,prlproof-mode-map
					     rear-nonsticky t
					     read-only t))
    (re-search-forward prlproof-rule-end)
    (forward-char 1)
    (setq end (match-beginning 0))
    (add-text-properties end (point) '(invisible t intangible t))

    ;; Find the subgoals
    (while (re-search-forward prlproof-subgoal-start nil t)
      (let ((pos (match-beginning 0)))
	(forward-char 1)
	(add-text-properties pos (point) '(invisible t intangible t))
	(setq markers (cons (point-marker) markers))))

    ;; Now make the subgoals read-only
    (add-text-properties end (point-max) `(local-map ,prlproof-mode-map read-only t))

    ;; Specify caml-region
    (set-marker caml-point-min start)
    (set-marker caml-point-max end)
    (goto-char start)

    ;; Save markers
    (setq prlproof-markers (nreverse markers))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAVIGATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Move to the next position:
;;    1. If in the goal, move to the rule box
;;    2. If in the rule box, move to the first subgoal
;;    3. If in a subgoal, move to the next subgoal
;;
(defun prlproof-next-goal ()
  "Move to the next goal or rule box"
  (interactive)
  (let ((index (prlproof-locate-point)))
    (prlproof-move-point (1+ index))))

(defun prlproof-prev-goal ()
  "Move to the next goal or rule box"
  (interactive)
  (let ((index (prlproof-locate-point)))
    (prlproof-move-point (1- index))))

(defun prlproof-goto-rule-box ()
  "Move the point to the rule box"
  (interactive)
  (goto-char caml-point-min))

;;
;; Figure out what part of the screen the point is in.
;;
(defun prlproof-locate-point ()
  "Find out which area of the screen the point is in"
  (let ((markers (cdr prlproof-markers))
	(i 1)
	(here (point))
	flag)

    ;; Goal, rule box
    (cond ((< here caml-point-min) 0)
	  ((<= here caml-point-max) 1)
	  (t
	   ;; Search subgoals
	   (while (and markers (not flag))
	     (setq mark (car markers))
	     (if (< here mark)
		 (setq flag t)
	       (setq i (1+ i))
	       (setq markers (cdr markers))))

	   ;; Index
	   i))))

;;
;; Given an area of the screen, move the point.
;;
(defun prlproof-move-point (index)
  "Move the point to an area of the screen."
  (cond ((<= index 0) (goto-char (car prlproof-markers)))
	((= index 1) (goto-char caml-point-min))
	(t
	 ;; Search markers
	 (let ((mark (nth (1- index) prlproof-markers)))
	   (if mark
	       (goto-char mark)
	     (goto-char (point-max)))))))

;;
;; View the goal sourrounding the point.
;;
(defun prlproof-view-goal ()
  "View the goal under the point"
  (interactive)
  (let* ((loc (prlproof-locate-point))
	 (command (cond ((= loc 0) "shell__move_up ();;\n")
			((> loc 1) (concat "shell__move_down " (int-to-string (- loc 2)) ";;\n"))
			(t nil))))
    (if command
	(funcall (car prlproof-output-function)
		 (cdr prlproof-output-function)
		 command))))

;;
;; Go up.
;;
(defun prlproof-quit-goal ()
  "Move up to the previous goal"
  (interactive)
  (funcall (car prlproof-output-function)
	   (cdr prlproof-output-function)
	   "shell__move_up ();;\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RULE SUBMISSION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Submit the tactic to the refiner.
;;
(defun prlproof-submit-rule ()
  "Get the rule text and submit it."
  (interactive)
  (let ((tac (buffer-substring caml-point-min caml-point-max))
	(str (prlproof-formatted-rule-text)))
    (message str)
    (funcall (car prlproof-output-function)
	     (cdr prlproof-output-function)
	     (concat "shell__refine \"" tac "\" (" str ");;\n"))))

;;
;; Take the text in the rule box, and format it into a string.
;; We are careful here about expanding terms surrounded by
;;    (| term |) and (termbegin term termend)
;;
(defun prlproof-formatted-rule-text ()
  "Format the text in the rule box"
  (save-excursion
    (save-restriction
      (goto-char caml-point-min)
      (caml-narrow-to-region)

      ;; Now piece together the text
      (let ((pos (point))
	    text s start end)

	;; Collect all terms
	(while (re-search-forward "(|\\|\\btermbegin\\b" nil 0)
	  (setq start (match-beginning 0))

	  ;; Get the normal string
	  (setq s (buffer-substring pos start))
	  (set-text-properties 0 (1- (length s)) nil s)
	  (setq text (cons s text))

	  ;; Get the term
	  (re-search-forward "|)\\|\\btermend\\b" nil 0)
	  (setq end (point))
	  (setq text (cons (concat "(termParse__term_of_string \""
				   (prlproof-quote-buffer-substring start end)
				   "\")")
			   text))

	  ;; Start over
	  (setq pos end))

	;; Collect terminating string
	(setq text (cons (prlproof-quote-buffer-substring pos (point)) text))

	;; Form the string
	(apply 'concat (nreverse text))))))

;;
;; Take a buffer substring and quote it so that CAML will read it
;; correctly.  We have to escape the chars \ and "
;;
(defun prlproof-quote-buffer-substring (start end)
  "Quote the text in the region so that it is read correctly by CAML."
  (save-excursion
    (save-restriction
      (goto-char start)
      (narrow-to-region start end)

      ;; Now piece together the text
      (let ((pos (point))
	    text s start end)

	;; Collect all terms
	(while (and (skip-chars-forward "^\\\"") (not (eobp)))
	  ;; Quote the next char
	  (setq start (point))

	  ;; Get the normal string
	  (setq s (buffer-substring pos start))
	  (set-text-properties 0 (1- (length s)) nil s)
	  (setq text (cons s text))

	  ;; Get the special char
	  (forward-char 1)
	  (setq end (point))
	  (setq s (buffer-substring start end))
	  (set-text-properties 0 (1- (length s)) nil s)
	  (setq text (cons (concat "\\" s) text))

	  ;; Start over
	  (setq pos end))

	(goto-char (point-max))

	;; Collect terminating string
	(setq s (buffer-substring pos (point)))
	(set-text-properties 0 (1- (length s)) nil s)
	(setq text (cons s text))

	;; Form the string
	(apply 'concat (nreverse text))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRAILER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'prlproof)

(run-hooks 'prlproof-load-hook)
