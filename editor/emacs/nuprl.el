;;; nuprl.el --- specialized comint.el for running a NuPRL session.
;;
;; This is derived inpart by looking at the source to
;; shell.el, delivered with the standard GNU emacs package,
;; with the following copyright notice:
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Copyright (C) 1988, 1993, 1994, 1995 Free Software Foundation, Inc.
;; ;;
;; ;; Author: Olin Shivers <shivers@cs.cmu.edu>
;; ;; Maintainer: Simon Marshall <simon@gnu.ai.mit.edu>
;; ;; Keywords: processes
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright (C) 1996 Jason J. Hickey.
;;
;; Author: Jason Hickey <jyh@cs.cornell.edu>
;; Keywords: Nuprl, processes.

;; YOUR .EMACS FILE
;;=============================================================================
;; Some suggestions for your .emacs file.
;;
;; ;; Define M-# to run some strange command:
;; (eval-after-load "nuprl"
;;  '(define-key nuprl-mode-map "\M-#" 'nuprl-dynamic-spell))

;;; Brief Command Documentation:
;;;============================================================================
;;; Comint Mode Commands: (common to nuprl and all comint-derived modes)
;;;
;;; m-p	    comint-previous-input    	    Cycle backwards in input history
;;; m-n	    comint-next-input  	    	    Cycle forwards
;;; m-r     comint-previous-matching-input  Previous input matching a regexp
;;; m-s     comint-next-matching-input      Next input that matches
;;; m-c-l   comint-show-output		    Show last batch of process output
;;; return  comint-send-input
;;; c-d	    comint-delchar-or-maybe-eof	    Delete char unless at end of buff.
;;; c-c c-a comint-bol                      Beginning of line; skip prompt
;;; c-c c-u comint-kill-input	    	    ^u
;;; c-c c-w backward-kill-word    	    ^w
;;; c-c c-c comint-interrupt-subjob 	    ^c
;;; c-c c-z comint-stop-subjob	    	    ^z
;;; c-c c-\ comint-quit-subjob	    	    ^\
;;; c-c c-o comint-kill-output		    Delete last batch of process output
;;; c-c c-r comint-show-output		    Show last batch of process output
;;; c-c c-h comint-dynamic-list-input-ring  List input history
;;;         send-invisible                  Read line w/o echo & send to proc
;;;         comint-continue-subjob	    Useful if you accidentally suspend
;;;					        top-level job
;;; comint-mode-hook is the comint mode hook.

;;; Nuprl Mode Commands:
;;;         nuprl			Fires up the nuprl process
;;; tab     comint-dynamic-complete	Complete filename/command/history
;;; m-?     comint-dynamic-list-filename-completions
;;;					List completions in help buffer
;;;
;;; The Nuprl mode hook is nuprl-mode-hook
;;; comint-prompt-regexp is initialised to nuprl-prompt-pattern, for backwards
;;; compatibility.

;;; Read the rest of this file for more information.

;;; Customization and Buffer Variables
;;; ===========================================================================
;;; 

;;; Code:

(require 'comint)
(require 'prled)
(require 'prlitem)
(require 'prlproof)

(defvar nuprl-prompt-pattern "^# *"
  "Regexp to match prompts in the inferior nuprl.
Defaults to \"^# *\".")

(defvar nuprl-delimiter-argument-list '(?\| ?& ?< ?> ?\( ?\) ?\;)
  "List of characters to recognise as separate arguments.
This variable is used to initialize `comint-delimiter-argument-list' in the
nuprl buffer.  The default is (?\\| ?& ?< ?> ?\\( ?\\) ?\\;).")

(defvar nuprl-dynamic-complete-functions
  '(comint-replace-by-expanded-history)
  "List of functions called to perform completion.
This variable is used to initialise `comint-dynamic-complete-functions' in the
nuprl buffer.")

(defvar nuprl-input-autoexpand 'history
  "*If non-nil, expand input command history references on completion.
This mirrors the optional behavior of tcsh (its autoexpand and histlit).

If the value is `input', then the expansion is seen on input.
If the value is `history', then the expansion is only when inserting
into the buffer's input ring.  See also `comint-magic-space' and
`comint-dynamic-complete'.

This variable supplies a default for `comint-input-autoexpand',
for Nuprl mode only.")

(defvar explicit-nuprl-file-name nil
  "*If non-nil, is file name to use for explicitly requested inferior nuprl.")

(defvar nuprl-mode-map nil)
(cond ((not nuprl-mode-map)
       (setq nuprl-mode-map (nconc (make-sparse-keymap) comint-mode-map))
       (define-key nuprl-mode-map "\C-c\C-f" 'nuprl-forward-command)
       (define-key nuprl-mode-map "\C-c\C-b" 'nuprl-backward-command)
       (define-key nuprl-mode-map "\t" 'comint-dynamic-complete)
       (define-key nuprl-mode-map "\M-?"
	 'comint-dynamic-list-filename-completions)
       (define-key nuprl-mode-map [menu-bar completion]
	 (copy-keymap (lookup-key comint-mode-map [menu-bar completion])))
       (define-key-after (lookup-key nuprl-mode-map [menu-bar completion])
	 [complete-env-variable] '("Complete Env. Variable Name" .
				   nuprl-dynamic-complete-environment-variable)
	 'complete-file)
       (define-key-after (lookup-key nuprl-mode-map [menu-bar completion])
	 [expand-directory] '("Expand Directory Reference" .
			      nuprl-replace-by-expanded-directory)
	 'complete-expand)))

(defvar nuprl-mode-hook '()
  "*Hook for customising Nuprl mode.")

(defvar nuprl-font-lock-keywords
  (list (cons nuprl-prompt-pattern 'font-lock-keyword-face)
	'("[ \t]\\([+-][^ \t\n]+\\)" 1 font-lock-comment-face)
	'("^[^ \t\n]+:.*" . font-lock-string-face)
	'("^\\[[1-9][0-9]*\\]" . font-lock-string-face))
  "Additional expressions to highlight in Nuprl mode.")

;;
;; Record the output mode.
;;    if nuprl-output-mode is nil, then output is normal
;;    if not, then it contains the name of the display that is being captured
;;
;; nuprl-display-marker records the beginning of the captured display
;; nuprl-display-buffer is the buffer for capturing displays
(defvar nuprl-display-mode nil
  "If non-nil, this variable is the name of the display being captured")

(defvar nuprl-display-delimiter nil
  "This variable may hold the last display delimiter")

(defvar nuprl-display-marker nil
  "The marker is at the beginning of the display")

(defvar nuprl-display-buffer "-nuprl-display-"
  "This is the name of the buffer being used to capture a display")

(defvar nuprl-display-quote "--+--"
  "String that delimits NuPRL displays")

;; Other buffers used to display info
(defvar nuprl-modules-buffer "*nuprl-modules*"
  "This is the buffer that is used to list NuPRL modules")

(defvar nuprl-theory-buffer "*nuprl-theory*"
  "This buffer is used to display theories")

(defvar nuprl-item-buffer "*nuprl-item*"
  "This buffer is used to display an item in a theory")

(defvar nuprl-proof-buffer "*nuprl-proof*"
  "This buffer is used to display the current proof step.")

;; Marker that divides the rules from teh display forms
(defvar nuprl-theory-end-marker nil
  "This marker divides the rules from the display forms")

;; Properties of the file
(defvar nuprl-buffer-properties nil
  "This is the property list declared by the buffer")

;; Debug pointer
(defvar nuprl-debug-arrow1 nil
  "The is the overlay corresponding to the current debug pointer")
(defvar nuprl-debug-arrow2 nil
  "The is the overlay corresponding to the current debug pointer")

(defvar nuprl-debug-arrow-before "==>"
  "The is the content of the debug pointer")
(defvar nuprl-debug-arrow-after "<=="
  "The is the content of the debug pointer")

;; Flags


;;; Basic Procedures
;;; ===========================================================================
;;;

;;
;; Enter a mode to interact with the NuPRL session.
;;
(defun nuprl-mode ()
  "Major mode for interacting with an inferior Nuprl session.
\\[comint-send-input] after the end of the process' output sends the text from
    the end of process to the end of the current line.
\\[comint-send-input] before end of process output copies the current line minus the prompt to
    the end of the buffer and sends it (\\[comint-copy-old-input] just copies the current line).
\\[send-invisible] reads a line of text without echoing it, and sends it to
    the session.  This is useful for entering passwords.  Or, add the function
    `comint-watch-for-password-prompt' to `comint-output-filter-functions'.

If you want to make multiple Nuprl buffers, rename the `*nuprl*' buffer
using \\[rename-buffer] or \\[rename-uniquely] and start a new session.

If you want to make Nuprl buffers limited in length, add the function
`comint-truncate-buffer' to `comint-output-filter-functions'.

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

\\{nuprl-mode-map}
Customization: Entry to this mode runs the hooks on `comint-mode-hook' and
`nuprl-mode-hook' (in that order).  Before each input, the hooks on
`comint-input-filter-functions' are run.  After each Nuprl output, the hooks
on `comint-output-filter-functions' are run.

Variables `comint-completion-autolist', `comint-completion-addsuffix',
`comint-completion-recexact' and `comint-completion-fignore' control the
behavior of file name, command name and variable name completion.  Variable
`nuprl-completion-execonly' controls the behavior of command name completion.
Variable `nuprl-completion-fignore' is used to initialise the value of
`comint-completion-fignore'.

Variables `comint-input-ring-file-name' and `comint-input-autoexpand' control
the initialisation of the input ring history, and history expansion.

Variables `comint-output-filter-functions', a hook, and
`comint-scroll-to-bottom-on-input' and `comint-scroll-to-bottom-on-output'
control whether input and output cause the window to scroll to the end of the
buffer."
  (interactive)
  (comint-mode)
  (setq major-mode 'nuprl-mode)
  (setq mode-name "NuPRL")
  (use-local-map nuprl-mode-map)
  (setq comint-prompt-regexp nuprl-prompt-pattern)
  (setq comint-delimiter-argument-list nuprl-delimiter-argument-list)
  (setq comint-dynamic-complete-functions nuprl-dynamic-complete-functions)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start comint-prompt-regexp)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(nuprl-font-lock-keywords t))
  (setq comint-input-autoexpand nuprl-input-autoexpand)

  ;; Output processing
  (make-local-variable 'nuprl-display-mode)
  (make-local-variable 'nuprl-display-delimiter)
  (make-local-variable 'nuprl-display-marker)
  (make-local-variable 'nuprl-display-buffer)
  (setq nuprl-display-marker (point-min-marker))
  (setq nuprl-display-buffer (generate-new-buffer nuprl-display-buffer))
  (add-hook 'comint-output-filter-functions 'nuprl-output-filter)
  (add-hook 'kill-buffer-hook 'kill-nuprl-mode)

  ;; Other buffers
  (make-local-variable 'nuprl-modules-buffer)
  (make-local-variable 'nuprl-theory-buffer)
  (make-local-variable 'nuprl-item-buffer)
  (make-local-variable 'nuprl-proof-buffer)
  (make-local-variable 'nuprl-theory-end-marker)
  (setq nuprl-modules-buffer (generate-new-buffer nuprl-modules-buffer))
  (setq nuprl-theory-buffer (generate-new-buffer nuprl-theory-buffer))
  (setq nuprl-item-buffer (generate-new-buffer nuprl-item-buffer))
  (setq nuprl-proof-buffer (generate-new-buffer nuprl-proof-buffer))

  ;; Flags
  (make-local-variable 'nuprl-buffer-properties)

  ;; Arrows
;  (make-local-variable 'nuprl-debug-arrow1)
;  (make-local-variable 'nuprl-debug-arrow2)

  ;; Set the font
  (set-default-font "nl12")
  (standard-display-8bit 128 255)

  ;; Fire it up
  (run-hooks 'nuprl-mode-hook)
  (comint-read-input-ring t))

;;
;; Run an inferior NuPRL session.
;;
(defun nuprl ()
  "Run an inferior NuPRL session, with I/O through buffer *nuprl*.
If buffer exists but nuprl process is not running, make new nuprl.
If buffer exists and nuprl process is running, just switch to buffer `*nuprl*'.
Program used comes from variable `explicit-nuprl-file-name',
 or (if that is nil) from the NUPRL environment variable,
 or else it is the command \"nuprl\".
The buffer is put in NuPRL mode, giving commands for sending input
and controlling the subjobs of the nuprl.  See `nuprl-mode'.
See also the variable `nuprl-prompt-pattern'.

\(Type \\[describe-mode] in the NuPRL buffer for a list of commands.)"
  (interactive)
  (if (not (comint-check-proc "*nuprl*"))
      (let* ((prog (or explicit-nuprl-file-name
		       (getenv "NUPRL")
		       "nuprl"))		     
	     (name (file-name-nondirectory prog))
	     nuprl-buffer)
	(save-excursion
	  ;; May eventually replace nil with optional args
	  (set-buffer (apply 'make-comint "nuprl" prog nil))
	  (setq nuprl-buffer (current-buffer))
	  (nuprl-mode))
	(pop-to-buffer nuprl-buffer))
    (pop-to-buffer "*nuprl*")))

;;
;; When the buffer is killed, this destroys any extra stuff
;; lying around.
;;
(defun kill-nuprl-mode ()
  "Delete extra buffers created during NuPRL mode"
  (if (bufferp nuprl-display-buffer)
      (kill-buffer nuprl-display-buffer))
  (if (bufferp nuprl-modules-buffer)
      (kill-buffer nuprl-modules-buffer))
  (if (bufferp nuprl-theory-buffer)
      (kill-buffer nuprl-theory-buffer))
  (if (bufferp nuprl-item-buffer)
      (kill-buffer nuprl-item-buffer))
  (if (bufferp nuprl-proof-buffer)
      (kill-buffer nuprl-proof-buffer))
  (nuprl-hide-debug-mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nuprl-forward-command (&optional arg)
  "Move forward across ARG nuprl command(s).  Does not cross lines.
See `nuprl-command-regexp'."
  (interactive "p")
  (let ((limit (save-excursion (end-of-line nil) (point))))
    (if (re-search-forward (concat nuprl-command-regexp "\\([;&|][\t ]*\\)+")
			   limit 'move arg)
	(skip-syntax-backward " "))))


(defun nuprl-backward-command (&optional arg)
  "Move backward across ARG nuprl command(s).  Does not cross lines.
See `nuprl-command-regexp'."
  (interactive "p")
  (let ((limit (save-excursion (comint-bol nil) (point))))
    (if (> limit (point))
	(save-excursion (beginning-of-line) (setq limit (point))))
    (skip-syntax-backward " " limit)
    (if (re-search-backward
	 (format "[;&|]+[\t ]*\\(%s\\)" nuprl-command-regexp) limit 'move arg)
	(progn (goto-char (match-beginning 1))
	       (skip-chars-forward ";&|")))))

(defun nuprl-dynamic-complete-environment-variable ()
  "Dynamically complete the environment variable at point.
Completes if after a variable, i.e., if it starts with a \"$\".
See `nuprl-dynamic-complete-as-environment-variable'.

This function is similar to `comint-dynamic-complete-filename', except that it
searches `process-environment' for completion candidates.  Note that this may
not be the same as the interpreter's idea of variable names.  The main problem
with this type of completion is that `process-environment' is the environment
which Emacs started with.  Emacs does not track changes to the environment made
by the interpreter.  Perhaps it would be more accurate if this function was
called `nuprl-dynamic-complete-process-environment-variable'.

Returns non-nil if successful."
  (interactive)
  (let ((variable (nuprl-match-partial-variable)))
    (if (and variable (string-match "^\\$" variable))
	(prog2 (message "Completing variable name...")
	    (nuprl-dynamic-complete-as-environment-variable)))))


(defun nuprl-dynamic-complete-as-environment-variable ()
  "Dynamically complete at point as an environment variable.
Used by `nuprl-dynamic-complete-environment-variable'.
Uses `comint-dynamic-simple-complete'."
  (let* ((var (or (nuprl-match-partial-variable) ""))
	 (variable (substring var (or (string-match "[^$({]\\|$" var) 0)))
	 (variables (mapcar (function (lambda (x)
					(substring x 0 (string-match "=" x))))
			    process-environment))
	 (addsuffix comint-completion-addsuffix)
	 (comint-completion-addsuffix nil)
	 (success (comint-dynamic-simple-complete variable variables)))
    (if (memq success '(sole shortest))
	(let* ((var (nuprl-match-partial-variable))
	       (variable (substring var (string-match "[^$({]" var)))
	       (protection (cond ((string-match "{" var) "}")
				 ((string-match "(" var) ")")
				 (t "")))
	       (suffix (cond ((null addsuffix) "")
			     ((file-directory-p
			       (comint-directory (getenv variable))) "/")
			     (t " "))))
	  (insert protection suffix)))
    success))

;;
;; Handle an output string.
;; This function multiplexes output to the different displays,
;; including:
;;    1. The module display list
;;
;; Output that is bound for a display is delimited by the strings:
;;    --+--display--+--		(start/stop output to the display)
;;
;; When the start sequent is detected, we place a marker in the buffer
;; and continuously delete input until the stop sequence is detected.
;; In the meantime, all input is forwared to the display processor.
;;
;; All the display processors are known a-priori, and the processor for
;; the display is named "nuprl-<display>-output-processor"
;;
;; Note:
;;    The current buffer is the expected buffer.
;;
;; If the insertion point jump before the old marker,
;; we assume that it did not jump during a delimiter, and
;; we reset the saved marker to the new position.
;;
;; We also catch CAML debugging info in the form:
;; \032\032FILENAME:CHRACTER\n
;; and display the file in the other buffer.
;;
(defun nuprl-output-filter (string)
  "Handle output to the NuPRL buffer, and watch for special
output meant to produce displays"
  ;; Account for jumping insertion point
  (if (< comint-last-output-start nuprl-display-marker)
      (setq nuprl-display-marker (copy-marker comint-last-output-start)))

  ;; Process new input
  (save-excursion
   (nuprl-collect-display nuprl-display-marker
			  (+ comint-last-output-start (length string)))))

;;
;; Collect input surrounded by delimiters.
;; The delimiters should be markers.
;;
(defun nuprl-collect-display (start end)
  "Process the text delimited by START and END,
and collect it into a display if necessary"
  (let (lend nstart line)
    (while (progn
	     ;; Delimit the current line
	     (goto-char start)
	     (setq lend (progn (end-of-line) (point)))
	     (setq nstart (progn (forward-line) (point)))
	     (and (<= nstart end) (< lend nstart)))

      ;; Get the next line and process it
      (setq line (buffer-substring start lend))
      (cond (nuprl-display-mode
	     ;; If the trailer is found, copy the region to the
	     ;; display buffer and process it.  Otherwise, copy
	     ;; the line to the display and continue
	     (cond ((equal line nuprl-display-delimiter)
		    (delete-region start nstart)
		    (nuprl-process-display-buffer))

		   (t
		    (nuprl-move-region-to-display start nstart))))

	    ((nuprl-is-display-header line)
	     ;; This is a header line; just switch to display mode
	     (setq nuprl-display-delimiter line)
	     (setq nuprl-display-mode (nuprl-get-display-name line))
	     (delete-region start nstart))

	    ((nuprl-is-scroll-command line)
	     ;; Scroll the window
	     (delete-region start nstart))

	    ((nuprl-is-debug-command line)
	     ;; Display the file in the other window
	     (delete-region start nstart))

	    (t
	     ;; This is a normal line: just skip it
	     (set-marker start nstart))))))

;;
;; See if the current line is a header
;;
(defun nuprl-is-display-header (line)
  "Determine if a line is a display delimiter"
  (let ((l (length line))
	(dl (length nuprl-display-quote)))
    (and (> l (+ dl dl))
	 (let ((head (substring line 0 dl))
	       (tail (substring line (- l dl) l)))
	   (and (equal head nuprl-display-quote)
		(equal tail nuprl-display-quote))))))

;;
;; Extract the display name from the delimiter
;;
(defun nuprl-get-display-name (line)
  "Get the name of the display from the display delimiter"
  (let ((l (length line))
	(dl (length nuprl-display-quote)))
    (substring line dl (- l dl))))

;;
;; Move the region of the current buffer to the display buffer.
;;
(defun nuprl-move-region-to-display (start end)
  "Mave a region delimited by START and END to the display buffer"
  (let ((cbuf (current-buffer)))
    (save-excursion
      (set-buffer nuprl-display-buffer)
      (insert-buffer-substring cbuf start end))
    (delete-region start end)))

;;
;; Process the display buffer.
;;
(defun nuprl-process-display-buffer ()
  "Process the contents of the display buffer"
  (cond ((equal nuprl-display-mode "modules")
	 (nuprl-process-modules-display))
	((equal nuprl-display-mode "theory")
	 (nuprl-process-theory-display))
	((equal nuprl-display-mode "item")
	 (nuprl-process-item-display))
	((equal nuprl-display-mode "proof")
	 (nuprl-process-proof-display)))
  (save-excursion
    (set-buffer nuprl-display-buffer)
    (erase-buffer))
  (setq nuprl-display-mode nil))

;;
;; After a buffer is processed, we want to display it when:
;;    1. It is not already in a window
;;    2. The interactive buffer is in the selected window
;;
(defun nuprl-show-buffer (buf)
  "Display a NuPRL buffer if desirable"
  (let* ((nbuf (current-buffer))
	 (nwin (get-buffer-window nbuf))
	 (swin (selected-window))
	 newwin height)
    (when (and nwin (not (get-buffer-window buf)))
	  ;; Delete other windows, and install the new one
	  (delete-other-windows nwin)
	  (setq height (- (window-height nwin) 10))
	  (when (> height 10)
		(setq newwin (split-window-vertically height))
		(set-window-buffer nwin buf)
		(select-window (if (equal swin nwin) newwin nwin))
		(set-buffer nbuf)))))

;;
;; Send a line to the process.
;;
(defun nuprl-simple-send (buf str)
  "Send a command to the process"
  (let ((proc (get-buffer-process buf)))
    (save-excursion
      (set-buffer buf)
;      (insert str)
      (comint-simple-send proc str))))

;;
;; Handle a view command in the modules buffer.
;;
(defun nuprl-view-module (buf code str)
  "Handle a view command for the *nuprl-modules* buffer.
This means to view the contents of the module.  The first is
the NuPRL buffer, and the second is the module to view."
  (when (eq code 'view)
	(let ((command (concat "shell__cd \"/" str "\";;")))
	  (nuprl-simple-send buf command))))
  
;;
;; Process the modules display.  the step are:
;;    1. Copy the input to a new buffer
;;    2. Place the new buffer in PRLed mode.
;;
(defun nuprl-process-modules-display ()
  "Process the modules display by creating a new buffer in PRLed mode"
  ;; Modify the buffer
  (let ((obuf (current-buffer)))
    (save-excursion
      (set-buffer nuprl-modules-buffer)
      (let (buffer-read-only)
	(erase-buffer)
	(insert-buffer nuprl-display-buffer)

	;; Erase trailing whitespace
	(goto-char (point-max))
	(skip-syntax-backward " ")
	(delete-region (point) (point-max))

	;; Go back to beginning
	(goto-char (point-min))
	(set-buffer-modified-p nil))

      ;; Use prled-mode
      (let ((prled-view-function (cons 'nuprl-view-module obuf)))
	(prled-mode))

      ;; Get info about the buffer from mode line
      (nuprl-process-property-info)))

  ;; Display it if necessary
  (nuprl-show-buffer nuprl-modules-buffer))

;;
;; Handle a view command in a theory window.
;;
(defun nuprl-view-module-item (buf code str)
  "Handle a view command for the *nuprl-theory* buffer.
This means to view the contents of the module.  The first is
the NuPRL buffer, and the second is the line of the view command."
  (cond ((eq code 'view)
	 (nuprl-view-module-item-aux buf))
	((eq code 'quit)
	 (nuprl-quit-item buf 'quit))))

(defun nuprl-view-module-item-aux (buf)
  "View the module item at the cursor"
  (let (start code wstart wend word command path)
    (setq path (cdr (assq 'path nuprl-buffer-properties)))
    (save-excursion
      (beginning-of-line)
      (skip-syntax-forward " ")
      (setq start (point))
      (when (and (>= start prled-min-marker)
		 (not (looking-at "-")))

	    (cond ((< start nuprl-theory-end-marker)
		   ;; This is an item of the theory
		   ;; We distinguish between parents, labels, and other items
		   (setq code (buffer-substring (+ start 2) (+ start 3)))
		   (goto-char (+ start 4))
		   (setq wstart (point))
		   (skip-syntax-forward "w")
		   (setq wend (point))
		   (setq word (buffer-substring wstart wend))
		   (cond ((equal code "P")
			  (setq command (concat "shell__view \"/" word "\";;")))
			 ((equal code "L")
			  (setq command nil))
			 (t
			  (setq command (concat "shell__view \"" word "\";;")))))
		  (t
		   ;; This is a display form
		   (goto-char (+ start 2))
		   (setq wstart (point))
		   (skip-syntax-forward "w")
		   (setq wend (point))
		   (setq word (buffer-substring wstart wend))
		   (setq command (concat "shell__view \"" path "/" word "\";;"))))

	    ;; Now we have the command; send it
	    (when command (nuprl-simple-send buf command))))))
  
;;
;; Process the modules display.  the step are:
;;are:
;;    1. Copy the input to a new buffer
;;    2. Place the new buffer in PRLed mode.
;;
(defun nuprl-process-theory-display ()
  "Process the modules display by creating a new buffer in PRLed mode"
  ;; Modify the buffer
  (let ((obuf (current-buffer)))
    (save-excursion
      (set-buffer nuprl-theory-buffer)

      ;; Insert the temp buffer
      (let (buffer-read-only)
	(erase-buffer)
	(insert-buffer nuprl-display-buffer)

	;; Erase trailing whitespace
	(goto-char (point-max))
	(skip-syntax-backward " ")
	(delete-region (point) (point-max)))

      ;; Use prled-mode
      (let ((prled-view-function (cons 'nuprl-view-module-item obuf)))
	(prled-mode))

      ;; Navigation info
      (nuprl-process-property-info)

      ;; Set the marker for end of theory items
      (save-excursion
	;; Find the marker before display forms
	(goto-char prled-min-marker)
	(setq nuprl-theory-end-marker (copy-marker prled-min-marker))
	(set-marker nuprl-theory-end-marker (re-search-forward "^--" nil nil)))))

  ;; Display it if necessary
  (nuprl-show-buffer nuprl-theory-buffer))

;;
;; Quit the item display.
;;
(defun nuprl-quit-item (buf command)
  "Quit the item display; display the last window instead"
  (when (eq command 'quit)
	(let* ((dir (cdr (assq 'path nuprl-buffer-properties)))
	       (command (concat "cd \"" dir "/..\";;")))
	  (nuprl-simple-send buf command))))

;;
;; Process the item display.  The steps are:
;;    1. Copy the input to a new buffer
;;    2. Place the new buffer in Fundamental mode.
;;
(defun nuprl-process-item-display ()
  "Process the modules display by creating a new buffer in PRLed mode"
  ;; Modify the buffer
  (let ((obuf (current-buffer)))
    (save-excursion
      (set-buffer nuprl-item-buffer)

      ;; Insert the temp buffer
      (let (buffer-read-only)
	(erase-buffer)
	(insert-buffer nuprl-display-buffer)

	;; Erase trailing whitespace
	(goto-char (point-max))
	(skip-syntax-backward " ")
	(delete-region (point) (point-max)))

      ;; Use fundamental mode
      (let ((prlitem-view-function (cons 'nuprl-quit-item obuf)))
	(prlitem-mode))

      ;; Navigation info
      (nuprl-process-property-info)

      ;; Read only
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)))

  ;; Display it if necessary
  (nuprl-show-buffer nuprl-item-buffer))

;;
;; Get the property info.
;; This info should be on the first line, in the form
;; Key: value; Key: value; ...
;; Skip initial and final garbage.
;;
(defun nuprl-process-property-info ()
  "Get the property info for a buffer"
  (save-excursion
    (let ((inhibit-point-motion-hooks t)
	  bol eol wstart wend over props key value)
      (goto-char (point-min))
      (setq bol (point))
      (end-of-line)
      (setq eol (point))
      (goto-char bol)
      (while (progn
	       ;; Move to next key
	       (skip-chars-forward "^:")
	       (when (and (< (point) eol) (looking-at ":"))
		     ;; Get the key
		     (setq wend (point))
		     (skip-syntax-backward "w")
		     (setq key (buffer-substring (point) wend))

		     ;; Collect the value, and highlight it
		     (goto-char (1+ wend))
		     (skip-chars-forward " \t")
		     (setq wstart (point))
		     (skip-chars-forward "^ \t\n")
		     (setq wend (point))
		     (setq value (buffer-substring wstart wend))
		     (set-text-properties 0 (length key) nil key)
		     (set-text-properties 0 (length value) nil value)
		     (setq props (cons (cons (intern key) value) props))
		     (setq over (make-overlay wstart wend))
		     (overlay-put over 'evaporate t)
		     (overlay-put over 'face 'highlight)
		     t)))
      (setq nuprl-buffer-properties (reverse props)))))

;;
;; Process a proof step.
;;    1. Copy the input to a new buffer
;;    2. Place the new buffer in PRLproof mode.
;;
(defun nuprl-process-proof-display ()
  "Process the proof display by creating a new buffer in PRLproof mode"
  ;; Modify the buffer
  (let ((obuf (current-buffer)))
    (save-excursion
      (set-buffer nuprl-proof-buffer)

      ;; Insert the temp buffer
      (let (buffer-read-only
	    (inhibit-read-only t))
	(erase-buffer)
	(insert-buffer nuprl-display-buffer))

;	;; Erase trailing whitespace
;	(goto-char (point-max))
;	(skip-syntax-backward " ")
;	(delete-region (point) (point-max)))

      ;; Navigation info
      (nuprl-process-property-info)

      ;; Use prled-mode
      (let ((prlproof-view-function (cons 'nuprl-simple-send obuf)))
	(prlproof-mode))))

  ;; Display it if necessary
  (nuprl-show-buffer nuprl-proof-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCROLL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Scroll the window so that this line is at the top
;;
(defun nuprl-is-scroll-command (line)
  "Scroll the window to the top if the line matches."
  (let ((cols (length line)))
    (cond ((and (= cols 1) (= (aref line 0) ?\014))
	   (message "Line feed"))

	  ;; Not a degger command
	  (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEBUG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Display files when given the debugging command \032\032(H|M)FILENAME:CHARACTER\n
;;
(defun nuprl-is-debug-command (line)
  "Display the file specified by the debugging command,
if this line specifies one"
  (let ((cols (length line)))
    (cond ((and (> cols 2)
		(= (aref line 0) ?\032)
		(= (aref line 1) ?\032))

	   ;; Command
	   (let ((cmd (aref line 2)))
	     (cond ((eq cmd ?M)
		    ;; Display the mark
		    (let* ((first-colon (string-match ":" line 3))
			   (second-colon (string-match ":" line (1+ first-colon)))
			   (filename (substring line 3 first-colon))
			   (col (string-to-int (substring line (1+ first-colon) second-colon)))
			   (before (equal "before" (substring line (1+ second-colon) cols))))

		      ;; Display
		      (nuprl-display-debug-mark filename col before)))

		   ((eq cmd ?H)
		    ;; Hide the mark
		    (nuprl-hide-debug-mark))))
	   t)

	  ;; Not a degger command
	  (t nil))))


;;
;; Display the debug marker.
;;
(defun nuprl-display-debug-mark (filename col before-p)
  "Bring up the debug marker"
  (let ((buffer (find-file-noselect filename))
	pos)

    ;; Move around in the buffer
    (save-excursion
      (set-buffer buffer)

      ;; Get absolute position of the cursor
      (save-restriction
	(widen)
	(setq pos (+ (point-min) col)))

      ;; If outside the restriction, remove the restriction
      (if (or (< pos (point-min)) (> pos (point-max)))
	  (widen)))

    ;; Display the buffer
    (nuprl-show-buffer buffer)
    (set-window-point (get-buffer-window buffer) pos)

    ;; Make a new debug arrow
    (unless nuprl-debug-arrow1
	    (setq nuprl-debug-arrow1 (make-overlay pos pos buffer))
	    (overlay-put nuprl-debug-arrow1 'priority 0)
	    (setq nuprl-debug-arrow2 (make-overlay pos pos buffer))
	    (overlay-put nuprl-debug-arrow2 'priority 1)
	    (overlay-put nuprl-debug-arrow2 'face 'highlight))

    ;; Adjust the overlays
    (save-excursion
      (set-buffer buffer)
      (goto-char pos)

      ;; Arrow
      (cond (before-p
	     (skip-syntax-forward " ")
	     (setq pos (point))
	     (overlay-put nuprl-debug-arrow1 'after-string nuprl-debug-arrow-before))

	    (t
	     (skip-syntax-backward " ")
	     (setq pos (point))
	     (overlay-put nuprl-debug-arrow1 'after-string nuprl-debug-arrow-after)))
      (move-overlay nuprl-debug-arrow1 pos pos buffer)

      ;; Face
      (move-overlay nuprl-debug-arrow2
		    (progn (beginning-of-line) (point))
		    pos
		    buffer))))

;;
;; Remove the debug arrow.
;;
(defun nuprl-hide-debug-mark ()
  "Remove the debug arrow"
  (when nuprl-debug-arrow1
	(delete-overlay nuprl-debug-arrow1)
	(setq nuprl-debug-arrow1 nil))
  (when nuprl-debug-arrow2
	(delete-overlay nuprl-debug-arrow2)
	(setq nuprl-debug-arrow2 nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRAILER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'nuprl)

(run-hooks 'nuprl-load-hooks)
