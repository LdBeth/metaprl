;; ============================================================================
;; MetaPRL hack stuff

(setq tuareg-default-indent 3)

;; A MetaPRL friendly shell.
;; M-x metaprl will start a shell that provide a nice interface to MetaPRL.
;; Short summary:
;; 1. It is a simple shell so you can do whatever you want in it.
;; 2. Guess that MetaPRL is running if input ends with a "\n# ".
;; 3. If "MP" is entered on the shell, automatically go to the editor/ml dir,
;;    run mp and enter an automatic #use command (needed for pwd()) .
;; 4. Prints a MetaPRL prompt instead of the wonderful "# ".  Shows the current
;;    pwd() there - using an automatic printf, catching its output.
;; 5. Input goes thru an alias replacing mechanism.
;; 6. Input is one line only, ";;" is automatically appended when missing.  It
;;    is still possible to paste multiple-line stuff in, you can also enter C-q
;;    C-j to begin a new line (the ";;" will not be appended in this case).
;; 7. Hide unnecessary output: "unit = ()".
;; 8. A string of the form ".../" is translated to "cd (\"...\")".

(defvar metaprl-aliases
  '(("cd|1"   . "cd\"$1\"")
    ("cd|0"   . "pwd()")
    ("..|0"   . "cd \"..\"")
    ("~|0"    . "cd \"~\"")
    ("pwd|0"  . "pwd()")
    ("ls|0"   . "ls()")
    ("ls|1"   . "view \"$1\"")
    ("by"     . "refine $*")
    ("BY"     . "refine $*")
    ("up|0"   . "up 1")
    ("root|0" . "root()")
    ("load|1" . "load \"$1\"")
    ("save|0" . "save()"))
  "*An alias list to use inside the MetaPRL shell.
This is a list of conses, each holding the alias and its expansion.  The alias
is a string with an optional |n attached - each input line entered when
MetaPRL is running is transformed according to the first string, first, str|n
is used where n is the number of arguments provided, if this is not found, then
a search for the string is searched.  So if you have aliases for x|0 x|1 and x,
the first two will be used for zero or one arguments, and the third for the
rest of the cases.

Alias text can use $i for the ith argument provided and $* for using all
arguments.  Use $$ for a single $ character.

Aliases are not recursive.")

(defvar metaprl-dir-command "^\\(cd\\|up\\|down\\|root\\)\\>"
  "*A regexp for commands that require a prompt-directory synchronization.")

(defvar metaprl-prompt "MetaPRL %s > "
  "*A \"metaprl\" prompt to display when working with the MetaPRL shell.
The string should contain %s which will be replaced by the current-path.")

(defvar metaprl-start-alias "MP"
  "*The shell `alias' that will be used to start MetaPRL.")

(defvar metaprl-start-shell-command "cd ~/meta-prl/editor/ml ; ./mp\n"
  "*The shell command used to start MetaPRL.
Change the cd command to wherever you have mp.")

(defvar metaprl-start-ml-command "#use \"eli\""
  "*The ml command used to initialize MetaPRL.")

(defvar metaprl-quiet-result "- : unit = ()\n"
  "*Regexp for output lines that will not be shown on the output.
Default is a () result.")

(defvar metaprl-dir-sync "printf \"CURDIR=:=%s\\n\" (pwd())"
  "*A command to use for directory syncing.
This print a string that is captured by this mode for setting the prompt, so if
it is changed, make sure you change the occurrence of CURDIR below.")

(defvar metaprl-should-init-ml nil "Internal.  Do not change.")
(defvar metaprl-cur-prompt nil "Internal.  Do not change.")

(defun replace-dollar-substrings (str args)
  (let ((len (1- (length str))) (i -1))
    (while (< (setq i (1+ i)) len)
      (if (eq (aref str i) ?$)
        (let* ((ch (aref str (1+ i)))
               (s (cond ((and (<= ?0 ch) (<= ch ?9))
                         (let ((n (- ch ?0)))
                           (if (< n (length args)) (nth n args) "")))
                        ((eq ch ?*) (mapconcat 'identity (cdr args) " "))
                        ((eq ch ?$) "$")
                        (t ""))))
          (setq str (concat (substring str 0 i) s (substring str (+ i 2))))
          (setq i (+ i (length s) -1))
          (setq len (+ len (length s) -2)))))
    str))

;; Quick patch for this to read comint-preoutput-filter-functions after setting
;; the buffer to use the local value.  This skips. however, the check that
;; string is not null.
;; The purpose of using this filter for comint processes
;; is to keep comint-last-input-end from moving forward
;; when output is inserted.
(defun comint-output-filter (process string)
  ;; First check for killed buffer
  (let ((oprocbuf (process-buffer process)))
    (if (and string oprocbuf (buffer-name oprocbuf))
	(let ((obuf (current-buffer))
	      (opoint nil) (obeg nil) (oend nil))
	  (set-buffer oprocbuf)
	  (setq opoint (point))
	  (setq obeg (point-min))
	  (setq oend (point-max))
          (let ((functions comint-preoutput-filter-functions))
            (while (and functions string)
              (setq string (funcall (car functions) string))
              (setq functions (cdr functions))))
	  (let ((buffer-read-only nil)
		(nchars (length string))
		(ostart nil))
	    (widen)
	    (goto-char (process-mark process))
	    (setq ostart (point))
	    (if (<= (point) opoint)
		(setq opoint (+ opoint nchars)))
	    ;; Insert after old_begv, but before old_zv.
	    (if (< (point) obeg)
		(setq obeg (+ obeg nchars)))
	    (if (<= (point) oend)
		(setq oend (+ oend nchars)))
	    (insert-before-markers string)
	    ;; Don't insert initial prompt outside the top of the window.
	    (if (= (window-start (selected-window)) (point))
		(set-window-start (selected-window) (- (point) (length string))))
	    (if (and comint-last-input-end
		     (marker-buffer comint-last-input-end)
		     (= (point) comint-last-input-end))
		(set-marker comint-last-input-end (- comint-last-input-end nchars)))
	    (set-marker comint-last-output-start ostart)
	    (set-marker (process-mark process) (point))
	    (force-mode-line-update))
	  (narrow-to-region obeg oend)
	  (goto-char opoint)
	  (run-hook-with-args 'comint-output-filter-functions string)
	  (set-buffer obuf)))))

(defun metaprl-preoutput-filter (str)
  ;; Look for directory sync commands, set prompt accordingly.
  (if (string-match "CURDIR=:=\\(.*\\)\n" str)
    (progn (setq metaprl-cur-prompt
                 (format metaprl-prompt (match-string 1 str)))
           (setq str (concat (substring str 0 (match-beginning 0))
                             (substring str (match-end 0))))))
  ;; Delete metaprl-quiet-result strings.
  (while (string-match metaprl-quiet-result str)
    (setq str (concat (substring str 0 (match-beginning 0))
                      (substring str (match-end 0)))))
  ;; Print the MetaPRL prompt - only when we have the ocaml prompt as the
  ;; last string displayed.
  (if (and metaprl-cur-prompt (string-match "^# \\'" str))
    (concat (substring str 0 (match-beginning 0)) metaprl-cur-prompt)
    str))

(defun metaprl-output-filter (str)
  ;; Use a ^L character to put that place on the top line.
  (let ((len (length str)) (i -1))
    (while (< (setq i (1+ i)) len)
      (if (= (aref str i) ?\C-l)
        (save-excursion
          (let ((start (+ comint-last-output-start i)))
            (delete-region start (+ start 1))
            (goto-char start)
            (recenter 0))))))
  ;; If MetaPRL was just started, initialize it by sending the ml string.
  (if metaprl-should-init-ml
    (progn
      (process-send-string
       "*meta-prl*"
       (format "%s;;\nprintf \"\\n\\n\" ; %s;;\n"
               metaprl-start-ml-command metaprl-dir-sync))
      (setq metaprl-should-init-ml nil))))

(defun metaprl-input-filter (str)
  ;; This uses an ugly hack: It changes the value of "input" that holds text to
  ;; send to the shell.  This is because the input-filter functions' output is
  ;; not taken as the new input.
  (let (;; Get the contents of the current line.
        (line (save-excursion
                (set-buffer "*meta-prl*")
                (forward-line -1)
                (buffer-substring (progn (beginning-of-line) (point))
                                  (progn (end-of-line) (point))))))
    (cond
      ;; If we have a metaprl prompt, go ahead with the aliases etc.
      ((string-match (regexp-quote metaprl-cur-prompt) line)
       (let* ((args (split-string input))
              (alias
               (cdr
                (or
                 (assoc (format "%s|%s" (car args) (length (cdr args)))
                        metaprl-aliases)
                 (assoc (car args) metaprl-aliases)))))
         ;; Do the argument substitution.
         (and alias (setq input (replace-dollar-substrings alias args)))
         ;; Shorthand cd commands.
         (and (not alias)
              args
              (null (cdr args))
              (eq (aref (car args) (1- (length input))) ?/)
              (setq input (concat "cd(\"" (car args) "\");ls()")))
         ;; Append a prompt-directory synchronization command.
         (if (string-match metaprl-dir-command input)
           (setq input (concat input " ; " metaprl-dir-sync)))
         ;; Convert empty input to a "()".
         (if (string-match "^[ \t]*$" input)
           (setq input "();;"))
         ;; Append a ";;" if needed.
         (if (not (string-match ";;[ \t]*$" input))
           (setq input (concat input ";;")))))
      ;; No metaprl prompt - still translate the MP alias.
      ((equal input metaprl-start-alias)
       (setq input metaprl-start-shell-command)
       (setq metaprl-should-init-ml t)
       ;; Initialize an empty prompt.
       (setq metaprl-cur-prompt (format metaprl-prompt "?"))))))

;; Set a shell for MetaPRL.
(defun meta-prl ()
  "Start a shell with a nice interface to MetaPRL.
It is a simple shell so you can do whatever you want in it.  If \"MP\" is
entered, then the shell will automatically go to the meta-prl/editor/ml
directory, run mp and enter an automatic #use command.  Once in MetaPRL, a
special prompt will be displayed, and alias convertion will be performed."
  (interactive)
  (require 'comint)
  (if (comint-check-proc "*meta-prl*")
    ;; Have a MetaPRL shell running, use it.
    (pop-to-buffer "*meta-prl*")
    (let ((shell-mode-hook
           ;; Rename the shell-buffer as soon as it is created.  This method
           ;; will cause problems when there is no *meta-prl* buffer, but there
           ;; is a *shell*  already - that *shell* will be used and renamed as
           ;; the *meta-prl* buffer.
           '(lambda (&rest args) (rename-buffer "*meta-prl*"))))
      ;; Start the shell.
      (shell)
      (set-buffer (get-buffer "*meta-prl*"))
      ;; Display all chars correctly.
      (standard-display-8bit 128 255)
      ;; Set the MetaPRL font.
      (set-default-font (if (eq window-system 'nt)
                          "-*-mp12-r-normal-*-12-96-*-*-c-*-*-ansi-*"
                          "mp12"))
      ;; Set the filter functions locally.
      (make-local-variable 'comint-output-filter-functions)
      (setq comint-output-filter-functions 'metaprl-output-filter)
      (make-local-variable 'comint-preoutput-filter-functions)
      (setq comint-preoutput-filter-functions '(metaprl-preoutput-filter))
      (make-local-variable 'comint-input-filter-functions)
      (setq comint-input-filter-functions 'metaprl-input-filter)
      (setq metaprl-cur-prompt (format metaprl-prompt "?"))
      ;; Just for continence
      (setq default-directory "~/meta-prl/editor/ml/"))))
