;; ============================================================================
;; Nuprl-light stuff

(setq tuareg-default-indent 3)

;; A Nuprl-Light friendly shell.
;; M-x nuprl will start a shell that provide a nice interface to nuprl-light.
;; Short summary:
;; 1. It is a simple shell so you can do whatever you want in it.
;; 2. Guess that nuprl is running if input ends with a "\n# ".
;; 3. If "NL" is entered on the shell, automatically go to the editor/ml dir,
;;    run nl an enter an automatic #use command (needed for pwd()) .
;; 4. Prints a nuprl prompt instead of the wonderful "# ".  Shows the current
;;    pwd() there - using an automatic printf, catching its output.
;; 5. Input goes thru an alias replacing mechanism.
;; 6. Input is one line only, ";;" is automatically appended when missing.  It
;;    is still possible to paste multiple-line stuff in, you can also enter C-q
;;    C-j to begin a new line (the ";;" will not be appended in this case).
;; 7. Hide unnecessary output: "unit = ()".
;; 8. A string of the form ".../" is translated to "cd (\"...\")".

(defvar nuprl-aliases
  '(("cd|1"   . "cd\"$1\"")
    ("cd|0"   . "pwd()")
    ("..|0"   . "cd \"..\"")
    ("~|0"    . "cd \"~\"")
    ("pwd|0"  . "pwd()")
    ("ls|0"   . "ls()")
    ("ls|1"   . "view \"$1\"")
    ("by"     . "refine $*")
    ("up|0"   . "up 1")
    ("root|0" . "root()")
    ("load|1" . "load \"$1\""))
  "*An alias list to use inside the nuprl-light shell.
This is a list of conses, each holding the alias and its expansion.  The alias
is a string with an optional |n attached - each input line entered when
nuprl-light is running is transformed according to the first string, first,
str|n is used where n is the number of arguments provided, if this is not
found, then a search for the string is searched.  So if you have aliases for
x|0 x|1 and x, the first two will be used for zero or one arguments, and the
third for the rest of the cases.

Alias text can use $i for the ith argument provided and $* for using all
arguments.  Use $$ for a single $ character.

Aliases are not recursive.")

(defvar nuprl-dir-command "^\\(cd\\|up\\|down\\|root\\)\\>"
  "*A regexp for commands that require a prompt-directory synchronization.")

(defvar nuprl-prompt "NUPRL %s > "
  "*A \"nuprl\" prompt to display when working with the nuprl-light shell.
The string should contain %s which will be replaced by the current-path.")

(defvar nuprl-start-alias "NL"
  "*The shell `alias' that will be used to start nuprl-light.")

(defvar nuprl-start-shell-command "cd ~/nuprl-light/editor/ml ; ./nl\n"
  "*The shell command used to start nuprl-light.
Change the cd command to wherever you have nl.")

(defvar nuprl-start-ml-command "#use \"eli\""
  "*The ml command used to initialize nuprl-light.")

(defvar nuprl-quiet-result "- : unit = ()\n"
  "*Regexp for output lines that will not be shown on the output.
Default is a () result.")

(defvar nuprl-dir-sync "printf \"CURDIR=:=%s\\n\" (pwd())"
  "*A command to use for directory syncing.
This print a string that is captured by this mode for setting the prompt, so if
it is changed, make sure you change the occurrence of CURDIR below.")

(defvar nuprl-should-init-ml nil "Internal.  Do not change.")
(defvar nuprl-cur-prompt nil "Internal.  Do not change.")

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

(defun nuprl-preoutput-filter (str)
  ;; Look for directory sync commands, set prompt accordingly.
  (if (string-match "CURDIR=:=\\(.*\\)\n" str)
    (progn (setq nuprl-cur-prompt (format nuprl-prompt (match-string 1 str)))
           (setq str (concat (substring str 0 (match-beginning 0))
                             (substring str (match-end 0))))))
  ;; Delete nuprl-quiet-result strings.
  (while (string-match nuprl-quiet-result str)
    (setq str (concat (substring str 0 (match-beginning 0))
                      (substring str (match-end 0)))))
  ;; Print the nuprl-light prompt - only when we have the ocaml prompt as the
  ;; last string displayed.
  (if (and nuprl-cur-prompt (string-match "^# \\'" str))
    (concat (substring str 0 (match-beginning 0)) nuprl-cur-prompt)
    str))

(defun nuprl-output-filter (str)
  ;; Use a ^L character to put that place on the top line.
  (let ((len (length str)) (i -1))
    (while (< (setq i (1+ i)) len)
      (if (= (aref str i) ?\C-l)
        (save-excursion
          (let ((start (+ comint-last-output-start i)))
            (delete-region start (+ start 1))
            (goto-char start)
            (recenter 0))))))
  ;; If nuprl-light was just started, initialize it by sending the ml string.
  (if nuprl-should-init-ml
    (progn
      (process-send-string
       "*nuprl*"
       (format "%s;;\nprintf \"\\n\\n\" ; %s;;\n"
               nuprl-start-ml-command nuprl-dir-sync))
      (setq nuprl-should-init-ml nil))))

(defun nuprl-input-filter (str)
  ;; This uses an ugly hack: It changes the value of "input" that holds text to
  ;; send to the shell.  This is because the input-filter functions' output is
  ;; not taken as the new input.
  (let (;; Get the contents of the current line.
        (line (save-excursion
                (set-buffer "*nuprl*")
                (forward-line -1)
                (buffer-substring (progn (beginning-of-line) (point))
                                  (progn (end-of-line) (point))))))
    (cond
      ;; If we have a nuprl prompt, go ahead with the aliases etc.
      ((string-match (regexp-quote nuprl-cur-prompt) line)
       (let* ((args (split-string input))
              (alias
               (cdr
                (or
                 (assoc (format "%s|%s" (car args) (length (cdr args)))
                        nuprl-aliases)
                 (assoc (car args) nuprl-aliases)))))
         ;; Do the argument substitution.
         (and alias (setq input (replace-dollar-substrings alias args)))
         ;; Shorthand cd commands.
         (and (not alias)
              args
              (null (cdr args))
              (eq (aref (car args) (1- (length input))) ?/)
              (setq input (concat "cd(\"" (car args) "\")")))
         ;; Append a prompt-directory synchronization command.
         (if (string-match nuprl-dir-command input)
           (setq input (concat input " ; " nuprl-dir-sync)))
         ;; Convert empty input to a "()".
         (if (string-match "^[ \t]*$" input)
           (setq input "();;"))
         ;; Append a ";;" if needed.
         (if (not (string-match ";;[ \t]*$" input))
           (setq input (concat input ";;")))))
      ;; No nuprl prompt - still translate the NL alias.
      ((equal input nuprl-start-alias)
       (setq input nuprl-start-shell-command)
       (setq nuprl-should-init-ml t)
       ;; Initialize an empty prompt.
       (setq nuprl-cur-prompt (format nuprl-prompt "?"))))))

;; Set a shell for nuprl-light.
(defun nuprl ()
  "Start a shell with a nice interface to nuprl-light.
It is a simple shell so you can do whatever you want in it.  If \"NL\" is
entered, then the shell will automatically go to the nuprl-light/editor/ml
directory, run nl and enter an automatic #use command.  Once in nuprl-light, a
special prompt will be displayed, and alias convertion will be performed."
  (interactive)
  (if (comint-check-proc "*nuprl*")
    ;; Have a nuprl-light shell running, use it.
    (pop-to-buffer "*nuprl*")
    (let ((shell-mode-hook
           ;; Rename the shell-buffer as soon as it is created.  This method
           ;; will cause problems when there is no *nuprl* buffer, but there is
           ;; a *shell*  already - that *shell* will be used and renamed as the
           ;; *nuprl* buffer.
           '(lambda (&rest args) (rename-buffer "*nuprl*"))))
      ;; Start the shell.
      (shell)
      (set-buffer (get-buffer "*nuprl*"))
      ;; Display all chars correctly.
      (standard-display-8bit 128 255)
      ;; Set the nuprl-light font.
      (set-default-font (if (eq window-system 'nt)
                          "-*-nl12-r-normal-*-12-96-*-*-c-*-*-ansi-*"
                          "nl12"))
      ;; Set the filter functions locally.
      (make-local-variable 'comint-output-filter-functions)
      (setq comint-output-filter-functions 'nuprl-output-filter)
      (make-local-variable 'comint-preoutput-filter-functions)
      (setq comint-preoutput-filter-functions '(nuprl-preoutput-filter))
      (make-local-variable 'comint-input-filter-functions)
      (setq comint-input-filter-functions 'nuprl-input-filter)
      (setq nuprl-cur-prompt (format nuprl-prompt "?"))
      ;; Just for continence
      (setq default-directory "~/nuprl-light/editor/ml/"))))
