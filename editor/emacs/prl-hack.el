;;; prl-hack.el --- hack a convenient MetaPRL shell.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@cs.cornell.edu)

;; A MetaPRL friendly shell.
;; M-x metaprl will start a shell that provide a nice interface to MetaPRL.
;; Short summary:
;; 1. It is a simple shell so you can do whatever you want in it.
;; 2. Guess that MetaPRL is running if input ends with a "\n# ".
;; 3. If "MP" is entered on the shell, automatically go to the editor/ml dir,
;;    run mp, open some modules and enter an automatic #use command.
;; 4. Prints a MetaPRL prompt instead of the wonderful "# ".  Shows the current
;;    pwd() there - using an automatic printf, catching its output.
;; 5. Input goes thru an alias replacing mechanism.
;; 6. Input is one line only, ";;" is automatically appended when missing.  It
;;    is still possible to paste multiple-line stuff in, you can also enter C-q
;;    C-j to begin a new line (the ";;" will not be appended in this case).
;; 7. Hide unnecessary output: "unit = ()".
;; 8. A string of the form ".../" is translated to "cd (\"...\")".


;;; Commentary:
;; 

;;; Code:
(defvar metaprl-aliases
  '(("cd|1"   . "ignore (cd\"$1\")")
    ("cd|0"   . "pwd()")
    ("..|0"   . "ignore (cd\"..\")")
    ("~|0"    . "ignore (cd\"~\")")
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

(defvar metaprl-dir-command
  "^\\([( ]\\|ignore\\)*\\(cd\\|up\\|down\\|root\\)\\>"
  "*A regexp for commands that require a prompt-directory synchronization.")

(defvar metaprl-prompt "MetaPRL %s > "
  "*A \"metaprl\" prompt to display when working with the MetaPRL shell.
The string should contain %s which will be replaced by the current-path.")

(defvar metaprl-start-alias "MP"
  "*The shell `alias' that will be used to start MetaPRL.")

(defvar metaprl-home-directory (expand-file-name "~/meta-prl")
  "*The MetaPRL root directory.")

(defvar metaprl-editor-directory
  (expand-file-name "editor/ml" metaprl-home-directory)
  "*The directory containing the 'mp' program.")

(defvar metaprl-start-shell-command
  (format "cd %s ; ./mp\n" metaprl-editor-directory)
  "*The shell command used to start MetaPRL.
Change the cd command to wherever you have mp.")

(defvar metaprl-init-script "~/.mprc"
  "*The ml command used to initialize MetaPRL.")

(defvar metaprl-modules-to-open '("Printf" "Mp")
  "*A list of strings that need to be opened on startup.
Add stuff, but don't remove.")

(defvar metaprl-quiet-result "- : unit = ()\n"
  "*Regexp for output lines that will not be shown on the output.
Default is a () result.")

(defvar metaprl-dir-sync "printf \"CURDIR=:=%s\\n\" (pwd())"
  "*A command to use for directory syncing.
This print a string that is captured by this mode for setting the prompt, so if
it is changed, make sure you change the occurrence of CURDIR below.")

(defvar metaprl-window-size 20
  "*The size of the `meta-prl' buffer.
An integer smaller than 100, the precent of buffer heigh, or nil - default.")

(defvar metaprl-should-init-ml    nil "Internal.  Do not change.")
(defvar metaprl-cur-prompt        nil "Internal.  Do not change.")
(defvar metaprl-initialized-emacs nil "Internal.  Do not change.")

(defun metaprl-replace-dollar-substrings (str args)
  "Replace $N's in STR by the corresponding element of ARGS."
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
  "Prefilter for output string STR.
This is responsible for setting and displaying the prompt, and deleting
uninformative output stuff specified by `metaprl-quiet-result'."
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
  (if (string-match "\\`# \\'" str)
    (if metaprl-cur-prompt
      (concat (substring str 0 (match-beginning 0)) metaprl-cur-prompt)
      "")
    str))

(defun metaprl-output-filter (str)
  "Filter for output string STR.
Use a ^L character to put that place on the top line and also handle
initialization - must wait until the process is ready to initialize."
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
       (format
        "open %s;;\n%s;;\nprintf \"\\n\\n\";%s;;\n"
        ;; Open modules (note: need at least one for this)
        (mapconcat 'identity metaprl-modules-to-open ";;\nopen ")
        ;; #use a script if it exists
        (if (and metaprl-init-script
                 (file-readable-p (expand-file-name metaprl-init-script)))
          (format "#use \"%s\"" (expand-file-name metaprl-init-script))
          "()")
        ;; Initial prompt sync.
        metaprl-dir-sync))
      (setq metaprl-should-init-ml nil))))

(defun metaprl-input-filter (str)
  "Filter for input string STR.
Expand aliases etc...  (the good stuff is in here.)"
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
      ((and metaprl-cur-prompt
            (string-match (regexp-quote metaprl-cur-prompt) line))
       (let* ((args (split-string input))
              (alias
               (cdr
                (or
                 (assoc (format "%s|%s" (car args) (length (cdr args)))
                        metaprl-aliases)
                 (assoc (car args) metaprl-aliases)))))
         ;; Do the argument substitution.
         (and alias
              (setq input (metaprl-replace-dollar-substrings alias args)))
         ;; Shorthand cd commands.
         (and (not alias)
              args
              (null (cdr args))
              (eq (aref (car args) (1- (length input))) ?/)
              (setq input (concat "ignore(cd\"" (car args) "\");ls()")))
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
       (setq metaprl-cur-prompt nil)))))

(defun metaprl-shell ()
  "Same as `shell', modified to use *meta-prl* as the buffer name."
  (interactive)
  (if (not (comint-check-proc "*meta-prl*"))
    (let* ((prog (or explicit-shell-file-name
                     (getenv "ESHELL")
                     (getenv "SHELL")
                     "/bin/sh"))
           (name "meta-prl")
           (startfile (concat "~/.emacs_" name))
           (xargs-name (intern-soft (concat "explicit-" name "-args")))
           shell-buffer)
      (save-excursion
        (set-buffer (apply 'make-comint "meta-prl" prog
                           (if (file-exists-p startfile) startfile)
                           (if (and xargs-name (boundp xargs-name))
                             (symbol-value xargs-name)
                             '("-i"))))
        (setq shell-buffer (current-buffer))
        (shell-mode))
      (pop-to-buffer shell-buffer))
    (pop-to-buffer "*meta-prl*")))

;; Set a shell for MetaPRL.
(defun meta-prl ()
  "Start a shell with a nice interface to MetaPRL.
It is a simple shell so you can do whatever you want in it.  If \"MP\" is
entered, then the shell will automatically go to the meta-prl/editor/ml
directory, run mp and enter an automatic #use command.  Once in MetaPRL, a
special prompt will be displayed, and alias convertion will be performed."
  (interactive)
  (require 'comint)
  (require 'shell)
  (let ((shell-mode-hook
         ;; Rename the shell-buffer as soon as it is created.  This method
         ;; will cause problems when there is no *meta-prl* buffer, but there
         ;; is a *shell*  already - that *shell* will be used and renamed as
         ;; the *meta-prl* buffer.
         '(lambda (&rest args) (rename-buffer "*meta-prl*"))))
    ;; Start the shell, or switch to an existing one.
    (metaprl-shell)
    ;; Make the window no more than metaprl-window-size precent of the frame.
    (and metaprl-window-size (> metaprl-window-size 0)
         (< 1 (count-windows)) (= (window-width) (frame-width))
         (shrink-window
          (max 0 (- (window-height)
                    (/ (* metaprl-window-size (frame-height)) 100)))))
    ;; Initialize Emacs only once.
    (if (not metaprl-initialized-emacs)
      (progn
        (setq metaprl-initialized-emacs t)
        ;; Display all chars correctly.
        (standard-display-8bit 128 255)
        ;; Set the MetaPRL font.
        ;; My copy of emacs20 (Debian package version 20.3-8) has a
        ;; bug where set-default-font removes the inverse-video from the
        ;; modeline.  This ugly workaround lets me avoid set-default-font
        ;; and shouldn't affect anyone else.  -- cwitty, 6/5/99
        (if (not (and (fboundp 'frame-parameters)
                      (equal (cdr (assoc 'font (frame-parameters))) "mp12")))
          (set-default-font (if (eq window-system 'nt)
                              "-*-mp12-r-normal-*-12-96-*-*-c-*-*-ansi-*"
                              "mp12")))))
    ;; Initialize the meta-prl buffer once.
    (make-local-variable 'metaprl-new-buffer)
    (if (not (and (boundp 'metaprl-new-buffer) metaprl-new-buffer))
      (progn
        (setq metaprl-new-buffer t)
        ;; Set the filter functions locally.
        (make-local-variable 'comint-output-filter-functions)
        (setq comint-output-filter-functions 'metaprl-output-filter)
        (make-local-variable 'comint-preoutput-filter-functions)
        (setq comint-preoutput-filter-functions '(metaprl-preoutput-filter))
        (make-local-variable 'comint-input-filter-functions)
        (setq comint-input-filter-functions 'metaprl-input-filter)
        (setq metaprl-cur-prompt nil)
        ;; Just for convenience
        (setq default-directory
              (file-name-as-directory metaprl-editor-directory))))))

(provide 'prl-hack)

;;; prl-hack.el ends here
