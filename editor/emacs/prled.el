;; prled.el --- module--browsing commands
;;
;; This is derived somewhat from dired.el from the
;; standard GNU emacs distribution.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Run after loading this file
(defvar prled-load-hook nil
  "Run after loading prled.
You can customize key bindings or load extensions with this.")

;; Run after changing the mode
(defvar prled-mode-hook nil
  "Run at the very end of prled-mode.")

;; This is the function to which output is sent
(defvar prled-output-function nil
  "Function + arg to call on PRLed view command")

(defvar prled-view-function nil
  "Function + arg to call on PRLed view command")

;; The cursor is not allowed before this position
(defvar prled-min-marker nil
  "The cursor is not allowed before this position")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode and key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar prled-mode-syntax-table nil
  "Syntax table for PRLed mode")
(if prled-mode-syntax-table
    ()
  (setq prled-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_ "w" prled-mode-syntax-table))

(defvar prled-mode-map nil "Local keymap for prled-mode buffers.")
(if prled-mode-map
    nil
  (let ((map (make-keymap)))
    (suppress-keymap map)

    ;; Navigation
    (define-key map "v" 'prled-view-module)
    (define-key map "^" 'prled-quit-module)
    (define-key map "q" 'prled-quit-module)
    (define-key map "u" 'prled-quit-module)
    (define-key map "n" 'prled-next-module)
    (define-key map "\C-n" 'prled-next-module)
    (define-key map "j" 'prled-next-module)
    (define-key map "p" 'prled-prev-module)
    (define-key map "\C-p" 'prled-prev-module)
    (define-key map "k" 'prled-prev-module)

    (setq prled-mode-map map)))

;; PRLed mode is suitable only for specially formatted data.
(put 'prled-mode 'mode-class 'special)

(defun prled-mode ()
  "\
Mode for \"editing\" NuPRL-Light module listings.
In prled, you are \"editing\" a list of the modules in the system.
Each module is a line in the buffer.  You can move using the usual
cursor motion commands.\\<prled-mode-map>.
Letters no longer insert themselves, much like dired mode.
Type \\[prled-view-module] to view the contents of a module.

Customization variables (rename this buffer and type \\[describe-variable] on each line
for more info):

   (Actually, no local variables right now -- jyh 1/17/96)

Hooks (use \\[describe-variable] to see their documentation):

  prled-mode-hook
  prled-load-hook

Keybindings:
\\{prled-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map prled-mode-map)
  (set-syntax-table prled-mode-syntax-table)
  (setq major-mode 'prled-mode
	mode-name "PRLed"
	case-fold-search nil
	buffer-read-only t
	mode-line-buffer-identification '("PRLed: %17b"))

  ;; Local values
  (make-local-variable 'prled-output-function)
  (make-local-variable 'prled-min-marker)
  (setq prled-output-function prled-view-function)
  (setq prled-min-marker (point-min-marker))
  (prled-set-min-marker)

  ;; Highlight all comment lines
  (prled-highlight-comments)
  (goto-char prled-min-marker)
  (prled-move-to-module-name)

  ;; Fire up
  (run-hooks 'prled-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPLEMENTATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Set the min point
;;
(defun prled-set-min-marker ()
  "Find the minimum position for the min marker.
At the same time, highlight the text before the min
position"
  (let ((start (point-min))
	end over)
    (goto-char start)
    (setq end (re-search-forward "^-" (point-max) 1))
    (forward-line)
    (set-marker prled-min-marker (point))
    (setq over (make-overlay start prled-min-marker))
    (overlay-put over 'face 'italic)
    (overlay-put over 'intangible t)
    (overlay-put over 'evaporate t)))

;;
;; Highlight all the comments in the file
;;
(defun prled-highlight-comments ()
  "Highlight all the comments in prled mode"
  (let (bol eol nol)
    (goto-char prled-min-marker)
    (while (progn
	     (skip-syntax-forward " ")
	     (setq bol (point))
	     (end-of-line)
	     (setq eol (point))
	     (forward-line)
	     (setq nol (point))
	     (cond ((= bol nol) nil)
		   ((equal (buffer-substring bol (+ bol 1)) "-")
		    (let* ((bolx (if (> bol (point-min)) (1- bol) bol))
			   (over (make-overlay bolx nol)))
		      (overlay-put over 'face 'italic)
		      (overlay-put over 'intangible t)
		      (overlay-put over 'evaporate t)))
		   (t t))))))

;;
;; View the module under the cursor
;;
(defun prled-view-module-aux (code)
  "View the module at the cursor"
  (prled-move-to-module-name)
  (let ((name (prled-get-module-name)))
    (funcall (car prled-output-function)
	     (cdr prled-output-function)
	     code
	     name)))

(defun prled-view-module ()
  "View the module at the cursor"
  (interactive)
  (prled-view-module-aux 'view))

(defun prled-quit-module ()
  "View the parent module"
  (interactive)
  (prled-view-module-aux 'quit))

;;
;; Move down to the next module.
;;
(defun prled-next-module (arg)
  "Select the next module.  Optional argument specifies how many modules."
  (interactive "p")
  (next-line arg)
  (prled-move-to-module-name))

;;
;; Move up to the previous module.
;;
(defun prled-prev-module (arg)
  "Select the previous module.  Optional argument specifies how many modules."
  (interactive "p")
  (previous-line arg)
  (beginning-of-line)

  ;; Search for a module name
  (let* ((beginn (point))
	 (startn beginn))

    ;; Keep going back until finally get something
    (while (progn (prled-move-to-module-name)
		  (and (>= startn prled-min-marker)
		       (> (point) beginn)))
      (goto-char startn)
      (forward-line -1)
      (beginning-of-line)
      (setq startn (point)))))

;;
;; Move the cursor to the start of the module name on this line.
;;
(defun prled-move-to-module-name ()
  "Move the cursor to the first character of the module name"
  (let (startn endn nextn)
    ;; Move forward until a legitimate line
    (while (progn
	     ;; Check min bounds
	     (if (< (point) prled-min-marker)
		 (goto-char prled-min-marker))

	     ;; Delimit the line
	     (beginning-of-line)
	     (skip-syntax-forward " ")
	     (setq startn (point))
	     (end-of-line)
	     (setq endn (point))
	     (forward-line)
	     (setq nextn (point))
	     (setq word (buffer-substring startn (+ startn 1)))
	     (set-text-properties 0 1 nil word)

	     ;; Continue until line contains something
	     (and (not (= nextn endn)) (equal word "-"))))

    ;; Move to the first char
    (goto-char startn)))

(defun prled-move-to-end-of-module-name ()
  "Move the cursor to the end of this module name.
The cursor should be within the module name"
  (end-of-line)
  (point))

;;
;; Get the name of the module at the current line.
;;
(defun prled-get-module-name ()
  "Get the name of the currently selected module"
  (let (p1 p2 module)
    ;; Locate the beginning and end of the module name
    (save-excursion
      (setq p1 (prled-move-to-module-name))
      (setq p2 (prled-move-to-end-of-module-name)))

    ;; Get the module name and strip face properties
    (if (setq module (and p1 p2 (buffer-substring p1 p2)))
	(set-text-properties 0 (length module) nil module))
    
    module))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRAILER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'prled)

(run-hooks 'prled-load-hook)
