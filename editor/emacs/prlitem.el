;; prlitem.el --- module--browsing commands
;;
;; This is derived somewhat from dired.el from the
;; standard GNU emacs distribution.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Run after loading this file
(defvar prlitem-load-hook nil
  "Run after loading prlitem.
You can customize key bindings or load extensions with this.")

;; Run after changing the mode
(defvar prlitem-mode-hook nil
  "Run at the very end of prlitem-mode.")

;; Function to call on view command
;; This usually is the command "quit"
(defvar prlitem-output-function nil
  "Function + arg to call on PRLitem view command")

(defvar prlitem-view-function nil
  "Function + arg to call on PRLitem view command")

;; The cursor is not allowed before this position
(defvar prlitem-min-marker nil
  "The cursor is not allowed before this position")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode and key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar prlitem-mode-syntax-table nil
  "Syntax table for PRLitem mode")
(if prlitem-mode-syntax-table
    ()
  (setq prlitem-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_ "w" prlitem-mode-syntax-table))

(defvar prlitem-mode-map nil
  "Local keymap for prlitem-mode buffers.")
(if prlitem-mode-map
    nil
  (let ((map (make-keymap)))
    (suppress-keymap map)

    ;; Navigation
    (define-key map "q" 'prlitem-quit)
    (define-key map "u" 'prlitem-quit)
    (define-key map "^" 'prlitem-quit)

    (setq prlitem-mode-map map)))

;; PRLitem mode is suitable only for specially formatted data.
(put 'prlitem-mode 'mode-class 'special)

(defun prlitem-mode ()
  "\
Mode for \"displaying\" NuPRL-Light module items.
In prlitem, you are \"editing\" an item in the system.
Type \\[prlitem-quit] to quit the module.

Customization variables (rename this buffer and type \\[describe-variable] on each line
for more info):

   (Actually, no local variables right now -- jyh 1/17/96)

Hooks (use \\[describe-variable] to see their documentation):

  prlitem-mode-hook
  prlitem-load-hook

Keybindings:
\\{prlitem-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map prlitem-mode-map)
  (set-syntax-table prlitem-mode-syntax-table)
  (setq major-mode 'prlitem-mode
	mode-name "PRLitem"
	case-fold-search nil
	buffer-read-only t
	mode-line-buffer-identification '("PRLitem: %17b"))

  ;; Local values
  (make-local-variable 'prlitem-output-function)
  (make-local-variable 'prlitem-min-marker)
  (setq prlitem-output-function prlitem-view-function)
  (setq prlitem-min-marker (point-min-marker))
  (prlitem-set-min-marker)

  ;; Fire up
  (run-hooks 'prlitem-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPLEMENTATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Set the min point
;;
(defun prlitem-set-min-marker ()
  "Find the minimum position for the min marker.
At the same time, highlight the text before the min
position"
  (let ((start (point-min))
	end over)
    (goto-char start)
    (setq end (re-search-forward "^-" (point-max) 1))
    (forward-line)
    (set-marker prlitem-min-marker (point))
    (setq over (make-overlay start prlitem-min-marker))
    (overlay-put over 'face 'italic)
    (overlay-put over 'evaporate t)))

;;
;; Highlight all the comments in the file
;;
(defun prlitem-highlight-comments ()
  "Highlight all the comments in prlitem mode"
  (let (bol eol nol)
    (save-excursion
      (goto-char prlitem-min-marker)
      (while (progn
	       (skip-syntax-forward " ")
	       (setq bol (point))
	       (end-of-line)
	       (setq eol (point))
	       (forward-line)
	       (setq nol (point))
	       (cond ((= bol nol) nil)
		     ((equal (buffer-substring bol (+ bol 1)) "-")
		      (let ((over (make-overlay (- bol 1) (+ eol 1))))
			(overlay-put over 'face 'italic)
			(overlay-put over 'evaporate t)))
		     (t t)))))))

;;
;; Send the quit command
;;
(defun prlitem-quit ()
  "Quit this display"
  (interactive)
  (funcall (car prlitem-output-function)
	   (cdr prlitem-output-function)
	   'quit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRAILER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'prlitem)

(run-hooks 'prlitem-load-hook)
