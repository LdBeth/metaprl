;;
;; Some utilities to use in CAML mode.
;;

(provide 'caml-util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MACROS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro when (condition &rest body)
  `(and ,condition (progn . ,body)))

(defmacro unless (condition &rest body)
  `(or ,condition (progn . ,body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mapcan (f l)
  "Map a function over a list and nconc the results"
  (if l (nconc (funcall f (car l)) (mapcan f (cdr l)))))

(defun mapfold (f a l)
  "Fold a function over a list.  The function takes an argument,
and element of the list, and returns a cons cell containing a new
argument and a new element of the list."
  (let (x new)
    (while l
      (setq x (funcall f a (car l)))
      (setq a (car x))
      (setq new (cons (cdr x) new))
      (setq l (cdr l)))
    (reverse new)))

(defun mapfilter (xxxf xxxl)
  "Map a function over a list like mapcar, but don't include nil results"
  (let (xxxnew xxxitem)
    (while xxxl
      (setq xxxitem (funcall xxxf (car xxxl)))
      (if xxxitem (setq xxxnew (cons xxxitem xxxnew)))
      (setq xxxl (cdr xxxl)))
    (reverse xxxnew)))

(defun it-list (f a l)
  "Fold a function over a list.  This function
takes three arguments: a function, and initial argument,
and the list.  The function is applied to each item in the list along
with the argument, and it computes a new value for the argument."
  (while l
    (setq a (funcall f a (car l)))
    (setq l (cdr l)))
  a)
