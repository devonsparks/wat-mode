;;; wat-mode-macro.el --- Assembler macros for `wat-mode' -*- lexical-binding: t; -*-

;;  Copyright (C) 2018, Devon Sparks
;;  URL: https://github.com/devonsparks/wat-mode
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;

;;; Code:

(defconst *wat-macro-tag* "wat-macro"
  "Prefix used to identify all macro expansions.")

(defconst *wat-comment-prefix* ";;"
  "Comment prefix used for annotating expanded `wat-mode' macros.")


(defalias 'wat-output 'pp
  "`wat-output' aliases the formatting function for expanded macros.

Its arguments should follow the `pp' convention, e.g.,
(wat-output object &optional stream)")


(defmacro define-wat-macro (name args &rest body)
    "Extends wat syntax with simple macro expansions ('wat-macros').

NAME: the macro's name (to be referenced in later wat code)
ARGS: an in-line argument list to the macro.
BODY: the form to be expanded.

Macros created with `define-wat-macro' are true elisp macros.
Quasiquote works as you'd expect, e.g.,:

'(@ define-register (name initial-value)
  (global ,name (mut i32) (i32.const ,initial-value)))'

wat-macros can be evaluated in the current session with `eval-last-sexp'
and expanded in place by invoking `wat-macro-expand' with point at the
start of the form."
    (let ((tag (make-symbol *wat-macro-tag*))
	  (name-string (symbol-name name)))
      `(defmacro ,name ,(append args '(&rest body))
	 (backquote (,tag ,name-string ,@body)))))


(defalias '@ 'define-wat-macro
  "@ is syntactic sugar for `define-wat-macro'.")


(defun wat-expand-in-place (&optional forward)
  "`macroexpand-all' the s-expression at point.

FORWARD: If non-nil, `macroexpand-all' the s-expression following point.
Otherwise, use s-expression preceding point.

If an error occurs during macro expansion, re-insert the original form."
  (interactive)
  (save-excursion
  (if forward
      (kill-sexp 1)
      (backward-kill-sexp))
  (condition-case nil
      (let ((exp (read (current-kill 0))))
	(progn
	  (wat-output (macroexpand-all exp)
		      (current-buffer))))
	 ;; (indent-region (point-min) (point-max) nil)))
      ('error (message "Invalid expression")
	     (insert (current-kill 0))))))


(defun wat-strip-wrapper-1 ()
  "Delete s-expression wrapping `define-wat-macro' expansions.

All wat macros expand to an s-expression starting with `*wat-macro-tag*'.

Example:

\(@ my-macro ()  <body>)

...

\(my-macro) => \(,*wat-macro-tag* \"my-macro\" <body>)

The leading wat-macro is useful for debugging but isn't valid WebAssembly.
To fix this, `wat-strip-wrapper-1' starts at point, looks for the next
occurrence  of `*wat-macro-tag*', commenting out the tag and deleting
the wrapping parens.  So the expanded macro above becomes:
;; ,*wat-macro-tag* \"my-macro\"
		     <body>"
  (let* ((label (concat "(" *wat-macro-tag*))
	 (labelc (length label))
	 (pstart (progn
		   (search-forward label nil nil)
		   (goto-char (- (point) labelc))
		   (point)))
	 (pend (progn
		 (forward-list)
		 (point))))
    (goto-char pstart)
    (delete-char labelc)
    (insert *wat-comment-prefix*)
    (goto-char (- pend labelc))
    (delete-char 1)
    (goto-char pstart)))


(defun wat-strip-wrapper ()
  "Repeatedly call `wat-strip-wrapper-1', starting at point, until failure."
  (interactive)
  (save-excursion
    (condition-case nil
	(while t
	  (wat-strip-wrapper-1))
      ('error  (message "Macro expansion complete!")))))


(defun wat-remove-escapes ()
  "Remove escape characters created during macro expansion."
  (interactive)
  (save-excursion
  (while (search-forward "\\" nil t)
       (replace-match "" t nil))))


(defun wat-mode-macro-expand ()
  "Perform wat macro expansion on the current buffer."
  (interactive)
  (wat-expand-in-place t)
  (wat-strip-wrapper)
  (wat-remove-escapes))

(provide 'wat-mode-macro)

;;; wat-mode-macro.el ends here
