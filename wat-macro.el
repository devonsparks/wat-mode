;;
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

(defconst *wat-macro-tag* "wat-macro"
  "Prefix used to identify all macro expansions.")

(defconst *wat-comment-token* ";;"
  "wat comment token - used for annotating macro expansions.")

(defalias 'wat-output 'pp)

(defmacro define-wat-macro (name args &rest body)
    "Extends wat syntax with simple macro expansions."    
    (let ((tag (make-symbol *wat-macro-tag*))
	  (name-string (symbol-name name)))
      `(defmacro ,name ,(append args '(&rest body))       
	 (backquote (,tag ,name-string ,@body)))))


(defalias '@ 'define-wat-macro
  "@ extends wat syntax to support macro expansions.")


(defun wat-expand-in-place (&optional forward)
  "Replace an sexp with its value. "
  (interactive)
  (save-excursion
  (if forward
      (kill-sexp 1)
      (backward-kill-sexp))
  (condition-case nil
      (let ((exp (read (current-kill 0))))
	(wat-output (macroexpand-all exp)
		 (current-buffer)))  
      ('error (message "Invalid expression")
	     (insert (current-kill 0))))))


(defun wat-sanitize-1 ()
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
    (insert *wat-comment-token*)
    (goto-char (- pend labelc))
    (delete-char 1)
    (goto-char pstart)))


(defun wat-strip-wrapper ()
  (interactive)
  (save-excursion 
    (condition-case nil
	(while t
	  (wat-sanitize-1))
      ('error  (message "Macro expansion complete!")))))


(defun wat-remove-escapes ()
  (interactive)
  (save-excursion
  (while (search-forward "\\" nil t)
       (replace-match "" t nil))))


(defun wat-macro-expand ()
  (interactive)
  (wat-expand-in-place t)
  (wat-strip-wrapper)
  (wat-remove-escapes))

(provide 'wat-macro)

;; wat-macro.el ends here
