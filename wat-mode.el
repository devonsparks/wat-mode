;;
;;   Copyright (C) 2018, Devon D.Sparks
;;   URL: https://github.com/devonsparks/wat-mode
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


(defconst *wat-comment-token* ";;"
  "wat comment token - used for annotating macro expansions.")


(defconst *wat-macro-tag* "wat-macro"
  "Prefix used to identify all macro expansions.")

(defalias 'wat-output 'pp)


(defmacro define-wat-macro (name args &rest body)
    "Extends wat syntax with simple macro expansions."    
    (let ((tag (make-symbol *wat-macro-tag*))
	  (name-string (symbol-name name)))
      `(defmacro ,name ,(append args '(&rest body))       
	 (backquote (,tag ,name-string ,@body)))))


(defalias '@ 'define-wat-macro
  "@ extends wat syntax to support macro expansions.")

;;;;;;;;;

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


;;;;;;;;;;;;;;;;


(setq wat-font-highlights
      '(       
	;; memory instructions
	("get_local\s-*\\|set_local\s-*\\|tee_local\s-*"                        . font-lock-function-name-face)
	("get_global\s-*\\|set_global\s-*"                                      . font-lock-function-name-face)
	("align\s-*\\|offset\s-*"                                               . font-lock-function-name-face)
	("load8_u\s-*\\|load8_s\s-*"                                            . font-lock-function-name-face)
	("load16_u\s-*\\|load16_s\s-*"                                          . font-lock-function-name-face)
	("load32_u\s-*\\|load32_s\s-*"                                          . font-lock-function-name-face)
	("load\s-*"                                                             . font-lock-function-name-face)
	("store\s-*\\|store8\\store16\s-*\\|store32\s-*"                        . font-lock-function-name-face)	
	("memory.size\s-*\\|memory.grow\s-*"                                    . font-lock-function-name-face)

	;; operations
	("extend_s/i32\s-*\\|extend_u/i32\s-*"                                  . font-lock-builtin-face)
	("convert_s/i32\s-*\\|convert_u/i32\s-*"                                . font-lock-builtin-face)
	("convert_s/i64\s-*\\|convert_u/i64\s-*"                                . font-lock-builtin-face)
	("demote/f64\s-*\\|promote/f32\s-*"                                     . font-lock-builtin-face)
	("reinterpret/f32\s-*\\|reinterpret/f64\s-*"                            . font-lock-builtin-face)
	("const\s-*\\|clz\s-*\\|ctz\s-*\\|popcnt\s-*"                           . font-lock-builtin-face)
	("add\s-*\\|sub\s-*\\|mul\s-*"                                          . font-lock-builtin-face)
	("div\s-*\\|div_s\s-*\\|div_u\s-*\\|rem_s\s-*\\|rem_u\s-*"              . font-lock-builtin-face)
	("and\s-*\\|or\s-*\\|xor\s-*"                                           . font-lock-builtin-face)
	("shl\s-*\\|shr_s\s-*\\|shr_u\s-*\\|rotl\s-*\\|rotr\s-*"                . font-lock-builtin-face)
	("abs\s-*\\|neg\s-*\\|ceil\s-*\\|floor\s-*\\|trunc\s-*"                 . font-lock-builtin-face)
	("nearest\s-*\\|sqrt\s-*\\|min\s-*\\|max\s-*\\|copysign\s-*"            . font-lock-builtin-face)
	("eqz\s-*\\|eq\s-*\\|ne\s-*\\|lt\s-*\\|lt_s\s-*\\|lt_u\s-\\|le_u\s-*"   . font-lock-builtin-face)
	("gt\s-*\\|gt_s\s-*\\|gt_u\s-*\\|le_s\s-*\\|ge_s\s-*\\|ge_u\s-*"        . font-lock-builtin-face)
	("wrap/i64\s-*"                                                         . font-lock-builtin-face)
	("trunc_s/f32\s-*\\|trunc_u/f32\s-*\\|trunc_s/f64\s-*\\|trunc_u/64\s-*" . font-lock-builtin-face)
	("br\s-*\\|br_if\s-*\\|br_table\s-*"                                    . font-lock-builtin-face)
	("nop\s-*\\|unreachable\s-*\\|return\s-*\\|call_indirect\\|call\s-*"    . font-lock-builtin-face)
	("drop\s-*\\|select\s-*"                                                . font-lock-builtin-face)
	
	;; types
	("i32[\s-.]*\\|i64[\s-*.]\\|f32[\s-*.]\\|f64[\s-*.]"                    . font-lock-type-face)
	("local\s-*\\|param\s-*\\|result\s-*"                                   . font-lock-type-face) 
	("anyfunc\s-*"                                                          . font-lock-type-face)

        ;; top-level forms
        ("module"                                                               . font-lock-keyword-face)
        ("type\s-*\\|func\s-*\\|elem\s-*\\|data\s-*\\|global\s-*"               . font-lock-keyword-face)
        ("table\s-*\\|memory\s-*\\|start\s-*\\|import\s-*\\|export\s-*"         . font-lock-keyword-face)

        ;; groupings
        ("loop\s-*\\|block\s-*\\|if\s-*\\|else\s-*\\|end\s-*"                   . font-lock-regex-grouping-construct)))
		
	
	
(defvar wat-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map (kbd "C-c 1")   'wat-macro-expand)
    map)
  "Keymap for wat-mode, derived from lisp-mode.")


(define-derived-mode wat-mode lisp-mode "wat-mode"
  "Major mode for editing WebAssembly's text encoding."
  (use-local-map wat-mode-map)
  (setq font-lock-defaults '(wat-font-highlights)))


(add-to-list 'auto-mode-alist '("\\.wat\\'" . wat-mode))


(provide 'wat-mode)




