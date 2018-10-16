
(defconst *wat-comment-token* ";;"
  "wat comment token - used for annotating macro expansions.")

(defconst *wat-macro-tag* "wat-macro"
  "Prefix used to identify all macro expansions.")

(defalias 'wat-fmt 'pp)


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
	(wat-fmt (macroexpand-all exp)
		 (current-buffer)))
      ('error (message "Invalid expression")
	     (insert (current-kill 0))))))

(defun wat-sanitize-1 ()
  (let* ((label (concat "(" *wat-macro-tag*))
	 (labelc (length label))	
	 (ostart (progn
		   (search-forward label nil nil)
		   (goto-char (- (point) labelc))
		   (point)))
	 (oend (progn
		 (forward-list)
		 (point))))
    (goto-char ostart)
    (delete-char labelc)
    (insert *wat-comment-token*)
    (goto-char (- oend labelc))
    (delete-char 1)
    (goto-char ostart)))


(defun wat-sanitize ()
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
  (wat-sanitize)
  (wat-remove-escapes))


(global-set-key (kbd "C-c 1") 'wat-macro-expand)


;;; wasm-interp support

(defvar wasm-interp-file-path "/usr/local/bin/wasm-interp"
  "Path to wasm-interp")

(defvar wat-desugar-file-path "/usr/local/bin/wat-desugar"
  "Path to wat-desugar")

(defvar wasm-interp-args '("--host-print")
  "Arguments to pass to `run-wasm-interp")

(defun run-wasm-interp ()
  "Run an inferior instance of `wasm-interp'"
  (interactive)
  (let*
      ((proc-name "wasm-interp")
       (proc-buf "*wasm-interp*")
       (buffer (comint-check-proc proc-name)))
   (pop-to-buffer-same-window
      (if (or buffer (not (derived-mode-p 'wat-mode))
              (comint-check-proc (current-buffer)))
          (get-buffer-create (or buffer proc-buf))
        (current-buffer)))
     (unless buffer
       (apply 'make-comint-in-buffer proc-name buffer
	      wasm-interp-file-path wasm-interp-args))))

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
  (let ((smap (make-sparse-keymap))
        (map (make-sparse-keymap "wat")))
    (set-keymap-parent smap lisp-mode-shared-map)
    (define-key map [run-wat-interpreter] '("Run wat interpreter" . run-scheme))
    (define-key map [uncomment-region]
      '("Uncomment Out Region" . (lambda (beg end)
                                   (interactive "r")
                                   (comment-region beg end '(4))))
)
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)
    smap)
  "Keymap for wat-mode, derived from lisp-mode.")


(define-derived-mode wat-mode lisp-mode "wat-mode"
  "Major mode for editing WebAssembly's text encoding."
  (use-local-map wat-mode-map)
  (setq font-lock-defaults '(wat-font-highlights)))


(provide 'wat-mode)




