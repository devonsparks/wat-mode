(defconst *wat-comment-token* ";;"
  "wat comment token - used for annotating macro expansions.")

(defconst *wat-macro-tag* 'wat-macro
  "Prefix used to identify all macro expansions.")

(defconst *wat-macro-tag-sep* ":"
  "Separator character for macro expansion tags")

(defalias 'wat-printer 'pp
  "Procedure used to format expressions during macro expansion.")

(defun wat-macro-tag (name)
  "Built a symbol tag of wat macro NAME in the current buffer"
  (intern (mapconcat 'identity
		     (list (symbol-name *wat-macro-tag*)
			   (symbol-name name))
		     *wat-macro-tag-sep*)))


(defmacro wat-macro (name args &rest body)
    "Extends wat syntax with simple macro expansions."    
    (let ((tag (wat-macro-tag name)))
      `(defmacro ,name ,(append args '(&rest body))
	   (cons (quote ,tag)
		 (backquote ,body)))))


(defun wat-macro-sanitize-1 ()
  "Replaces the next wat macro tag occurrence in the 
   current buffer at point with its commented equivalent.

   Returns: t if expansion succeeded; null otherwise."
    (goto-char (search-forward (symbol-name *wat-macro-tag*)))
    (goto-char (search-backward "("))
    (save-excursion
      (delete-char 1)
      (insert *wat-comment-token*)
      (goto-char (forward-list))
      (delete-char -1))
    (goto-char (search-forward " " nil t)))


(defun wat-macro-sanitize ()
  "Repeatly applies macro expansion on the current buffer
   at point until no more expansions are possible."
  (let ((more t))
    (while more
      (setq more (wat-macro-sanitize-1)))))


(defun wat-expand (form buffer-name)
  "Given a wat-macro FORM and buffer BUFFER-NAME,
   expand FORM and annotate expansions with macro comments."
  (wat-printer form (get-buffer-create buffer-name))
  (switch-to-buffer buffer-name)
  (beginning-of-buffer)
  (wat-macro-sanitize))


(defmacro wat-module (&rest exp)
  "A macro replacement for wat's macro top-level syntax."
  `(cons 'module
	 (mapcar 'macroexpand-all
		 (quote ,exp))))

(defalias '@ 'wat-macro
  "@ extends wat syntax to support macro expansions.")

(defalias 'module 'wat-module
  "Bind macro expander to match wat's top-level syntax.")

;;;

(setq wat-font-highlights
      '(("module" . font-lock-warning-face)
	("i32\\|i64\\|f32\\|f64" . font-lock-type-face)   
	("type\\|func\\|elem\\|data\\|param\\|result" . font-lock-type-face) 
        ("anyfunc"               . font-lock-type-face)
	("loop\\|block\\|if\\|else\\|end" .
	 font-lock-regex-grouping-construct)           
        ("unreachable\\|nop\\|br\\|br_if\\|br_table\\|return\\|call\\call_indirect" .
	 font-lock-regex-grouping-construct)           
	("drop\\|select" . font-lock-preprocessor-instructions) 
	("local\\|get_local\\|set_local\\|tee_local\\|get_global\\|set_global" .
	 font-lock-variable-name-face)                          
	("align\\|offset" . font-lock-function-name-face)
	("load" . font-lock-function-name-face)
	("load8_u\\|load8_s" . font-lock-function-name-face)
	("load16_u\\|load16_s" . font-lock-function-name-face)
	("load32_u\\|load32_s" . font-lock-function-name-face)
	("store\\|store8\\store16\\|store32" . font-lock-function-name-face)
	("memory\\|size\\|memory\.grow" . font-lock-function-name-face)
	("const\\|clz\\|ctz\\|popcnt" . font-lock-builtin-face)
	("add\\|sub\\|mul\\|div\\|div_s\\|div_u\\|rem_s\\|rem_u" . font-lock-builtin-face)
	("and\\|or\\|xor\\|shl\\|shr_s\\|shr_u\\|rotl\\|rotr" . font-lock-builtin-face)
	("abs\\|neg\\|ceil\\|floor\\|trunc" . font-lock-builtin-face)
	("nearest\\|sqrt\\|min\\|max\\|copysign" . font-lock-builtin-face)
	("eqz\\|eq\\|ne\\|lt\\|lt_s\\|lt_u\\|gt\\|gt_s\\|gt_u\\|le_s\\|le_u\\|ge_s\\|ge_u" . font-lock-builtin-face)
	("wrap/i64\\|trunc_s/f32\\|trunc_u/f32\\|trunc_s/f64\\|trunc_u/64" . font-lock-builtin-face)
	("extend_s/i32\\|extend_u/i32" . font-lock-builtin-face)
	("convert_s/i32\\|convert_u/i32\\|convert_s/i64\\|convert_u/i64" . font-lock-builtin-face)
	("demote/f64\\|promote/f32" . font-lock-builtin-face)
	("reinterpret/f32\\|reinterpret\f64" . font-lock-builtin-face)
	("import\\|export\\|start\\|table\\|memory\\|memory\\|global" . font-lock-keyword-face)))
	
	



(defvar wat-mode-map
  (let ((smap (make-sparse-keymap))
        (map (make-sparse-keymap "wat")))
    (set-keymap-parent smap lisp-mode-shared-map)
    (define-key map [run-wat-interpreter] '("Run wat interpreter" . run-scheme))
    (define-key map [uncomment-region]
      '("Uncomment Out Region" . (lambda (beg end)
                                   (interactive "r")
                                   (comment-region beg end '(4)))))
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))
    (define-key map (kbd "C-c C-@") 'wat-)
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)
    smap)
  "Keymap for wat-mode.
   All commands in `lisp-mode-shared-map' are inherited by this map.")


(define-derived-mode wat-mode lisp-mode "wat-mode"
  "Major mode for editing WebAssembly's text encoding."
  (use-local-map wat-mode-map)
  (setq font-lock-defaults '(wat-font-highlights)))






(provide 'wat-mode)
