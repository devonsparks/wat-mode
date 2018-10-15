

(defconst *wat-comment-token* ";;"
  "wat comment token - used for annotating macro expansions.")

(defconst *wat-macro-tag* 'wat-macro
  "Prefix used to identify all macro expansions.")

(defconst *wat-macro-tag-sep* ":"
  "Separator character for macro expansion tags")

(defalias 'wat-fmt 'pp
  "Procedure used to format expressions during macro expansion.")


(defun eval-and-replace (&optional forward)
  "Replace an sexp with its value. If FORWARD is non-nil,
   kill the sexp following point. Otherwise, kill preceeding sexp."
  (interactive)
  (if forward
      (kill-sexp 1)
      (backward-kill-sexp))
  (condition-case nil
      (wat-fmt (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


(defun wat-expand-in-place (&optional forward)
  "Replace an sexp with its value. "
  (interactive)
  (if forward
      (kill-sexp 1)
      (backward-kill-sexp))
  (condition-case nil
      (let ((exp (read (current-kill 0))))
	(wat-fmt (macroexpand-all exp)
		 (current-buffer)))
      ('error (message "Invalid expression")
	     (insert (current-kill 0)))))

(defun wat-buffer-output-name (buffer-name)
  (concat buffer-name ".1"))

(defun wat-compile-module (&optional forward)
  (let ((buffer (get-buffer-create
		 (wat-buffer-output-name (buffer-name)))))
    (intern *wat-macro-tag*)
    (fset *wat-macro-tag* (lambda (name body) body))
    (with-current-buffer buffer
      (eval-and-replace forward))
    (unintern *wat-macro-tag*)))
	
(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key (kbd "C-c k") 'wat-expand-in-place)

     
(defmacro define-wat-macro (name args &rest body)
    "Extends wat syntax with simple macro expansions."    
    (let ((tag 'wat-macro)
	  (name-string (symbol-name name)))
      `(defmacro ,name ,(append args '(&rest body))       
	   (backquote (,tag ,name-string ,@body)))))
		 

(defmacro define-wat-module (&rest exp)
  "A macro replacement for wat's macro top-level syntax."
  (if (not (consp exp))
      (error (message "Module must be an s-expression")))
  (if (not (consp (car exp)))
      (error (message "Module must be enclosed in s-expression")))
  (if (not (equal (caar exp) 'module))
      (error (message "Module definition not found")))      
  `(macroexpand-all (quote ,(car exp))))












(defmacro wat-module-expand (buffer-or-name &rest module)
  (wat-module-really-expand buffer-or-name (quote module)))


  


(defun wat-module-check (module)
  (if (not (consp module))
      (error "Module must be an s-expression"))
  (if (and (consp module)
	   (not (equal (car module) 'module)))
      (error "Module must start with tag \"module\""))
  (if  (and (consp module)
	    (> (length module) 1))
      (error "Module contains more than one top-level s-expression")))
  
  
  

  

(defalias '@ 'define-wat-macro
  "@ extends wat syntax to support macro expansions.")


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

;;;

(setq wat-font-highlights
      '(("module" . font-lock-warning-face)

	;; operations
	("extend_s/i32\\|extend_u/i32" . font-lock-builtin-face)
	("convert_s/i32\\|convert_u/i32\\|convert_s/i64\\|convert_u/i64" . font-lock-builtin-face)
	("demote/f64\\|promote/f32" . font-lock-builtin-face)
	("reinterpret/f32\\|reinterpret/f64" . font-lock-builtin-face)
	("const\\|clz\\|ctz\\|popcnt" . font-lock-builtin-face)
	("add\\|sub\\|mul\\|div\\|div_s\\|div_u\\|rem_s\\|rem_u" . font-lock-builtin-face)
	("and\\|or\\|xor\\|shl\\|shr_s\\|shr_u\\|rotl\\|rotr" . font-lock-builtin-face)
	("abs\\|neg\\|ceil\\|floor\\|trunc" . font-lock-builtin-face)
	("nearest\\|sqrt\\|min\\|max\\|copysign" . font-lock-builtin-face)
	("eqz\\|eq\\|ne\\|lt\\|lt_s\\|lt_u\\|gt\\|gt_s\\|gt_u\\|le_s\\|le_u\\|ge_s\\|ge_u" . font-lock-builtin-face)
	("wrap/i64\\|trunc_s/f32\\|trunc_u/f32\\|trunc_s/f64\\|trunc_u/64" . font-lock-builtin-face)
        ("unreachable\\|nop\\|br\\|br_if\\|br_table\\|return\\|call\\call_indirect" .
	 font-lock-builtin-face)
	("drop\\|select" .  font-lock-builtin-face)
	
	;; types
	("i32\\|i64\\|f32\\|f64" . font-lock-type-face)   
	("local\\|param\\|result" . font-lock-type-face) 
        ("anyfunc"               . font-lock-type-face)

	;; top-level forms
	("type\\|func\\|elem\\|data\\|global\\|table\\|memory\\|start\\|import\\|export" . font-lock-keyword-face)
	
	;; groupings
	("loop\\|block\\|if\\|else\\|end"  . font-lock-regex-grouping-construct)           
		
	;; memory instructions
	("get_local\\|set_local\\|tee_local\\|get_global\\|set_global" .
	 font-lock-variable-name-face)                          
	("align\\|offset" . font-lock-function-name-face)
	("load8_u\\|load8_s" . font-lock-function-name-face)
	("load16_u\\|load16_s" . font-lock-function-name-face)
	("load32_u\\|load32_s" . font-lock-function-name-face)
	("load" . font-lock-function-name-face)
	("store\\|store8\\store16\\|store32" . font-lock-function-name-face)	
      	("memory\.size\\|memory\.grow" . font-lock-function-name-face)))
	
	

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



  
  
(defun wat-macro (name quoted-body)
  (insert (format ";; %s\n" name))
  (insert quoted-body))

(provide 'wat-mode)
(load "/home/devon/repos/wat-mode/demo.el")

(global-set-key (kbd "C-c j") 'wat-macro-expand-all)
