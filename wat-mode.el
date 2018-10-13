(require 'comint) 

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
  (goto-char (point-min))
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

;; ? Unused
(defun wat-module-yank ()
  "Assumes the buffer is active and cursor is over it"
    (if (not (search-backward "module"))
	(throw 'module-missing
	       (format "WebAssembly module tag not found. "
		       (buffer-file-name))))
    (backward-char) ;; on to open paren
    (kill-sexp))

(defun wat-format-canonical ()
  (let ((output-file (buffer-file-name)))
    (make-process
     :name "wat-desugar"
     :buffer nil
     :command (list wat-desugar-file-path
		    output-file
		    "-o"
		    output-file))))




(defun wat-expand-all (&optional format-canonical)
  (save-buffer)
  (let ((expanded-buffer (get-buffer-create (concat (buffer-file-name) ".1")))
	(start (point-min))
	(end   (point-max)))    
    (copy-region-as-kill start end)
    (with-current-buffer expanded-buffer
      (erase-buffer))
    (wat-expand (eval (read (current-kill 0)))
		expanded-buffer)))
      ;;(kill-new (format "%s" (eval (read (current-kill 0)))))
      ;;(yank))))
   ;; (print (eval (read (current-kill 0))) expanded-buffer)))

(provide 'wat-mode)
(load "/home/devon/repos/wat-mode/demo.el")

