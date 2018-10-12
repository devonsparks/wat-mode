
(defconst *wat-comment-token* ";;"
  "wat comment token - used for annotating macro expansions")

(defconst *wat-macro-tag* 'wat-macro
  "Prefix used to identify all macro expansions")

(defalias 'wat-printer 'pp
  "Procedure used to format expressions during macro expansion")

(defun wat-macro-tag (name)
  "Built a symbol tag of wat macro NAME in the current buffer"
  (intern (mapconcat 'identity
		     (list (symbol-name *wat-macro-tag*)
			   (symbol-name name)) ":")))


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
  "A macro replacement for wat's (macro top-level expression."
  `(cons 'module
	 (mapcar 'macroexpand-all
		 (quote ,exp))))

(defalias '@ 'wat-macro
  "@ extends wat syntax to support macro expansions.")

(defalias 'module 'wat-module
  "Bind macro expander to match wat's top-level syntax.")



