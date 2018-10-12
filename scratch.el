(defmacro inc (var)
`(setq ,var (1+ ,var)))

(defvar *wat-macro-tag* 'wat-macro)


(defmacro @ (name args &rest body)
  `(defmacro ,name ,(append args '(&rest body))
       (backquote (cons (quote ,*wat-macro-tag*)
			 (quote ,body)))))

(@ define-word (name)
  (func ,name (type $word)
	,@body))
	   	   
(@ define-register (name initial-value)
  (global ,name (mut i32) (i32.const ,initial-value)))

(@ op-reg (reg value op)
  (set_global ,reg (,op (get_global ,reg) ,value)))

(@ reg.add (reg value)
  (op-reg ,reg ,value i32.add)))

(@ reg.sub (reg value)
  (op-reg ,reg ,value i32.sub)))

(@ reg.load (regA regB)
  (set_global ,regA (get_global ,regB)))

(@ reg.lpush (reg)
 (get_global ,reg)
 (i32.load))



(@ $NEXT ()
   (reg.load $W $IP) ;; W <- (IP)
   (reg.add $IP CELL) ;; IP <- IP + 4
   (reg.lpush $W)
   (call_indirect)    ;; JMP (W)
   )

;;(define-word $ENTER
;;  (reg.add $RSP CELL) ;; RSP <- RSP + 4
;;  (reg.load $RSP $IP) ;; RSP < (IP)
;;  )



(defmacro module (&rest exp)
  `(cons 'module
	 (mapcar 'macroexpand-all (quote ,exp))))
			



