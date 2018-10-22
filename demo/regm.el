	   	   
(@ define-register (name initial-value)
  (global ,name (mut i32) (i32.const ,initial-value)))

(@ op-reg (reg value op)
  (set_global ,reg (,op (get_global ,reg) (i32.const ,value))))

(@ reg.add (reg value)
  (op-reg ,reg ,value i32.add))

(@ reg.sub (reg value)
  (op-reg ,reg ,value i32.sub))

