(module
 ;; "define-register"
	    (global $W
		    (mut i32)
		    (i32.const 0))
 (func $do-add
       ;; "reg.add"
		  ;; "op-reg"
			     (set_global $W
					 (i32.add
					  (get_global $W)
					  (i32.const 10)))))




 



