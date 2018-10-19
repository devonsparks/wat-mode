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

(require 'rx)


(defvar wat-mem-instr-regex
      (eval-when-compile
	(rx
	 (or
	  (and bow (or "align=" "offset="))
	  
	  (and bow
	       (or
		"memory.size"
		"memory.grow"
		;; floats
		(and "f" (or "32" "64") "."(or "store" "load"))

		;; special case for 64-bit integer loads only
		(and "i64" "." (or "load32_s" "load32_u" "store32"))

		;; integers
		(and	      
		 "i" (or "32" "64") "."
		 (or
		  (and "store" (zero-or-one (or "8" "16")))
		  (and "load" (zero-or-one (and (or "8" "16") "_" (or "s" "u")))))))
	       eow)))))



(defvar wat-num-instr-regex
  (eval-when-compile
    (rx
     (and
      bow
      (or

	;; i32 only
	(and "i32" "."
	     (or
	      "wrap/i64" 
	      "wrap_i64"  ;; spec#884 syntax
	      (and "reinterpret"
		   (or "/f32"  
		       "_f32")))) ;;spec#884 syntax

	;; i64 only
	(and "i64" "."
	     (or
	      (and "extend" "_"
		  (or
		   (and (or "s" "u") "/" "i32") 
		   (and "i32_" (or "s" "u")))) ;; spec#884 syntax
	      (and "reinterpret"
		   (or "/f64" "_f64"))))

	;; i32 and i64
	(and (or "i32" "i64") "."
	     (or "const"
		  "clz"
		  "ctz"
		  "popcnt"
		  "add"
		  "sub"
		  "mul"
		  (and "div" "_" (or "s" "u"))
		  (and "rem" "_" (or "s" "u"))
		  "and"
		  "or"
		  "xor"
		  "shl"
		  (and "shr" "_" (or "s" "u"))
		  (and "rot" (or "l" "r"))
		  "eqz"
		  "eq"
		  "ne"
		  (and "lt" "_" (or "s" "u"))
		  (and "gt" "_" (or "s" "u"))
		  (and "le" "_" (or "s" "u"))
		  (and "ge" "_" (or "s" "u"))
		  (and "trunc" "_" (or
				    (and (or "s" "u") (optional ":sat")
					 "/" (or "f32" "f64"))   ; current syntax 
				    (and (optional "sat_")
					 (or "f32" "f64") "_" (or "s" "u")))))) ; spec/#884 syntax
	;; f32 only
	(and "f32" "."
	     (or
	      (and "demote"
		   (or "/" "_") ;; current + spec#884 syntax
		   "f64")
	      (and "reinterpret"
		   (or "/" "_") ;; current + spec#884 syntax
		       "i32")))

	;; f64 only
	(and "f64" "."
	     (or
	      (and "promote"
		   (or "/" "_") ;; current + spec#884 syntax
		   "f32")
	      (and "reinterpret"
		   (or "/" "_") ;; current + spec#884 syntax
		   "i64")))
	
	;; both f32 and f64
	(and (or "f32" "f64") "."
	     (or  "const"
		  "abs"
		  "neg"
		  "ceil"
		  "floor"
		  "trunc"
		  "nearest"
		  "sqrt"
		  "add"
		  "sub"
		  "mul"
		  "div"
		  "min"
		  "max"
		  "copysign"
		  "eq"
		  "ne"
		  "lt"
		  "gt"
		  "le"
		  "ge"
		  (and "convert" "_"
		       (or
			(and (or "s" "u") "/" (or "i32" "i64"))   ;; current syntax
			(and (or "i32" "i64") "_" (or "s" "u")))))) ;; spec#884 syntax
      eow)))))

		  

(defvar wat-folded-instr-regex
  (eval-when-compile
      (rx
       (and
	bow
	(or
	 "block"
	 "loop"
	 (and (not (any "_")) "if")
	 "then"
	 "else"
	 "end")
	eow))))



(defvar wat-control-instr-regex
  (eval-when-compile
      (rx
       (and
	bow
	(or
	 "unreachable"
	 "nop"
	 "br"
	 "br_if"
	 "br_table"
	 "return"
	 "call_indirect"
	 "call")
	eow))))

(defvar wat-var-instr-regex
  (eval-when-compile
      (rx
       (and
	bow
	(or
	 ;; current syntax
	 (and
	  (or "tee" (and (or "g" "s") "et"))
	  "_"
	  (or "global" "local"))
	 
	 ;; spec#884 
	 (and
	  (or "global" "local")
	  "."
	  (or "tee" (and (or "g" "s") "et")))
	eow)))))

(defvar wat-par-instr-regex
  (eval-when-compile
    (rx
       (and (or "drop" "select") eow))))

(defvar wat-ident-regex
      "$[0-9a-zA-Z!#$%'*+-./:<=>\?@^_`|~]+")

(defvar wat-func-type-regex
  (eval-when-compile
    (rx (and bow (or "param" "result") eow))))

(defvar wat-table-type-regex
  (eval-when-compile
    (rx
     (and bow
	  (or
	   "anyfunc"
	   "funcref")			; spec#884
	  eow))))

(defvar wat-global-type-regex
      (eval-when-compile (rx (and bow "mut" eow))))

(defvar wat-val-type-regex
  (eval-when-compile
    (rx (and
	space
	 (or "i32" "i64" "f32" "f64")
	 eow))))

(defvar wat-keyword-regex
  (eval-when-compile
    (rx
     (and
      bow
      (or
       "type"
       "func"
       "table"
       "memory"
       "global"
       "local"
       "import"
       "export"
       "start"
       "offset"
       "elem"
       "data"
       "module")
      eow))))

(defvar wat-wast-regex
  (eval-when-compile
    (rx
     (and
      bow
      (or
       "script:"
       "register"
       "module:"
       "binary"
       "quote"
       "action:"
       "invoke"
       "get"
       "assertion:"
       "assert_return"
       "assert_return_canonical_nan"
       "assert_return_arithmetic_nan"
       "assert_trap"
       "assert_malformed"
       "assert_invalid"
       "assert_unlinkable"
       "meta:"
       "input"
       "output")
      eow))))
  
(provide 'wat-regex)

;; wat-regex.el ends here
