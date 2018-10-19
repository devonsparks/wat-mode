;;; wat-mode-font-lock.el --- font lock regexps for WebAssembly -*- lexical-binding: t; -*-

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

;;; Code:

(require 'rx)

(defconst wat-mode-font-lock-mem-instr-regex
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
	       eow))))
      "Supported memory instructions in core.")



(defconst wat-mode-font-lock-num-instr-regex
  (eval-when-compile
    (rx
     (and
      bow
      (or

	;; i32 only
       (and "i32"
	    (or
	     (and "x4" "." "trunc" "_"
		  (or
		   (and (or "s" "u") "/" "f32x4:sat")
		   (and "sat_f32x4" "_" (or "s" "u"))))
	     		     
	    (and "." (or
		      "wrap/i64"
		      "wrap_i64"  ;; spec#884 syntax
		      (and "atomic" "." "rmw"
			   (or "8" "16")
			   (or
			    (and "_" (or "s" "u") "."
				 (or "add" "sub" "and" "or" "xor" "xchg" "cmpxchg"))
			    (and "." (or "add" "sub" "and" "or" "xor" "xchg" "cmpxchg") "_" "u")))
		      (and "reinterpret"
			   (or "/f32"
			       "_f32")))))) ;;spec#884 syntax

       ;; i64 only
       (and "i64"
	    (or
	     (and "x2" "." "trunc" "_"
		  (or
		   (and (or "s" "u") "/" "f64x2:sat")
		   (and "sat_f64x2" "_" (or "s" "u"))))
	     (and "." (or
		       (and "atomic" "." "rmw"
			   (or "8" "16" "32")
			   (or
			    (and "_" (or "s" "u") "."
				 (or "add" "sub" "and" "or" "xor" "xchg" "cmpxchg"))
			    (and "." (or "add" "sub" "and" "or" "xor" "xchg" "cmpxchg") "_" "u")))
		       (and "extend" "_"
			    (or
			     (and (or "s" "u") "/" "i32")
			     (and "i32_" (or "s" "u")))) ;; spec#884 syntax
		       (and "reinterpret"
			    (or "/f64" "_f64"))))))

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
	(and "f32"
	     (or
	      (and "x4" "." "convert" "_"
		   (or
		    (and (or "s" "u") "/" "i32x4")
		    (and "i32x4" "_" (or "s" "u"))))
	      (and "." (or
			(and "demote"
			     (or "/" "_") ;; current + spec#884 syntax
			     "f64")
			(and "reinterpret"
			     (or "/" "_") ;; current + spec#884 syntax
			     "i32")))))

	;; f64 only
	(and "f64"
	     (or
	      (and "x2" "." "convert" "_"
		   (or
		    (and (or "s" "u") "/" "i64x2")
		    (and "i64x2" "_" (or "s" "u"))))
	      
	      (and "."
		   (or
		    (and "promote"
			 (or "/" "_") ;; current + spec#884 syntax
			 "f32")
		    (and "reinterpret"
			 (or "/" "_") ;; current + spec#884 syntax
			 "i64")))))
	     
	     ;; f32 and f64
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
			(and (or "i32" "i64") "_" (or "s" "u"))))))) ;; spec#884 syntax
      eow)))
  "Supported numerical instructions in core.
Includes new syntax per WebAssembly/spec#884, along with SIMD and threading extensions.")
  
      

		  

(defconst wat-mode-font-lock-folded-instr-regex
  (eval-when-compile
      (rx
       (and
	bow
	(or
	 "block"
	 "loop"
	 "if"
	 "then"
	 "else"
	 "end")
	eow)))
  "Supported folded instructions in core.")



(defconst wat-mode-font-lock-control-instr-regex
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
	eow)))
  "Supported control instructions in core.")



(defconst wat-mode-font-lock-var-instr-regex
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
	 eow))))
  "Supported variable instructions in core.
Includes revised syntax per WebAssembly/spec/#884.")



(defconst wat-mode-font-lock-par-instr-regex
  (eval-when-compile
    (rx
     (and (or "drop" "select") eow)))
  "Supported parametric instructures in core.")



(defconst wat-mode-font-lock-ident-regex
      "$[0-9a-zA-Z!#$%'*+-./:<=>\?@^_`|~]+")



(defconst wat-mode-font-lock-func-type-regex
  (eval-when-compile
    (rx (and bow (or "param" "result") eow)))
  "Supported function types in core.")



(defconst wat-mode-font-lock-table-type-regex
  (eval-when-compile
    (rx
     (and bow
	  (or
	   "anyfunc"
	   "funcref") ; spec#884
	  eow)))
  "Supported table type instructions in core.")


(defconst wat-mode-font-lock-global-type-regex
  (eval-when-compile (rx (and bow "mut" eow)))
  "Supported global type instructions in core.")



(defconst wat-mode-font-lock-val-type-regex
  (eval-when-compile
    (rx (and
	space
	 (or "i32" "i64" "f32" "f64")
	 eow)))
  "Supports value type instructions in core.")



(defconst wat-mode-font-lock-keyword-regex
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
      eow)))
  "Supported keywords in core.")



(defconst wat-mode-font-lock-wast-regex
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
      eow)))
  "Supported keywords in .wast extension.")



(provide 'wat-mode-font-lock)

;;; wat-mode-font-lock.el ends here
