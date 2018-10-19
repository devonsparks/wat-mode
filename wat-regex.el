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
	 (and
	  bow
	  (or
	   "memory.size"
	   "memory.grow"
	   "align="
	   "offset="
	   ;; floats
	   (and "f" (or "32" "64") "."(or "store" "load"))

	   ;; special case for 64-bit integer loads only
	   (and "i64" "." (or "load32_s" "load32_u" "store32"))

	     ;; integers
	   (and	      
	    "i" (or "32" "64") "."
	    (or
	     (and "store" (zero-or-one (or "8" "16")))
	     (and "load" (zero-or-one (and (or "8" "16") "_" (or "s" "u"))))))
	    eow)))))

(defvar wat-num-instr-regex
  (eval-when-compile
    (rx
     (and
      bow
      (or
	"f32.demote/f64"
	"f64.promote/f32"
	(and "i32" "." "wrap/i64")
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
		  (and "trunc" "_" (or "s" "u") "/" (or "f32" "f64"))
		  (and "extend" "_" (or "s" "u") "/" "i32")
		  (and "reinterpret" "/" (or "f32" "f64"))))
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
		  (and "convert" "_" (or "s" "u") "/" (or "i32" "i64"))
		  (and "reinterpret" "/" (or "i32" "i64")))))
      eow))))

		  

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
	(and
	 (or "tee" (and (or "g" "s") "et"))
	 "_"
	 (or "global" "local"))
	eow))))

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
  (eval-when-compile (rx (and bow "anyfunc" eow))))

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
     ;; (or
     ;;  (and  "type" eow)
     ;;  (and  "func" eow)
     ;;  (and  "table" eow)
     ;;  (and  "memory" eow)
     ;;  (and  "global" eow)
     ;;  (and  "local" eow)
     ;;  (and  "import" eow)
     ;;  (and  "export" eow)
     ;;  (and  "start" eow)
     ;;  (and  "offset" eow)
     ;;  (and  "elem" eow)
     ;;  (and  "data" eow)
     ;;  (and  "module" eow)))))
(provide 'wat-regex)

;; wat-regex.el ends here
