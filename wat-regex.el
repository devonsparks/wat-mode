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

(setq wat-mem-instr-regex
      (rx
       (or
	"memory.size"
	"memory.grow"
	"align="
	"offset="
	(and     
	 (or
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
	 (zero-or-more space)))))

(setq wat-num-instr-regex
      (rx
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
		  (and "reinterpret" "/" (or "i32" "i64")))))))
		  

(setq wat-folded-instr-regex
      (rx
       (or "block"
	   "if"
	   "then"
	   "else"
	   "end"
	   "loop")))


(setq wat-control-instr-regex
      (rx
       (or "unreachable"
	   "nop"
	   "br"
	   "br_if"
	   "br_table"
	   "return"
	   "call_indirect"
	   "call")))

(setq wat-var-instr-regex
      (rx
	(and (or "tee" (and (or "g" "s") "et")) "_" (or "global" "local"))))

(setq wat-par-instr-regex
      (rx
       (or "drop" "select")))

(setq wat-ident-regex
      "$[0-9a-zA-Z!#$%'*+-./:<=>\?@^_`|~]+")

(setq wat-func-type-regex
      (rx
       (or "func" "param" "result")))

(setq wat-table-type-regex
      (rx "anyfunc"))

(setq wat-global-type-regex
      (rx (and "mut" space)))

(setq wat-keyword-regex
      (rx
       (or
	(and "type" space)
	(and "func" space)
	(and "table" space)
	(and "memory" space)
	(and "global" space)
	(and "local" space)
	(and "import" space)
	(and "export" space)
	(and "start" space)
	(and "offset" space)
	(and "elem" space)
	(and "data" space)
	(and "module"))))

(provide 'wat-regex)

;; wat-regex.el ends here
