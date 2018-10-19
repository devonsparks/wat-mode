;;; wat-mode-test-font-lock.el --- ert tests of `wat-mode' font locks -*- lexical-binding: t; -*-

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

(require 'ert)
(require 'wat-mode-font-lock)

(ert-deftest wat-mode-test-font-lock-match-mem-instr-yes-test ()
  (let ((regex wat-mode-font-lock-mem-instr-regex))
    (mapcar (lambda (str)
	      (should (string-match regex str)))
	    '("i32.load"
	      "i64.load"
	      "f32.load"
	      "f64.load"
	      "i32.load8_s"
	      "i32.load8_u"
	      "i32.load16_s"
	      "i32.load16_u"
	      "i64.load8_s"
	      "i64.load8_u"
	      "i64.load16_s"
	      "i64.load16_u"
	      "i64.load32_s"
	      "i64.load32_u"
	      "i32.store"
	      "i64.store"
	      "f32.store"
	      "f64.store"
	      "i32.store8"
	      "i32.store16"
	      "i64.store8"
	      "i64.store16"
	      "i64.store32"
	      "offset="
	      "align="
	      "memory.grow"
	      "memory.size")))
  "Test of supported memory instructions in core.")


(ert-deftest wat-mode-test-font-lock-match-mem-instr-no-test ()
  (let ((regex wat-mode-font-lock-mem-instr-regex))
    (mapcar (lambda (str)
	      (should-not (string-match regex str)))
	    '("i32.load32_u "
	      "i32.load32_s "
	      "f32.store8 "
	      "f64.store8 "
	      "f32.store16 "
	      "f64.store16 "
	      "f64.store32 "
	      "align"
	      "grow")))
  "Sample of malformed memory instructions in core.")


(ert-deftest wat-mode-test-font-lock-match-ident-yes-test ()
  (let ((regex wat-mode-font-lock-ident-regex))
    (mapcar (lambda (str)
	      (should (string-match regex str)))
	    '("$NEXT"
	      "$9ds"
	      "$<=>")))
  "Sample of supported indentifiers in core.")


(ert-deftest wat-mode-test-font-lock-match-ident-no-test ()
  (let ((regex wat-mode-font-lock-ident-regex))
    (mapcar (lambda (str)
	      (should-not (string-match regex str)))
	    '("_bad"
	      "maL"
	      "$")))
  "Sample of unsupported identifiers in core.")


(ert-deftest wat-mode-test-font-lock-match-num-yes-test ()
  (let ((regex wat-mode-font-lock-num-instr-regex))
    (mapcar (lambda (str)
	      (should (string-match regex str)))
	    '("i32.const"
	      "i64.const"
	      "f32.const"
	      "f64.const"
	      "i32.clz"
	      "i32.ctz"
	      "i32.popcnt"
	      "i32.add"
	      "i32.sub"
	      "i32.mul"
	      "i32.div_s"
	      "i32.div_u"
	      "i32.rem_s"
	      "i32.rem_u"
	      "i32.and"
	      "i32.or"
	      "i32.xor"
	      "i32.shl"
	      "i32.shr_s"
	      "i32.shr_u"
	      "i32.rotl"
	      "i32.rotr"
	      "i64.clz"
	      "i64.ctz"
	      "i64.popcnt"
	      "i64.add"
	      "i64.sub"
	      "i64.mul"
	      "i64.div_s"
	      "i64.div_u"
	      "i64.rem_s"
	      "i64.rem_u"
	      "i64.and"
	      "i64.or"
	      "i64.xor"
	      "i64.shl"
	      "i64.shr_s"
	      "i64.shr_u"
	      "i64.rotl"
	      "i64.rotr"
	      "f32.abs"
	      "f32.neg"
	      "f32.ceil"
	      "f32.floor"
	      "f32.trunc"
	      "f32.nearest"
	      "f32.sqrt"
	      "f32.add"
	      "f32.sub"
	      "f32.mul"
	      "f32.div"
	      "f32.min"
	      "f32.max"
	      "f32.copysign"
	      "f64.abs"
	      "f64.neg"
	      "f64.ceil"
	      "f64.floor"
	      "f64.trunc"
	      "f64.nearest"
	      "f64.sqrt"
	      "f64.add"
	      "f64.sub"
	      "f64.mul"
	      "f64.div"
	      "f64.min"
	      "f64.max"
	      "f64.copysign"
	      "i32.eqz"
	      "i32.eq"
	      "i32.ne"
	      "i32.lt_s"
	      "i32.lt_u"
	      "i32.gt_s"
	      "i32.gt_u"
	      "i32.le_s"
	      "i32.le_u"
	      "i32.ge_s"
	      "i32.ge_u"
	      "i64.eqz"
	      "i64.eq"
	      "i64.ne"
	      "i64.lt_s"
	      "i64.lt_u"
	      "i64.gt_s"
	      "i64.gt_u"
	      "i64.le_s"
	      "i64.le_u"
	      "i64.ge_s"
	      "i64.ge_u"
	      "f32.eq"
	      "f32.ne"
	      "f32.lt"
	      "f32.gt"
	      "f32.le"
	      "f32.ge"
	      "f64.eq"
	      "f64.ne"
	      "f64.lt"
	      "f64.gt"
	      "f64.le"
	      "f64.ge"

	      "i32.wrap/i64"
	      "i32.wrap_i64"
	      "i32.trunc_s/f32"
	      "i32.trunc_f32_s"
	      "i32.trunc_u/f32"
	      "i32.trunc_f32_u"
	      "i32.trunc_s/f64"
	      "i32.trunc_f64_s"
	      "i32.trunc_u/f64"
	      "i32.trunc_f64_u"
	      "i64.extend_s/i32"
	      "i64.extend_i32_s"
	      "i64.extend_u/i32"
	      "i64.extend_i32_u"
	      "i64.trunc_s/f32"
	      "i64.trunc_f32_s"
	      "i64.trunc_u/f32"
	      "i64.trunc_f32_u"
	      "i64.trunc_s/f64"
	      "i64.trunc_f64_s"
	      "i64.trunc_u/f64"
	      "i64.trunc_f64_u"
	      "f32.convert_s/i32"
	      "f32.convert_i32_s"
	      "f32.convert_u/i32"
	      "f32.convert_i32_u"
	      "f32.convert_s/i64"
	      "f32.convert_i64_s"
	      "f32.convert_u/i64"
	      "f32.convert_i64_u"
	      "f32.demote/f64"
	      "f32.demote_f64"
	      "f64.convert_s/i32"
	      "f64.convert_i32_s"
	      "f64.convert_u/i32"
	      "f64.convert_i32_u"
	      "f64.convert_s/i64"
	      "f64.convert_i64_s"
	      "f64.convert_u/i64"
	      "f64.convert_i64_u"
	      "f64.promote/f32"
	      "f64.promote_f32"
	      "i32.reinterpret/f32"
	      "i32.reinterpret_f32"
	      "i64.reinterpret/f64"
	      "i64.reinterpret_f64"
	      "f32.reinterpret/i32"
	      "f32.reinterpret_i32"
	      "f64.reinterpret/i64"
	      "f64.reinterpret_i64"

	      ;; saturating float-to-int
	      "i32.trunc_s:sat/f32"
	      "i32.trunc_sat_f32_s"
	      "i32.trunc_u:sat/f32"
	      "i32.trunc_sat_f32_u"
	      "i32.trunc_s:sat/f64"
	      "i32.trunc_sat_f64_s"
	      "i32.trunc_u:sat/f64"
	      "i32.trunc_sat_f64_u"
	      "i64.trunc_s:sat/f32"
	      "i64.trunc_sat_f32_s"
	      "i64.trunc_u:sat/f32"
	      "i64.trunc_sat_f32_u"
	      "i64.trunc_s:sat/f64"
	      "i64.trunc_sat_f64_s"
	      "i64.trunc_u:sat/f64"
	      "i64.trunc_sat_f64_u"

	      ;; simd instructions
	      "i32x4.trunc_s/f32x4:sat"
	      "i32x4.trunc_sat_f32x4_s"
	      "i32x4.trunc_u/f32x4:sat"
	      "i32x4.trunc_sat_f32x4_u"
	      "i64x2.trunc_s/f64x2:sat"
	      "i64x2.trunc_sat_f64x2_s"
	      "i64x2.trunc_u/f64x2:sat"
	      "i64x2.trunc_sat_f64x2_u"

	      "f32x4.convert_s/i32x4"
	      "f32x4.convert_i32x4_s"
	      "f32x4.convert_u/i32x4"
	      "f32x4.convert_i32x4_u"
	      "f64x2.convert_s/i64x2"
	      "f64x2.convert_i64x2_s"
	      "f64x2.convert_u/i64x2"
	      "f64x2.convert_i64x2_u"

	      ;; atomic instructions
	      "i32.atomic.rmw8_u.add"
	      "i32.atomic.rmw8.add_u"
	      "i32.atomic.rmw16_u.add"
	      "i32.atomic.rmw16.add_u"
	      "i64.atomic.rmw8_u.add"
	      "i64.atomic.rmw8.add_u"
	      "i64.atomic.rmw16_u.add"
	      "i64.atomic.rmw16.add_u"
	      "i64.atomic.rmw32_u.add"
	      "i64.atomic.rmw32.add_u"
	      "i32.atomic.rmw8_u.sub"
	      "i32.atomic.rmw8.sub_u"
	      "i32.atomic.rmw16_u.sub"
	      "i32.atomic.rmw16.sub_u"
	      "i64.atomic.rmw8_u.sub"
	      "i64.atomic.rmw8.sub_u"
	      "i64.atomic.rmw16_u.sub"
	      "i64.atomic.rmw16.sub_u"
	      "i64.atomic.rmw32_u.sub"
	      "i64.atomic.rmw32.sub_u"
	      "i32.atomic.rmw8_u.and"
	      "i32.atomic.rmw8.and_u"
	      "i32.atomic.rmw16_u.and"
	      "i32.atomic.rmw16.and_u"
	      "i64.atomic.rmw8_u.and"
	      "i64.atomic.rmw8.and_u"
	      "i64.atomic.rmw16_u.and"
	      "i64.atomic.rmw16.and_u"
	      "i64.atomic.rmw32_u.and"
	      "i64.atomic.rmw32.and_u"
	      "i32.atomic.rmw8_u.or"
	      "i32.atomic.rmw8.or_u"
	      "i32.atomic.rmw16_u.or"
	      "i32.atomic.rmw16.or_u"
	      "i64.atomic.rmw8_u.or"
	      "i64.atomic.rmw8.or_u"
	      "i64.atomic.rmw16_u.or"
	      "i64.atomic.rmw16.or_u"
	      "i64.atomic.rmw32_u.or"
	      "i64.atomic.rmw32.or_u"
	      "i32.atomic.rmw8_u.xor"
	      "i32.atomic.rmw8.xor_u"
	      "i32.atomic.rmw16_u.xor"
	      "i32.atomic.rmw16.xor_u"
	      "i64.atomic.rmw8_u.xor"
	      "i64.atomic.rmw8.xor_u"
	      "i64.atomic.rmw16_u.xor"
	      "i64.atomic.rmw16.xor_u"
	      "i64.atomic.rmw32_u.xor"
	      "i64.atomic.rmw32.xor_u"
	      "i32.atomic.rmw8_u.xchg"
	      "i32.atomic.rmw8.xchg_u"
	      "i32.atomic.rmw16_u.xchg"
	      "i32.atomic.rmw16.xchg_u"
	      "i64.atomic.rmw8_u.xchg"
	      "i64.atomic.rmw8.xchg_u"
	      "i64.atomic.rmw16_u.xchg"
	      "i64.atomic.rmw16.xchg_u"
	      "i64.atomic.rmw32_u.xchg"
	      "i64.atomic.rmw32.xchg_u"
	      "i32.atomic.rmw8_u.cmpxchg"
	      "i32.atomic.rmw8.cmpxchg_u"
	      "i32.atomic.rmw16_u.cmpxchg"
	      "i32.atomic.rmw16.cmpxchg_u"
	      "i64.atomic.rmw8_u.cmpxchg"
	      "i64.atomic.rmw8.cmpxchg_u"
	      "i64.atomic.rmw16_u.cmpxchg"
	      "i64.atomic.rmw16.cmpxchg_u"
	      "i64.atomic.rmw32_u.cmpxchg"
	      "i64.atomic.rmw32.cmpxchg_u"
	      )))
  "Sample of supported numerical instructions, including those
   for SIMD and threading extensions.")


(ert-deftest wat-mode-test-font-lock-match-num-no-test ()
  (let ((regex wat-mode-font-lock-num-instr-regex))
    (mapcar (lambda (str)
	      (should-not (string-match regex str)))
	    '("i32x4.trunc_s/f64x4"
	      "i32x4.trunc_s/f32x4"
	      "i64x2.trunc_s/f32x4"
	      "i64x4.trunc_s/f32x4"
	      "f32x4.convert_s/i64x2"
	      "f32x4.convert_s/i64x4"
	      "f64x2.convert_s/i32x2"
	      "f64x2.convert_s/i32x4"
	      "f32x4.convert__i64x2_s"
	      "f32x4.convert_i64x4_s"
	      "f64x2.convert_i32x2_s"
	      "f64x2.convert_i32x4_s"

	      "i32.atomic.rmw32_u.add"
	      "i32.atomic.rmw32.add_u"
	      "i32.atomic.rmw32_s.add"
	      "i32.atomic.rmw32.add_s"
	      "i64.rmw16_u.add"
	      "i64.rmw16.add_u"
	      )))
  "Sample of unsupported numerical instructions, including those
   for SIMD and threading extensions.")

(ert-deftest wat-mode-test-font-lock-match-folded-instr-yes-test ()
  (let ((regex wat-mode-font-lock-folded-instr-regex))
    (mapcar (lambda (str)
	      (should (string-match regex str)))
	    '("block"
	      "if"
	      "then"
	      "else"
	      "end"
	      "loop")))
  "Supported folded instructions in core.")


(ert-deftest wat-mode-test-font-lock-match-control-instr-yes-test ()
  (let ((regex wat-mode-font-lock-control-instr-regex))
    (mapcar (lambda (str)
	      (should (string-match regex str)))
	    '("unreachable"
	      "nop"
	      "br"
	      "br_if"
	      "br_table"
	      "return"
	      "call_indirect"
	      "call")))
  "Supported control/branching instructions in core.")


(ert-deftest wat-mode-test-font-lock-match-var-instr-yes-test ()
  (let ((regex wat-mode-font-lock-var-instr-regex))
    (mapcar (lambda (str)
	      (should (string-match regex str)))
    '("get_local"
      "set_local"
      "tee_local"
      "get_global"
      "set_local"
      
      ;; spec#884
      "local.get"
      "local.set"
      "global.get"
      "global.set"
      )))
  "Supported variable assignment instructions, including
  revised names based on WebAssembly/spec#884.")


(ert-deftest wat-mode-test-font-lock-match-par-instr-yes-test ()
  (let ((regex wat-mode-font-lock-par-instr-regex))
    (mapcar (lambda (str)
	      (should (string-match regex str)))
	    '("drop" "select")))
  "Supported parametric instructions in core.")


(ert-deftest wat-mode-test-font-lock-match-func-type-yes-test ()
  (let ((regex wat-mode-font-lock-func-type-regex))
    (mapcar (lambda (str)
	      (should (string-match regex str)))
	    '("param"
	      "result")))
  "Supported function types in core.")


(ert-deftest wat-mode-test-font-lock-match-keyword-yes-test ()
  (let ((regex wat-mode-font-lock-keyword-regex))
    (mapcar (lambda (str)
	      (should (string-match regex str)))
	    '("(func)"
	      "(module)"
	      "(import $foo)"
	      "(export $foo)"
	      "(global $W (mut i32))")))
  "Sample of supported keyword instructions in core.")

(provide 'wat-mode-test-font-lock)

;;; wat-mode-test-font-lock.el ends here
