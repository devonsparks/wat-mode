;;; wat-mode.el --- A major mode for WebAssembly -*- lexical-binding: t; -*-

;;  Copyright (C) 2018, Devon Sparks
;;
;;  Author: Devon Sparks <devon.sparks@gmail.com>
;;  URL: https://github.com/devonsparks/wat-mode
;;  Version: 1.0
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

(require 'wat-mode-font-lock)
(require 'wat-mode-macro)



(defvaralias 'wat-mode-indent-offset 'lisp-indent-offset
	"Make `wat-mode-indent-offset' alias the underlying lisp indentation behavior.")

(defvar wat-mode-syntax-table
  (let ((table (make-syntax-table lisp-mode-syntax-table)))
    ;; update identifier character class
    ;; to support word jumps
    (mapc #'(lambda (c)
	      (modify-syntax-entry c "w" table))
	  '(?! ?# ?$ ?% ?\' ?* ?+ ?- ?. ?\/ ?:
	    ?< ?= ?> ?\\ ?? ?@ ?^ ?_ ?\` ?| ?~))

    ;; enable wat block comments
    (modify-syntax-entry ?\(  "()1nb" table)
    (modify-syntax-entry ?\)  ")(4nb" table)
    (modify-syntax-entry ?\;  "< 123" table)
    (modify-syntax-entry ?\n ">b" table)
    table)
  "Syntax table for `wat-mode'.")



(defconst wat-mode-font-lock-keywords-1
  (list
   (cons wat-mode-font-lock-keyword-regex        'font-lock-keyword-face))
  "`wat-mode' highlight level 1 oof 4.
  
    Highlights core wat type keywords only.")


(defconst wat-mode-font-lock-keywords-2
  (append
   wat-mode-font-lock-keywords-1
   (list
    (cons wat-mode-font-lock-control-instr-regex  'font-lock-builtin-face)
    (cons wat-mode-font-lock-folded-instr-regex   'font-lock-builtin-face)
    (cons wat-mode-font-lock-var-instr-regex      'font-lock-builtin-face)
    (cons wat-mode-font-lock-par-instr-regex      'font-lock-builtin-face)
    (cons wat-mode-font-lock-table-type-regex     'font-lock-type-face)
    (cons wat-mode-font-lock-func-type-regex      'font-lock-type-face)
    (cons wat-mode-font-lock-global-type-regex    'font-lock-type-face)
    (cons wat-mode-font-lock-val-type-regex       'font-lock-type-face)))
   "`wat-mode' highlighting level 2 of 4.
     
     Highlights all core keywords minus numerical and memory instructions.")


(defconst wat-mode-font-lock-keywords-3
  (append
   wat-mode-font-lock-keywords-2
   (list
    (cons wat-mode-font-lock-ident-regex          'font-lock-variable-name-face)
    (cons wat-mode-font-lock-mem-instr-regex      'font-lock-builtin-face)
    (cons wat-mode-font-lock-num-instr-regex      'font-lock-builtin-face)))
  "`wat-mode' highlighting level 3 of 4.
   
    Highlights all core keywords.")


(defconst wat-mode-font-lock-keywords-4
  (append
   wat-mode-font-lock-keywords-3
   (list
    (cons wat-mode-font-lock-wast-regex            'font-lock-warning-face)))
  "`wat-mode' highlighting level 4 of 4.

   Highlights all core keywords plus .wast extensions.")


(defcustom wat-mode-font-lock-keywords 'wat-mode-font-lock-keywords-4
  "Active font lock level for `wat-mode'.")


(defvar wat-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map (kbd "C-c 1")   'wat-mode-macro-expand)
    (define-key map (kbd "RET")     'newline-and-indent)
    map)
  "Keymap for `wat-mode', derived from `lisp-mode'.")


(define-derived-mode wat-mode lisp-mode "wat-mode"
  "Major mode for editing WebAssembly's text encoding."
  (use-local-map wat-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(wat-mode-font-lock-keywords))
  (set-syntax-table wat-mode-syntax-table)
  "`wat-mode', an Emacs major mode for editing WebAssembly's text format")


(add-to-list 'auto-mode-alist '("\\.wat\\'" . wat-mode))
(add-to-list 'auto-mode-alist '("\\.wast\\'" . wat-mode))

(provide 'wat-mode)

;;; wat-mode.el ends here
