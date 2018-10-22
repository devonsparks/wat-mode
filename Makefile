.POSIX:

ROOT=.

.PHONY: check
check: wat-mode-test.el
	emacs -batch -L . -l ert -l $< -f ert-run-tests-batch-and-exit

.PHONY: demo
demo:
	emacs -batch -L . -l ~/.emacs -l $(ROOT)/demo/regm.el $(ROOT)/demo/reg.wat -f wat-mode-macro-expand -f save-buffer
