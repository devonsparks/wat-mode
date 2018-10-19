.POSIX:

.PHONY: default
default: all

.PHONY: check
check: wat-mode-test.el
	emacs -batch -l ert -l $< -f ert-run-tests-batch-and-exit
