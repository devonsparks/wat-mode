.POSIX:

.PHONY: check
check: wat-mode-test.el
	emacs -batch -L . -l ert -l $< -f ert-run-tests-batch-and-exit
