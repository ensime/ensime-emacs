#!/bin/bash

cask emacs --batch -q --directory=. --load test/ensime-emacs-test.el --funcall ert-run-tests-batch
