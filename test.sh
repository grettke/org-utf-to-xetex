#!/usr/bin/env bash

emacs -batch \
      -l ert \
      -l ~/src/org-utf-to-xetex/org-utf-to-xetex.el \
      -l ~/src/org-utf-to-xetex/org-utf-to-xetex-test.el \
      -f ert-run-tests-batch-and-exit
