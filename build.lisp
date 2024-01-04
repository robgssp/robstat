#!/usr/bin/env -S sbcl --script

(require :asdf)
(asdf:load-asd (merge-pathnames "robstat.asd" (uiop:getcwd)))
(asdf:load-system :robstat)
;; (sb-ext:save-lisp-and-die "robstat" :executable t :toplevel #'robstat:main)
(asdf:operate :program-op "robstat")

