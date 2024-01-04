#!/usr/bin/env -S sbcl --script

(require :asdf)
(asdf:load-asd (merge-pathnames "robstat.asd" (uiop:getcwd)))
(asdf:load-system :robstat)
(robstat:main)
