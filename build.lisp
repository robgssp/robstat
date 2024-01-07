#!/usr/bin/env -S sbcl --script

(require :asdf)
(asdf:load-asd (merge-pathnames "robstat.asd" (uiop:getcwd)))
(asdf:load-system :robstat)
(sb-ext:save-lisp-and-die "robstat" :executable t :toplevel #'robstat:main)
;; (uiop:delete-file-if-exists "robstat")
;; (let* ((image
;;          (asdf:output-file
;;           (asdf:operate :program-op "robstat")
;;           "robstat"))
;;        (output (merge-pathnames "robstat" (uiop:getcwd))))
;;   (format t "Copying ~a to ~a~%" image output)
;;   (uiop:rename-file-overwriting-target image output))
