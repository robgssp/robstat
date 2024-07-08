(cl:in-package :robstat)

;;;; Dumb nix/c2ffi hacks

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun include-flags (flags)
    (remove-if-not
     #'uiop:directory-exists-p
     (cond ((member (first flags) '("-isystem" "-idirafter") :test #'equal)
            (cons (second flags)
                  (include-flags (cddr flags))))
           ((member (first flags) '("-B" "-resource-dir") :test #'equal)
            (cons (format nil "~a/include" (second flags))
                  (include-flags (cddr flags))))
           (flags (include-flags (cdr flags)))
           (t nil))))

  (defun nix-includes ()
    (let* ((cflags (uiop:getenv "NIX_CFLAGS_COMPILE"))
           (split (uiop:split-string cflags :separator " ")))
      (cons (format nil "~a/include" (uiop:getenv "LIBC"))
            (include-flags split))))

  (defun gen-compile-commands ()
    (uiop:with-temporary-file (:pathname path)
      (uiop:run-program (list "clang" "-c" "test.c" "-o" "test.o"
                              (format nil "-MJ~a" path))
                        :error-output t)
      (concatenate 'string "[" (uiop:read-file-string path) "]")))

  (defun includes-from-compile-commands ()
    (let* ((parsed (yason:parse (gen-compile-commands) :object-as :alist))
           (args (cdr (assoc "arguments" (first parsed) :test #'equal))))
      (include-flags args))))

;;;; Autowrap

(cffi:define-foreign-library libasound
  (t (:default "libasound")))

(cffi:use-foreign-library libasound)

(cl:in-package :robstat.c)

(autowrap:c-include
 "test.h"
 :sysincludes (cl:cons "." (robstat::includes-from-compile-commands))
 :trace-c2ffi cl:t
 :spec-path #p"./spec/"
 :exclude-definitions (".*")
 :include-definitions ("getloadavg" "size_t" "timeval" "timespec"
                                    "statvfs" "__fsblkcnt_t" "__fsfilcnt_t"
                                    ".*snd_mixer.*" ".*SND_.*"
                                    "sysconf" "_SC_.*"
                                    "pa_.*" "PA_.*"))
