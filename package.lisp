(defpackage robstat.c
  (:export :getloadavg :statvfs))

(defpackage robstat
  (:use :common-lisp :alexandria-2 :trivia :plus-c :robstat.c)
  (:export :main))
