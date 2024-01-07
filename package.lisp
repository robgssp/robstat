(defpackage robstat.c
  (:export :getloadavg :statvfs))

(defpackage robstat
  (:use :common-lisp :alexandria-2 :trivia :plus-c :robstat.c)
  (:export :main :status :wifi :vpn :battery :disk-free :system-load
           :volume))

(defpackage robstat-user
  (:use :common-lisp :cl-user :alexandria-2 :trivia :robstat))
