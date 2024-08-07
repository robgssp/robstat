(asdf:defsystem robstat
  :description "i3-compatible Status Bar"
  :version "0.1"
  :author "Rob Glossop <robgssp@gmail.com>"
  :license "MIT"
  :depends-on (:yason :local-time :alexandria :trivia :dbus :cl-autowrap :cl-plus-c :cl-ppcre :swank)
  :serial t
  :entry-point "robstat:main"
  :components
  ((:file "package")
   (:file "autowrap")
   (:file "main")))
