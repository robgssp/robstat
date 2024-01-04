(asdf:defsystem robstat
  :description "i3 Status Bar"
  :version "0.1"
  :author "Rob Glossop <robgssp@gmail.com>"
  :license "MIT"
  :depends-on (:yason :local-time :alexandria :trivia :dbus :cl-autowrap/libffi)
  :serial t
  :components
  ((:file "package")
   (:file "autowrap")
   (:file "main")))
