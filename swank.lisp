(require 'asdf)
(asdf:load-system :swank)
(asdf:load-asd (merge-pathnames "robstat.asd" (uiop:getcwd)))
(swank:create-server :dont-close t)
