(asdf:defsystem hunch
  :licence "MIT"
  :version "1.0.0"
  :author "Your NameAlex Fokin <alex_fff@mail.ru>"
  :depends-on (:hunchentoot
	       :cl-who
	       :parenscript)
  :components ((:file "hunch.lisp")
	       (:file "ank.lisp")
	       (:file "utils.lisp")
	       (:file "template.lisp"))
  :description "Web-app sor tests.")
