(defsystem charapainter
  :author "April & May"
  :version "0.1.0"
  :depends-on (alexandria anaphora cl-ppcre serapeum)
  :components ((:file "fli-templates")
               (:file "package")
               (:file "util")
               (:file "defvars")
               (:file "tools")
               (:file "board")
               (:file "file")
               (:file "interface")))
