(defsystem charapainter
  :author "April & May"
  :version "0.2.1"
  :depends-on (alexandria anaphora serapeum)
  :components ((:file "fli-templates")
               (:file "package")
               (:file "util")
               (:file "pixel")
               (:file "defvars")
               (:file "tools")
               (:file "board")
               (:file "file")
               (:file "interface")))
