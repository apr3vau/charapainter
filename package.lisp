;; Copyright (c) 2024, April & May

(defpackage charapainter
  (:use :editor :anaphora)
  (:add-use-defaults)
  (:import-from alexandria
   read-file-into-byte-vector
   standard-deviation
   hash-table-plist
   plist-hash-table
   clamp
   lerp
   read-file-into-string
   deletef)
  (:import-from serapeum
   op
   make
   assocdr
   with-collector
   string-case
   pad-end
   string-join
   do-hash-table
   class-name-of)
  ;; Proclaim optimization
  (:import-from cl-user *optimization*))

(in-package charapainter)