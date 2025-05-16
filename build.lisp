(in-package cl-user)

(load-all-patches)

(require "asdf")

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(set-default-character-element-type 'character)
(pushnew :utf-8 system:*specific-valid-file-encodings*)

(defparameter *optimization* '(optimize (speed 3) (safety 0) (float 0)))

(asdf:load-system :charapainter)

(let* ((version (asdf:system-version (asdf:find-system :charapainter)))
       (src-dir (asdf:system-source-directory :charapainter))
       (build-dir (or (when-let (product-dir (environment-variable "BUILT_PRODUCTS_DIR"))
                        (truename product-dir))
                      (merge-pathnames "./build/" src-dir))))
  (deliver #'charapainter:main
           #+mswindows (merge-pathnames "Charapainter.exe" build-dir)
           #+darwin (create-macos-application-bundle
                     (merge-pathnames "Charapainter.app" build-dir)
                     :template-bundle (merge-pathnames "template.app/" src-dir)
                     :bundle-name "Charapainter"
                     :version-string version
                     :version "0.2.7"
                     :identifier "org.sdf.apr.charapainter"
                     :application-icns (merge-pathnames "res/cp.icns" src-dir))
           2
           :interface :capi
           :console :input
           ;:keep-fasl-dump t
           :editor-style :emacs
           :icon-file (merge-pathnames "res/cp.ico" src-dir)
           :startup-bitmap-file nil))
