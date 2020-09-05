;;;; cl-fourier-animation.asd

(asdf:defsystem #:cl-fourier-animation
  :description "Common Lisp implementation of a circle animation of
  inverse fourier Transform inspired by 3b1b's Youtube Videos."
  :author "Orm Finnendahl <orm-finnendahl@selmahfmdk-frankfurt.de>"
  :license  "GPL 2.0 or later"
  :version "0.0.1"
  :serial t
  :depends-on (#:lispbuilder-sdl
;;;               #:cl-opengl
               :cl-ppcre
               :bordeaux-fft
               :sb-cga
               :magicl)
  :components ((:file "package")
               (:file "utils")
               (:file "path-convert")
               (:file "fft-calculation")
               (:file "data")
               (:file "cl-fourier-animation")))
