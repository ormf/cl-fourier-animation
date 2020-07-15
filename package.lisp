;;;; package.lisp

(defpackage #:cl-fourier-animation
  (:use #:cl #:bordeaux-fft)
  (:export
   #:main
   #:set-shape
   #:*violinschluessel-512*
   #:*achtel-512*
   #:*hessen-512*))
