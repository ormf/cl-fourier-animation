;;; 
;;; fft-calculation.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2020 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :cl-fourier-animation)

(defun vector->complex (pt)
  (complex (float (elt pt 0) 1.0d0) (float (elt pt 1) 1.0d0)))

;;; convert the points of a transformed svg-path into an array of complex numbers:

(defparameter *src*
  (let ((coords *hessen-512*))
    (coerce
     (make-array (length coords)
                 :initial-contents (mapcar #'vector->complex
                                           (scale-rotate coords 800 (complex 1000 -400))))
     'bordeaux-fft:complex-sample-array)))

(defparameter *src*
  (let ((coords *achtel-512*))
    (coerce
     (make-array (length coords)
                 :initial-contents (mapcar #'vector->complex
                                           (scale-rotate coords 1500 (complex 2100 -1200))))
     'bordeaux-fft:complex-sample-array)))

;; ; (scale-rotate *achtel-512* 1500 (complex 2100 -1200))

(defun normalize-fft (fft)
  (let ((size (length fft)))
    (dotimes (i size)
      (setf (aref fft i) (/ (aref fft i) size))))
  (values fft))

;;; calc the normalized fft

(defparameter *fft* (normalize-fft (bordeaux-fft:fft *src*)))
(defparameter *freq-idx-transform* (get-transform-fn (length *fft*)))
(setf *fft-size* (length *fft*))

(defparameter *fft-idx-sorted*
  (coerce (mapcar #'first
                  (sort (loop for i from 0 for x across *fft* collect (list i x))
                        #'>
                        :key (lambda (x) (abs (second x)))))
          'vector))

(defparameter *fft-idx-sorted-reverse*
  (coerce
   (cons 0 (coerce (reverse (subseq *fft-idx-sorted* 1 nil)) 'list)) 'vector))

#|
(loop
  for n below 512
  for angle = (/ (* n pi 2) 512)
  collect (loop
            for i below 512
            for result = (* (aref *fft* i) (exp (* angle (if (>  i 256) (- i 512) i)
                                                   (complex 0 1))))
            summing result))


(loop
  for n below 512
  for angle = (/ (* n pi 2) 512)
  collect (loop
            for i below 512
            for cx = (aref *fft* i)
            for real = (realpart cx)
            for imag = (imagpart cx)
            for result = (first (scale-rotate (list (vector real imag))
                                              1 (* angle (if (>  i 256) (- i 512) i)) #(0 0) :round nil))
              then (let ((r (first (scale-rotate (list (vector real imag))
                                                 1 (* angle (if (>  i 256) (- i 512) i)) #(0 0) :round nil))))
                     
                     
                     (vector (+ (aref r 0) (aref result 0))
                             (+ (aref r 1) (aref result 1))))
            finally (return result)))

(scale-rotate (list #(0.7 0.7)) 1 (* pi 2 14/512) #(0 0))

(* (complex 0.7 0.7) (exp (* pi 2 14/512 (complex 0 1))))

#C(0.5700210178326478d0 0.8093676580210204d0)




(+ #(1 2) #(3 4))

|#
