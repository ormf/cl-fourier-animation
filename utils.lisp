;;; 
;;; utils.lisp
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

(defconstant +i+ (complex 0 1))

(defun complex->vector (cx &key round)
  (if round
      (vector (round (realpart cx))
              (round (imagpart cx)))
      (vector (realpart cx)
              (imagpart cx))))

(defun vector->complex (vec &key round)
  (if round
      (complex (float (round (elt vec 0)) 1.0d0)
               (float (round (elt vec 1)) 1.0d0))
      (complex (float (elt vec 0) 1.0d0)
               (float (elt vec 1) 1.0d0))))

(defun make-path (&rest coords)
  (loop for (x y) on coords by #'cddr while y collect (complex x y)))

(defun get-transform-fn (size)
  "get function to transform the range 0..size into the range
0..size/2, (- size/2-1) .. -1. This is like interpreting an integer as
two's complement for a range given by size.

  example: (funcall (get-transform-fn 512) 511) -> -1
  example: (funcall (get-transform-fn 512) 510) -> -2
"
  (declare ((unsigned-byte 64) size))
  (let* ((power-of-two (round (1- (log size 2))))
         (split-point (ash 1 power-of-two))
         (max (ash 1 (1+ power-of-two))))
    (lambda (x) (if (> x split-point) (- x max) x))))

(defun round-pt (pt)
  (vector (round (elt pt 0))
          (round (elt pt 1))))

(defun scale-rotate (path cx offset &key round)
  "rotate, scale and offset a seq of 2D points. the seq is a list of complex numbers"
;;;  (break "scale-rotate: ~a" path)
  (map 'list (lambda (pt)
               (let ((res (+ offset (* pt cx))))
                 (if round
                     (vector (round (realpart res))
                             (round (imagpart res)))
                     (vector (realpart res)
                             (imagpart res)))))
       path))


;;; cyclic path for showing the current paint procedure with a fading line.



;;; (defun )


