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
;;; calc the normalized fft

(defun normalize-fft (fft)
  (let ((size (length fft)))
    (dotimes (i size)
      (setf (aref fft i) (/ (aref fft i) size))))
  (values fft))
