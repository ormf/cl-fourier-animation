;;; 
;;; sdl-example.lisp
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

(defparameter *random-color* sdl:*white*)


(defparameter *mode* 0)
(defparameter *angle* 0)
(defparameter *offset* (vector 400 400))
(defparameter *max-num* 30)
(defparameter *angle-increment* -0.005)
(defparameter *shift* (floor (/ *max-num* -2)))
(defparameter *fft-size* 512)
(defparameter *cyclic-path* (make-array 1024 :element-type 'vector :initial-contents (loop for i below 1024 collect (vector 0 0))))
(defparameter *path-size* 0)
(defparameter *curr-path-idx* 0)

(defun paint ()
  (loop
    for idx below *path-size*
    for offs from *curr-path-idx*
    for pt1 = (aref *cyclic-path* (mod (+ idx offs) 1024))
    for pt2 = (aref *cyclic-path* (mod (+ 1 idx offs) 1024))
    for opacity = 255 then (max (- opacity 0.2) 0)
    do (sdl:draw-line
        pt1 pt2
        :color (sdl:color :r (round (* opacity 1.0)) :g (round (* opacity 1.0)) :b (round (* opacity 0.0)))
        :aa t)))

(defun draw-shape ()
  (loop
    for (pt1 pt2) on (scale-rotate *achtel-512* (complex 1500 0) (complex 2050 -750) :round t)
        with opacity = 80
        while pt2
        do (sdl:draw-line
            pt1 pt2
            :color (sdl:color :r (round opacity) :g (round opacity) :b (round opacity))
            :aa t)))
#|

(defun draw-shape ()
  (loop
    for (pt1 pt2) on (scale-rotate *hessen-512* (complex 800 0) (complex 1400 00)
                                   :round t)
        with opacity = 80
        while pt2
        do (sdl:draw-line
            pt1 pt2
            :color (sdl:color :r (round opacity) :g (round opacity) :b (round opacity))
            :aa t)))
|#

(declaim (inline draw-circled-arrow))
(defun draw-circled-arrow (cx offset)
  (sdl:draw-filled-polygon
   (scale-rotate *arrow-path* cx offset :round t)
   :color sdl:*white*)
  (sdl:draw-circle
   (complex->vector offset :round t)
   (round (abs cx))
   :color (sdl:color :r 140 :g 0 :b 140)))

(defun get-offset (mode)
  (case mode
    (1  (complex 0 0))
    (otherwise (complex -50)

(setf *shift* (* -1 (floor (/ *max-num* 2))))
;;; (setf *angle-increment* 0.01)
;;; (setf *angle-increment* -0)
;;; (setf *angle* 0)

(setf *mode* 3) 

;;; (main)

