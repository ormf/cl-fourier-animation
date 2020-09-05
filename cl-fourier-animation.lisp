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
(defparameter *frame-rate* 60)
(defparameter *offset* (vector 400 400))
(defparameter *max-num* 30)
(defparameter *angle-increment* -0.005)
(defparameter *shift* (floor (/ *max-num* -2)))

(defparameter *cyclic-path* (make-array 2048 :element-type 'vector
                                             :initial-contents (loop for i below 2048 collect (vector 0 0))))
(defparameter *path-size* 0)
(defparameter *curr-path-idx* 1)
(defparameter *max-path-length* 512)
(defparameter *curr-shape* *violinschluessel-512*)

(defun paint ()
  "paint *path-size* elements of *cyclic-path* as lines, starting at
*curr-path-idx*. Reduce opacity with incrementing index."
  (loop
    for idx below (- *path-size* 2)
    with offs = *curr-path-idx*
    for pt1 = (aref *cyclic-path* (mod (+ idx offs) *max-path-length*))
    for pt2 = (aref *cyclic-path* (mod (+ 1 idx offs) *max-path-length*))
    for opacity = 255 then (max (- opacity 0.2) 0)
    do (sdl:draw-line
        pt1 pt2
        :color (sdl:color :r (round (* opacity 1.0)) :g (round (* opacity 1.0)) :b (round (* opacity 0.0)))
        :aa t)))

(defun set-shape (shape)
  (setf *path-size* 0)
  (setf *curr-shape* shape)
  (setf *angle* 0)
  nil)

(defun draw-shape ()
  (with-slots (coords scale offset) *curr-shape*
    (loop
      for (pt1 pt2) on (scale-rotate coords scale offset :round t)
      with opacity = 50
      while pt2
      do (sdl:draw-line
          pt1 pt2
          :color (sdl:color :r (round opacity) :g (round opacity) :b (round opacity))
          :aa t))))

(declaim (inline draw-circled-arrow))
(defun draw-circled-arrow (cx offset)
  (sdl:draw-filled-polygon
   (scale-rotate *arrow-path* cx offset :round t)
   :color sdl:*white*)
  (sdl:draw-circle
   (complex->vector offset :round t)
   (round (abs cx))
   :color (sdl:color :r 140 :g 0 :b 140)))

(defun set-speed (freq)
  (setf *angle-increment* (/ (* 2 pi freq) *frame-rate*))
  (setf *max-path-length* (min 2048 (round (/ (* 2 pi) *angle-increment*)))))

(defun get-offset (mode)
  (case mode
    (1  (complex 0 0))
    (otherwise (shape-offset *curr-shape*))))

(defun mirror-list (i maxnum)
  (cond ((zerop i) 0)
        ((<= i (/ maxnum 2))
         (1- (* i 2)))
        (:else (* (- maxnum i) 2))))

#|
(defun mirror-list2 (i maxnum)
  (cond ((zerop i) 0)
        ((oddp i) (1+ i))
        (:else (- (+ 1 maxnum) i))))
|#

(defun mirror-list2 (i maxnum)
  (cond ((zerop i) 0)
        ((oddp i) i)
        (:else (- maxnum i))))

(defun get-idx (i mode)
  (with-slots (size fft-idx-sorted) *curr-shape*
    (case mode
      (3 (elt fft-idx-sorted (mirror-list i *max-num*)))
      (2 (elt fft-idx-sorted i))
      (1 (mod (+ i *shift*) size))
      (otherwise (mirror-list2 i *max-num*)))))

(defun main ()
  (sdl:with-init ()
    (sdl:window 1600 900 :double-buffer t :title-caption "Move a rectangle using the mouse")
    (setf (sdl:frame-rate) *frame-rate*)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event ()
       (sdl:push-quit-event))
      (:idle ()
       ;; Change the color of the box if the left mouse button is depressed
              ;; Clear the display each game loop
       (sdl:clear-display sdl:*black*)
       ;; Draw the shape outline
;;       (draw-shape)
       (paint)
       (let ((offset (get-offset *mode*)))
         (with-slots (fft scale freq-idx-transform-fn) *curr-shape*
           (dotimes (i *max-num*)
             (let* ((x (get-idx i *mode*))
                    (curr (* (aref fft x) scale
                             (exp (* +i+ (funcall freq-idx-transform-fn x) *angle*)))))
               (unless (= x 0) (draw-circled-arrow curr offset))
               (case *mode*
                 (1 (setf offset (complex (+ 50 (* (mod x 9) 100))
                                          (+ 60 (* (mod (floor x 9) 9) 100)))))
                 (otherwise (incf offset curr))))))
         (incf *angle* *angle-increment*)
         (setf *curr-path-idx* (mod (1- *curr-path-idx*) *max-path-length*))
         (setf *path-size* (min (1+ *path-size*) *max-path-length*))
         (setf (aref *cyclic-path* *curr-path-idx*) (vector (round (realpart offset)) (round (imagpart offset)))))
       ;; Redraw the display
       (sdl:update-display)))))

;;; (setf *angle-increment* 0.01)
;;; (setf *angle* 0)

(set-speed 0.01)
(setf *mode* 3)
(setf *path-size* 0)
(setf *max-num* 512)
(set-shape *violinschluessel-512*)
;; (set-shape *achtel-512*)
;; (set-shape *hessen-512*)

;;; Start app:
;;;
;;; (main)


