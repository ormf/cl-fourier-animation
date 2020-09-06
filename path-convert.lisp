;;; 
;;; path-convert.lisp
;;;
;;; simple parser to convert a svg Path into a point sequence.
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

(defun delimiterp (char)
  (member char '(#\V #\H #\M #\L #\C #\S #\Z #\v #\h #\m #\l #\c #\s #\z)))

(defun my-split (string &key (delimiterp #'delimiterp))
  (loop for beg = (position-if-not delimiterp string)
          then (position-if-not delimiterp string :start (1+ end))
        for end = (and beg (position-if delimiterp string :start beg))
        when beg collect (subseq string (1- beg) end)
    while end))

(defparameter *x* 0)
(defparameter *y* 0)
;;; (ql:quickload "magicl")

;;; (use-package :magicl)

;;; Absolute path coords extraction functions:

(defun moveto-rel (&rest coords)
  "moveto is handled like lineto."
  (loop for (x y) on coords by #'cddr
        collect `((,*x* ,*y*) (,(incf *x* x) ,(incf *y* y)))))

(defun lineto-rel (&rest coords)
  (loop for (x y) on coords by #'cddr
        collect `((,*x* ,*y*) (,(incf *x* x) ,(incf *y* y)))))

(defun vertical-rel (&rest coords)
  (loop for y in coords
        collect `((,*x* ,*y*) (,*x* ,(incf *y* y)))))

(defun horizontal-rel (&rest coords)
  (loop for x in coords
        collect `((,*x* ,*y*) (,(incf *x* x) ,*y*))))

(defun curveto-rel (&rest coords)
  (loop for (x1 y1 x2 y2 x y) on coords by (lambda (x) (nthcdr 6 x))
        collect `((,*x* ,*y*)
                  (,(+ *x* x1) ,(+ *y* y1))
                  (,(+ *x* x2) ,(+ *y* y2))
                  (,(incf *x* x) ,(incf *y* y)))))

(defun qcurveto-rel (&rest coords)
  (loop for (x1 y1 x y) on coords by (lambda (x) (nthcdr 6 x))
        collect `((,*x* ,*y*)
                  (,(+ *x* x1) ,(+ *y* y1))
                  (,(incf *x* x) ,(incf *y* y)))))

;;; (decasteljau 1 '((2 3) (3 4) (1 5) (4 6)))

(defun smooth-curveto-rel (&rest coords)
  (loop for (x2 y2 x y) on coords by (lambda (x) (nthcdr 4 x))
        collect `(,(incf *x* x) ,(incf *y* y))))

(defun moveto-abs (&rest coords)
  (loop for (x y) on coords by #'cddr
        collect `(,(setf *x* x) ,(setf *y* y))))

(defun lineto-abs (&rest coords)
  (loop for (x y) on coords by #'cddr
        collect `(,(setf *x* x) ,(setf *y* y))))

(defun vertical-abs (&rest coords)
  (loop for y in coords
        collect `(,*x* ,(setf *y* y))))

(defun horizontal-abs (&rest coords)
  (loop for x in coords
        collect `(,(setf *x* x) ,*y*)))

(defun curveto-abs (&rest coords)
  (loop for (x1 y1 x2 y2 x y) on coords by (lambda (x) (nthcdr 6 x))
        collect `(,(setf *x* x) ,(setf *y* y))))

(defun smooth-curveto-abs (&rest coords)
  (loop for (x2 y2 x y) on coords by (lambda (x) (nthcdr 4 x))
        collect `(,(setf *x* x) ,(setf *y* y))))

(defun close-path (&rest coords)
  (declare (ignore coords)))

(defparameter *fn-assoc*
  `(("m" . moveto-rel)
    ("l" . lineto-rel)
    ("v" . vertical-rel)
    ("h" . horizontal-rel)
    ("c" . curveto-rel)
    ("s" . smooth-curveto-rel)
    ("M" . moveto-abs)
    ("L" . lineto-abs)
    ("V" . vertical-abs)
    ("H" . horizontal-abs)
    ("C" . curveto-abs)
    ("S" . smooth-curveto-abs)
    ("z" . close-path)))

(defun read-to-list (seq)
  "read a svg subpath definition of the form \"<opcode> {x,y }*\" into
  a lisp form"
  (read-from-string (format nil "(~a ~a)"
                            (cdr (assoc (elt seq 0) *fn-assoc* :test #'string=))
                            (subseq seq 1 nil))))

(defun get-segments (str)
  "transform an svg path by first splitting the path into subpaths for
each opcode and then map over the subpaths by calling the functions
defined in *fn-assoc* over their coords. The function returns sublists
of two or more points being the coords of segments to be rasterized
using de Casteljau's algorithm."
  (let ((*x* 0.0) (*y* 0.0)) ;;; init special variables used in the path extraction functions.
    (loop
      for path in (map 'list #'read-to-list (my-split (cl-ppcre:regex-replace-all "," str " ")))
      append (apply (symbol-function (first path)) (rest path)))))

(defun decasteljau (x points)
  "algorithm of de Casteljau interpolating a bezier curve segment with
given points. Returns the point on the curve between start (x=0) and
end (x=1) of segment."
  (flet ((ip (v1 v2) (+ (* v1 (- 1 x)) (* v2 x))))
    (if (= (length points) 2) (apply #'mapcar #'ip points)
        (decasteljau x (loop for (pt1 pt2) on points while pt2
                             collect (mapcar #'ip pt1 pt2))))))

(defun rasterize (points &optional (num-points 1000))
  "rasterize a path segment using de casteljau's algorithm."
  (loop for i below num-points
        collect (decasteljau (/ i num-points) points)))

;;;(declaim (optimize (speed 3) (safety 0)))

(defun vlen (x0 y0 x1 y1)
  "length of line from (x0 y0) to (x1 y1)."
  (declare (type single-float x0 y0 x1 y1))
  (sqrt (+ (* (- x1 x0) (- x1 x0))
           (* (- y1 y0) (- y1 y0)))))

(defun get-lengths (coords)
  "return the accumulated lengths between succesive coords."
  (loop for ((x0 y0) (x1 y1)) on coords by #'cdr while x1
        summing (vlen x0 y0 x1 y1) into res collect res))

(defun get-rasterized-coords (path &optional (points-per-segment 1000))
  "turn a svg path into coords. points-per-segment defines the number
of coords of each path segment to return."
  (apply #'append (mapcar (lambda (segment) (rasterize segment points-per-segment)) (rest (get-segments path)))))

(defun get-coords (path num &optional (points-per-segment 100))
  "return num coords approximately evenly spaced along the
path. points-per-segment defines the number of points of each path
segment which serve as the pool of candidates of points to be
returned. Increase this number to optimize the approximation of the
spacing of returned points."
  (let* ((coords (get-rasterized-coords path points-per-segment))
         (lengths (get-lengths coords))
         (total-length (first (last lengths))))
    (loop
      for pt in coords
      for curr-length in lengths
      with length-inc = (/ total-length num)
      with target-length = 0.0
;;;      do (break "pt: ~a, curr-length: ~a, length-inc: ~a target-length: ~a~%" pt curr-length length-inc target-length)
      if (> curr-length target-length)
        collect (progn
                  (incf target-length length-inc)
                  pt))))

(defun normalize-coords (coords)
  (let* ((vals (apply #'append coords))
         (min-val (apply #'min vals))
         (max-val (apply #'max vals))
         (fn (lambda (x) (+ -1 (* 2 (/ (- x min-val) (- max-val min-val)))))))
    (loop for pt in coords
          collect (destructuring-bind (x y) pt
                    (list (dround (funcall fn x) 4) (dround (funcall fn y) 4))))))
#|
Lisp Tutorial:

|#
