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
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
        :when beg :collect (subseq string (1- beg) end)
    :while end))

(defparameter *x* 0)
(defparameter *y* 0)

;;; Path extraction functions:

(defun moveto-rel (&rest coords)
  (loop for (x y) on coords by #'cddr
        collect `(,(incf *x* x) ,(incf *y* y))))

(defun lineto-rel (&rest coords)
  (loop for (x y) on coords by #'cddr
        collect `(,(incf *x* x) ,(incf *y* y))))

(defun vertical-rel (&rest coords)
  (loop for y in coords
        collect `(,*x* ,(incf *y* y))))

(defun horizontal-rel (&rest coords)
  (loop for x in coords
        collect `(,(incf *x* x) ,*y*)))

(defun curveto-rel (&rest coords)
  (loop for (x1 y1 x2 y2 x y) on coords by (lambda (x) (nthcdr 6 x))
        collect `(,(incf *x* x) ,(incf *y* y))))

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

(defun get-coords (str)
  "transform an svg path by first splitting the path into subpaths for
each opcode and then map over the subpaths by calling the functions
defined in *fn-assoc* over their coords."
  (let ((*x* 0.0) (*y* 0.0)) ;;; init special variables used in the path extraction functions.
    (loop
      for path in (map 'list #'read-to-list (my-split (cl-ppcre:regex-replace-all "," str " ")))
      append (apply (symbol-function (first path)) (rest path)))))

(defun normalize-coords (coords)
  (let* ((vals (apply #'append coords))
         (min-val (apply #'min vals))
         (max-val (apply #'max vals))
         (fn (lambda (x) (+ -1 (* 2 (/ (- x min-val) (- max-val min-val)))))))
    (loop for pt in coords
          collect (destructuring-bind (x y) pt
                    (list (dround (funcall fn x) 4) (dround (funcall fn y) 4))))))
