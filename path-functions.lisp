(in-package #:typeset)


(defun reverse-path (path)
  "Reverse the PATH.

PATH is a list of alternating x y coordinates, e.g.
(x1 y1 x2 y2 x3 y3).

The result is
(x3 y3 x2 y2 x1 y2)"
  (loop
    :with result = (list)
    :for (x y) :on path :by #'cddr
    :do (push y result) (push x result)
    :finally (return result)))

(defun vector-length (x y)
  "Euclidian length of vector ((0 0) --> (x y)).

So basically sqrt (x^2 + y2)"
  (sqrt (+ (* x x) (* y y))))

(defun normalized-vector (x y)
  "Returns vector (x y) normalized to length 1

Will error out when length is zero (that is, when x=0 and y=0)."
  (let ((l (vector-length x y)))
    (list (/ x l) (/ y l))))

(defun normalized-path-direction (path)
  "Returns as a (x y) pair the normalized direction of the bezier path from the start.

That is, direction (x y) is the tangent direction of the be bezier
curve PATH at first point."
  (let ((x0 (pop path))
	(y0 (pop path)))
    (loop :for x1 = (pop path)
	  :for y1 = (pop path)
	  :while (and (= x0 x1) (= y0 y1))
	  :finally (return (normalized-vector (- y1 y0) (- x1 x0))))))


(defun tighten-path (path looseness)
  (flet ((step-6 (list)
	   (cdddr (cdddr list)))
	 (between (a b)
	   (+ (* (- 1 looseness) a)
	      (* looseness b))))
    (loop
      :for (ax ay bx by cx cy dx dy next) :on path :by #'step-6
      :while dy
      :append (list ax ay
	       (between ax bx)
	       (between ay by)
	       (between dx cx)
	       (between dy cy))
	:into result
      :unless next :append (list dx dy) :into result
      :finally (return result))))



(defun draw-point (x y color)
  (pdf:with-saved-state
    (pdf:set-color-fill color)
    (pdf:basic-rect (- x 2) (- y 2) 4 4)
    (pdf:fill-path)))

(defun make-pdf-bezier-curve (points)
  (pdf:move-to (pop points) (pop points))
  (loop :while points
	:do
	   (pdf:bezier-to (pop points) (pop points)
			  (pop points) (pop points)
			  (pop points) (pop points))))

(defun draw-bezier-with-control-points (path)
  (pdf:with-saved-state
    (pdf:set-color-stroke "black")
    (make-pdf-bezier-curve path)
    (pdf:stroke)
    (draw-point (pop path) (pop path) "green")
    (loop :while path
	  :do
	     (draw-point (pop path) (pop path) "blue")
	     (draw-point (pop path) (pop path) "blue")
	     (draw-point (pop path) (pop path) "green"))))


(defun polyline-from-bezier (points)
  "Returns a polyline as ((x1 y1) (x2 y2)...) from
the bezier cureve in points, as (x1 y1 x2 y2 ...).

The construction is done by dropping the control points."
  (let ((result (list)))
    (push (list (pop points) (pop points)) result)
    (loop :while points
	  :do
	     (loop :repeat 4 :do (pop points))
	     (push (list (pop points) (pop points)) result))
    (nreverse result)))
