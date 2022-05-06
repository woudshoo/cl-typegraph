;; cl-typesetting/cl-typegraph copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package #:typeset)


(defclass abstract-graph-edge ()
  ((id :accessor id :initform (make-graph-node-id))
   (label :accessor label :initarg :label :initform nil)
   (head :accessor head :initarg :head)
   (tail :accessor tail :initarg :tail)
   (direction :accessor direction :initarg :direction :initform :forward)
   (edge-arrows :accessor edge-arrows :initarg :edge-arrows :initform '(:head :arrow))
   (data :accessor data :initarg :data :initform nil)
   (dot-attributes :accessor dot-attributes :initarg :dot-attributes :initform nil)
   (color :accessor color :initarg :color :initform '(0.0 0.0 0.0))
   (width :accessor width :initarg :width :initform 1)
   (label-color :accessor label-color :initarg :color :initform '(0.0 0.0 0.0))
   (label-x :accessor label-x)
   (label-y :accessor label-y)
   (points :accessor points :initform ())))


(defmethod initialize-instance :after ((edge abstract-graph-edge)
				       &key graph  &allow-other-keys)
  (add-to-graph graph edge))


(defclass graph-edge (abstract-graph-edge) ()
  (:documentation "Basic Edge, drawing the bezier path as returned by DOT"))

(defclass ortho-edge (abstract-graph-edge) ()
  (:documentation "Edge type that draws straight lines that are rounded."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric stroke-edge (edge data)
  (:documentation "Draw the EDGE.  DATA is currently ignored")
  
  (:method  ((edge graph-edge) data)
    (pdf:with-saved-state
      (pdf:set-color-stroke (color edge))
      (pdf:set-color-fill (color edge))
      (pdf:set-line-width (width edge))
      (let ((head-arrow-type (getf (edge-arrows edge) :head))
	    (tail-arrow-type (getf (edge-arrows edge) :tail)))
	(stroke-arrow edge tail-arrow-type (points edge))
	(bezier-from-xy-points (points edge))
	(pdf:stroke)
	(stroke-arrow edge head-arrow-type (reverse-path (points edge)))
	(stroke-edge-label edge))))

  (:method ((edge ortho-edge) data)
    (pdf:with-saved-state
      (pdf:set-color-stroke (color edge))
      (pdf:set-color-fill (color edge))
      (pdf:set-line-width (width edge))
      (let (
	    (head-arrow-type (getf (edge-arrows edge) :head))
	    (tail-arrow-type (getf (edge-arrows edge) :tail)))
	(stroke-arrow edge tail-arrow-type (points edge))
	(cl-pdf:polyline (polyline-from-bezier (points edge)) :radius 5)
	(pdf:stroke)
	(stroke-arrow edge head-arrow-type (reverse-path (points edge)))
	(stroke-edge-label edge))))  )

(defgeneric stroke-edge-label (edge)
  (:documentation "Draw the label of EDGE")
  (:method ((edge abstract-graph-edge))
    (when (label edge)
      (pdf:set-color-fill (label-color edge))
      (pdf:draw-centered-text (label-x edge)(label-y edge)(label edge)
			      *edge-label-font* *edge-label-font-size*))))

(defgeneric stroke-arrow (edge arrow-type path)
  (:documentation "Draw an arrow whose shape is indicated by ARROW-TYPE at the start of PATH.

This is for the path of EDGE, but EDGE is typically ignored.")

  (:method (edge arrow-type path)
    "Default is no arrow.")

  (:method ((edge abstract-graph-edge) (arrow-type (eql :arrow)) path)
    (let* ((x2 (pop path))
	   (y2 (pop path))
	   (x1 (pop path))
	   (y1 (pop path))
	   (nx (- x1 x2))
	   (ny (- y1 y2))
	   (l (/ (sqrt (+ (* nx nx) (* ny ny)))))
	   (x0 (+ x2 (* nx *arrow-length* l)))
	   (y0 (+ y2 (* ny *arrow-length* l)))
	   (dx (* nx *arrow-width* l))
	   (dy (* ny *arrow-width* l)))
      (pdf:move-to x2 y2)
      (pdf:line-to (+ x0 dy) (- y0 dx))
      (pdf:line-to (- x0 dy) (+ y0 dx))
      (pdf:line-to x2 y2)
      (pdf:fill-and-stroke)))

  (:method ((edge abstract-graph-edge) (arrow-type (eql :circle)) path)
    (let* ((x2 (pop path))
	   (y2 (pop path))
	   (x1 (pop path))
	   (y1 (pop path))
	   (nx (- x1 x2))
	   (ny (- y1 y2))
	   (l (/ (sqrt (+ (* nx nx) (* ny ny)))))
	   (x0 (+ x2 (* nx *arrow-length* l 0.5)))
	   (y0 (+ y2 (* ny *arrow-length* l 0.5))))
      (pdf:circle x0 y0 (*  *arrow-length* 0.5))
      (pdf:fill-and-stroke))))
