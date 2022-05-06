;; cl-typesetting/cl-typegraph copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package #:typeset)


(defclass abstract-graph-node (box graph-positioning-mixin)
  ((id :accessor id :initform (make-graph-node-id))
   (data :accessor data :initarg :data :initform nil)
   (dot-attributes :accessor dot-attributes :initarg :dot-attributes :initform nil)
   (decoration :accessor decoration :initarg :decoration :initform +box+)
   (padding :accessor padding :initarg :padding :initform *box-padding*)
   (x :accessor x :initform 0)
   (y :accessor y :initform 0))
  (:default-initargs :dx nil :dy nil))


(defclass graph-node (abstract-graph-node) ())

(defclass graph-cluster (abstract-graph-node)
  ()
  (:default-initargs :data (list)))


(defmethod initialize-instance :after ((node abstract-graph-node) 
				       &key graph &allow-other-keys)
  (add-to-graph graph node))


(defgeneric adjust-graph-node-size (thing data)
  (:documentation  "Adjust NODE size to contain DATA.q

The size of the THING will be set to the size of DATA plus the size of the padding and
the size needed for the decoration.")
  
  (:method ((node abstract-graph-node) data)
    (incf (dx node) (content-size-adjust-x node))
    (incf (dy node) (content-size-adjust-y node)))


  (:method ((node graph-node) (data string))
    "For NODES containing plain strings, thiw will calculate the NODE size"
    (setf (dx node) (pdf::text-width (format nil "~a" data) *node-label-font* *node-label-font-size*))
    (setf (dy node) *node-label-font-size*)
    (call-next-method))

  (:method ((node graph-node) (box box))
    "For NODES containing a typeset BOX, it will size the NODE based
on the natural size of BOX."
    (compute-natural-box-size box)
    (setf (dx node) (dx box))
    (setf (dy node) (dy box))
    (call-next-method))


  (:method ((cluster graph-cluster) (data list))
    "For CLUSTER determine the size based on the boxes in DATA.

In addition this will adjust the X and Y coordinate so the CLUSTER
origin is set relative to the location of DATA"
    (loop :for b :in (data cluster)
	  :for x = (x b)
	  :for y = (y b)
	  :for dx = (dx b)
	  :for dy = (dy b)
	  :maximizing (+ x dx) :into max-x
	  :maximizing y  :into max-y
	  :minimizing x :into min-x
	  :minimizing (- y dy) :into min-y
	  :finally
	     (progn
	       (setf
		(x cluster) min-x
		(y cluster)  max-y)
	       (setf (dx cluster) (- max-x min-x))
	       (setf (dy cluster) (- max-y min-y))))
    (call-next-method)
    (decf (x cluster) (content-offset-x cluster))
    (incf (y cluster) (content-offset-y cluster))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod stroke-node ((node graph-positioning-mixin) data)
  (stroke-decoration node (decoration node))
  (stroke-node-content node data))

(defgeneric stroke-node-content (thing data)
  (:documentation "Draw DATA for THING.  THING is typically a node, but basically based for positioning")
  
  (:method ((node graph-positioning-mixin) data)
    (when data
      (pdf:set-color-fill '(0.0 0.0 0.0))
      (pdf:draw-centered-text (+ (x node) (* (dx node) 0.5))
			      (- (y node) (* (dy node) 0.5) (* 0.3 *node-label-font-size*))
			      (format nil "~a" data)
			      *node-label-font* *node-label-font-size*)))

  (:method ((node graph-positioning-mixin) (box box))
    (stroke box (content-x node) (content-y node))))

