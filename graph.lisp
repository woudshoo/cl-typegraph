;; cl-typesetting/cl-typegraph copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package #:typeset)


;;; dot-attributes is a list of (attribute value) pairs ex: ("rankdir" "LR")

(defclass graph (box graph-positioning-mixin)
  ((nodes :accessor nodes :initform (make-hash-table :test #'equal))
   (edges :accessor edges :initform (make-hash-table :test #'equal))
   (clusters :accessor clusters :initform (make-hash-table :test #'equal))
   (dot-attributes :accessor dot-attributes :initarg :dot-attributes :initform nil)
   (rank-constraints :accessor rank-constraints :initform nil)
   (decoration :accessor decoration :initarg :decoration :initform nil)
   (landscape-layout :accessor landscape-layout :initarg :landscape-layout :initform nil)
   (padding :accessor padding :initarg :padding :initform 0)
   (max-dx :accessor max-dx :initarg :max-dx :initform 400)
   (max-dy :accessor max-dy :initarg :max-dy :initform 400)
   (scale :accessor scale :initform 1)))

(defmethod y ((box graph)) (dy box))
(defmethod content-offset-y ((graph graph))
    "Note that the GRAPH coordinates work in the usual PDF coordinate system, so Y goes up."
    (with-quad (l-d t-d r-d b-d) (size-adjust (decoration graph))
      (with-quad (l-p t-p r-p b-p) (padding graph)
	(+ b-d b-p ))))

(defgeneric add-to-graph (graph thing)
  (:documentation "Add THING to GRAPH.  THING is a node, edge or cluster")
  (:method (graph node) nil)

  (:method  ((graph graph) (node graph-node))
    (setf (gethash (id node) (nodes graph)) node))

  (:method ((graph graph) (edge abstract-graph-edge))
    (setf (gethash (id edge) (edges graph)) edge))

  (:method ((graph graph) (cluster graph-cluster))
    (setf (gethash (id cluster) (clusters graph)) cluster)))


(defun get-node (graph id)
  (etypecase id
    (string (gethash id (nodes graph)))
    (symbol (gethash (symbol-name id) (nodes graph)))))

(defun add-rank-constraint (graph constraint nodes)
  (push (cons constraint nodes) (rank-constraints graph)))


(defun graph-box (graph &rest args)
  (let ((dx (dx graph))
	(dy (dy graph)))
    (when (landscape-layout graph)
      (rotatef dx dy))
    (add-box (apply 'make-instance 'user-drawn-box
		    :stroke-fn #'(lambda (box x y)
				   (if (landscape-layout graph)
				       (pdf:with-saved-state
					   (pdf:translate x (- y dy))
					   (pdf:rotate 90)
					   (stroke graph 0 0))
				       (stroke graph x y)))
		    :inline t :dx dx :dy dy
		    :allow-other-keys t args))))

(defmethod stroke ((graph graph) x y)
  (pdf:with-saved-state
    (pdf:translate x (- y (dy graph)))
    (stroke-decoration graph (decoration graph))
    (pdf:translate (content-offset-x graph) (content-offset-y graph))
    (pdf:scale (scale graph)(scale graph))
    (iter (for (nil cluster) in-hashtable (clusters graph))
	  (stroke-node cluster nil))
    (iter (for (id edge) in-hashtable (edges graph))
      (stroke-edge edge (data edge)))
    (iter (for (id node) in-hashtable (nodes graph))
      (stroke-node node (data node)))))

