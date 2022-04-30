;; cl-typesetting/cl-typegraph copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package #:typeset)

;(defparameter *dot-command* "dot -Tps ~s -o ~s")
(defparameter *dot-command* "dot")
(defparameter *dot-command-args* '("-Tplain-ext"))
(defparameter *graph-file-prefix* "/tmp/")
(defparameter *arrow-width* 2)
(defparameter *arrow-length* 6)
(defparameter *edge-label-font* (pdf:get-font "helvetica"))
(defparameter *edge-label-font-size* 9)
(defparameter *node-label-font* (pdf:get-font "helvetica"))
(defparameter *node-label-font-size* 12)
(defparameter *box-padding* '(4 2 4 2))


(defvar *graph-id-counter* 0)

(defun make-graph-node-id ()
  (format nil "N~d" (incf *graph-id-counter*)))

(defun make-graph-file-id ()
  (format nil "F~d" (incf *graph-id-counter*)))

(defclass graph-node-decoration-box ()
  ((background-color :accessor background-color :initarg :background-color :initform '(1.0 1.0 1.0))
   (background-transparency :accessor background-transparency :initarg :background-transparency :initform 1.0)
   (border-color :accessor border-color :initarg :border-color :initform '(0.0 0.0 0.0))
   (border-transparency :accessor border-transparency :initarg :border-transparency :initform nil)
   (border-width :accessor border-width :initarg :border-width :initform 1)
   (size-adjust :accessor size-adjust :initarg :size-adjust :initform 0)))

(defparameter +box+ (make-instance 'graph-node-decoration-box))

(defclass graph-box-mixin ()
  ()
  (:documentation "Helper class to unify drawing of borders and adding margins/padding.
Used by graph-node and graph classes"))

(defmethod x ((box graph-box-mixin)) 0)
(defmethod y ((box graph-box-mixin)) 0)
(defmethod padding ((box graph-box-mixin)) 0)
(defmethod decoration ((box graph-box-mixin)) nil)

(defclass graph-node (box graph-box-mixin)
  ((id :accessor id :initform (make-graph-node-id))
   (data :accessor data :initarg :data :initform nil)
   (dot-attributes :accessor dot-attributes :initarg :dot-attributes :initform nil)
   (decoration :accessor decoration :initarg :decoration :initform +box+)
   (padding :accessor padding :initarg :padding :initform *box-padding*)
   (x :accessor x :initform 0)
   (y :accessor y :initform 0))
  (:default-initargs :dx nil :dy nil))

(defclass graph-cluster (graph-node)
  ()
  (:default-initargs :data (list)))

(defclass graph-edge ()
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

;;; dot-attributes is a list of (attribute value) pairs ex: ("rankdir" "LR")

(defclass graph (box graph-box-mixin)
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

(defmethod initialize-instance :after ((node graph-node) 
				       &key fixed-height fixed-width graph &allow-other-keys)
  (adjust-graph-node-size node (data node) fixed-width fixed-height)
  (add-to-graph graph node))

(defmethod initialize-instance :after ((edge graph-edge)
				       &key graph  &allow-other-keys)
  (add-to-graph graph edge))

(defmethod add-to-graph (graph node)
  nil)

(defmethod add-to-graph ((graph graph) (node graph-node))
  (setf (gethash (id node) (nodes graph)) node))

(defmethod add-to-graph ((graph graph) (edge graph-edge))
  (setf (gethash (id edge) (edges graph)) edge))

(defmethod add-to-graph ((graph graph) (cluster graph-cluster))
  (setf (gethash (id cluster) (clusters graph)) cluster))

(defun get-node (graph id)
  (etypecase id
    (string (gethash id (nodes graph)))
    (symbol (gethash (symbol-name id) (nodes graph)))))


(defun add-rank-constraint (graph constraint nodes)
  (push (cons constraint nodes) (rank-constraints graph)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Size and location functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod adjust-graph-node-size ((node graph-node) (data t) fixed-height fixed-width)
  "Adjust NODE size to contain DATA.

The size of the NODE will be set to the size of DATA plus the size of the padding and
the size needed for the decoration.

If one of the FIXED-HEIGHT or FIXED-WIDTH is a generalized true, the corresponding
height or width will NOT be modified by this method."
  (unless fixed-width (incf (dx node) (content-size-adjust-x node)))
  (unless fixed-height (incf (dy node) (content-size-adjust-y node))))

(defmethod adjust-graph-node-size ((node graph-node) (data string) fixed-width fixed-height)
  (unless fixed-width
    (setf (dx node) (pdf::text-width (format nil "~a" data) *node-label-font* *node-label-font-size*)))
  (unless fixed-height
    (setf (dy node) *node-label-font-size*))
  (call-next-method))

(defmethod adjust-graph-node-size ((node graph-node) (box box) fixed-width fixed-height)
  (unless (and fixed-width fixed-height)
    (compute-natural-box-size box))
  (if fixed-width
      (setf (dx node) (or (dx node) (dx box)))
      (setf (dx node) (dx box)))
  (if fixed-height
      (setf (dy node) (or (dy node) (dy box)))
      (setf (dy node) (dy box)))
  (call-next-method))


(defmethod adjust-graph-node-size ((cluster graph-cluster) data fixed-width fixed-height)
  (loop :for b :in (data cluster)
	:for x = (x b)
	:for y = (y b)
	:for dx = (dx b)
	:for dy = (dy b)
	:maximizing (+ x dx) :into max-x
	:maximizing y  :into max-y
	:minimizing x :into min-x
	:minimizing (- y dy) :into min-y
	:finally (setf (x cluster) min-x
		       (dx cluster) (- max-x min-x)
		       (y cluster)  max-y
		       (dy cluster) (- max-y min-y)))
  (call-next-method)
  (decf (x cluster) (content-offset-x cluster))
  (incf (y cluster) (content-offset-y cluster)))

(defmethod content-offset-x ((node graph-box-mixin))
  (with-quad (l-d) (size-adjust (decoration node))
    (with-quad (l-p) (padding node)
      (+ l-d l-p ))))


(defmethod content-offset-y ((node graph-box-mixin))
  (with-quad (l-d t-d) (size-adjust (decoration node))
    (with-quad (l-p t-p) (padding node)
      (+ t-d t-p ))))

(defmethod content-offset-y ((node graph))
  (with-quad (l-d t-d r-d b-d) (size-adjust (decoration node))
    (with-quad (l-p t-p r-p b-p) (padding node)
      (+ b-d b-p ))))

(defmethod content-x ((node graph-box-mixin))
  (+ (x node) (content-offset-x node)))

(defmethod content-y ((node graph-box-mixin))
  (- (y node) (content-offset-y node)))

(defmethod content-size-adjust-x ((node graph-box-mixin))
  (with-quad (l-a t-a r-a) (size-adjust (decoration node))
    (with-quad (l-p t-p r-p) (padding node)
      (+ l-a l-p r-a r-p))))

(defmethod content-size-adjust-y ((node graph-box-mixin))
  (with-quad (l-a t-a r-a b-a) (size-adjust (decoration node))
    (with-quad (l-p t-p r-p b-p) (padding node)
      (+ t-a t-p b-a b-p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing dot files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gen-dot-attributes (s attributes &optional comma)
  (loop for (attribute value) in attributes do
	(if comma
	    (write-string ", " s)
	    (setf comma t))
	(write-string attribute s)
	(write-char #\= s)
	(write-line value s)))

(defmethod gen-graph-dot-data ((graph graph) s)
  (let ((*print-readably* nil))
    (format s "digraph G {
size=\"~a,~a\";
edge [fontname=~a,fontsize=~a];
"
	    (/ (- (max-dx graph) (content-size-adjust-x graph)) 72.0)
	    (/ (- (max-dy graph) (content-size-adjust-y graph)) 72.0)
	    (pdf:name *edge-label-font*) *edge-label-font-size*)
    (loop for (rank-constraint . nodes) in (rank-constraints graph) do
      (format s "{rank = ~a; ~{~s;~^ ~}};~%" rank-constraint (mapcar 'id nodes)))
    (format s "graph [")
    (gen-dot-attributes s (dot-attributes graph))
    (format s "];")
    (iter (for (id cluster) in-hashtable (clusters graph))
	  (gen-graph-dot-data cluster s))
    (iter (for (id node) in-hashtable (nodes graph))
      (gen-graph-dot-data node s))
    (iter (for (id edge) in-hashtable (edges graph))
      (gen-graph-dot-data edge s))
    (format s "}~%")))

(defmethod gen-graph-dot-data ((node graph-node) s)
  (format s "~s [fixedsize=true, width=~a, height=~a, shape=rect"
	  (id node)(/ (dx node) 72.0)(/ (dy node) 72.0))
  (gen-dot-attributes s (dot-attributes node) t)
  (format s "];~%"))

(defmethod gen-graph-dot-data ((edge graph-edge) s)
  (format s "~s -> ~s [label=\"~a\", arrowhead=none, color=\"~a\""
	  (id (head edge)) (id (tail edge))
	  (if (label edge) (label edge) "")
	  (id edge))
  (gen-dot-attributes s (dot-attributes edge) t)
  (format s "];~%"))


(defmethod gen-graph-dot-data ((cluster graph-cluster) s)
  (format s "subgraph cluster_~a {~{~s;~^ ~}};~%" (id cluster) (mapcar #'id (data cluster))))

(defun read-graph-line-values (string)
  (when string
    (let ((*package* (find-package :keyword)))
      (iter (for position first 0 then end)
	    (for (values object end) = (read-from-string string nil nil :start position))
	    (while object)
	    (collect object)))))

(defun process-graph-line (graph values)
  (setf (scale graph)  (first values)
	(dx graph) (+ (* (second values) (scale graph) 72.0) (content-size-adjust-x graph))
	(dy graph) (+ (* (third values) (scale graph) 72.0) (content-size-adjust-y graph))))

(defun process-graph-node-line (graph values)
  (let ((node (get-node graph (pop values))))
    (setf (x node) (- (* (pop values) 72.0) (* (dx node) 0.5))
	  (y node) (+ (* (pop values) 72.0) (* (dy node) 0.5)))))

(defun process-graph-edge-line (graph values)
  (let* ((id  (symbol-name (car (last values))))
	 (edge (gethash id (edges graph))))
    (pop values)
    (pop values)
    (setf (points edge) (iter (repeat (pop values))
			      (collect (* (pop values) 72.0))
			      (collect (* (pop values) 72.0))))
    (when (label edge)
      (pop values)
      (setf (label-x edge) (* (pop values) 72.0)
	    (label-y edge) (* (pop values) 72.0)))))

;;; this should be changed to use pipes instead of files and adapted to other Lisp implementations.
(defun compute-graph-layout (graph)
  (let* ((file-id (make-graph-file-id))
	 (dot-file (concatenate 'string *graph-file-prefix* file-id ".dot"))
	 (result-file  (concatenate 'string *graph-file-prefix* file-id ".txt")))
    (unwind-protect
	 (progn
	   (with-open-file (s dot-file :direction :output :if-exists :supersede)
	     (gen-graph-dot-data graph s))
#+lispworks (sys:call-system (format nil "~a~{ ~s~} ~s -o ~s" *dot-command* *dot-command-args* dot-file result-file) :wait t)
#+cmu (ext:run-program *dot-command* `(,@*dot-command-args* ,dot-file "-o" ,result-file) :wait t)
#+sbcl (sb-ext:run-program *dot-command* `(,@*dot-command-args* ,dot-file "-o" ,result-file) :wait t :search t )
	   (with-open-file (s result-file :direction :input)
	     (iter (for line = (read-line s nil))
		   (while line)
		   (for (line-type . values) = (read-graph-line-values line))
		   (case line-type
		     (:node (process-graph-node-line graph values))
		     (:edge (process-graph-edge-line graph values))
		     (:graph (process-graph-line graph values))
		     (:stop (finish))))))
      (progn (ignore-errors (delete-file dot-file))
	     (ignore-errors (delete-file result-file))))
    ;; post process to adjust cluster sizes based on content
    (iter (for (nil cluster) in-hashtable (clusters graph))
	  (adjust-graph-node-size cluster (data cluster) nil nil))))

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
    (stroke-node-decoration graph (decoration graph))
    (pdf:translate (content-offset-x graph) (content-offset-y graph))
    (pdf:scale (scale graph)(scale graph))
    (iter (for (nil cluster) in-hashtable (clusters graph))
	  (stroke-node cluster nil))
    (iter (for (id edge) in-hashtable (edges graph))
      (stroke-edge edge (data edge)))
    (iter (for (id node) in-hashtable (nodes graph))
      (stroke-node node (data node)))))

(defmethod stroke-node ((node graph-node) data)
  (stroke-node-decoration node (decoration node))
  (stroke-node-content node data))


(defmethod stroke-node-decoration ((node box) decoration))

(defmethod stroke-node-decoration ((node box) (decoration graph-node-decoration-box))
  (pdf:with-saved-state
    (when (background-color decoration)
      (pdf:set-color-fill (background-color decoration))
      (pdf:set-fill-transparency (background-transparency decoration)))
    (when (border-width decoration)
      (pdf:set-color-stroke (border-color decoration))
      (pdf:set-line-width (border-width decoration)))
    (pdf:basic-rect (x node) (y node)(dx node)(- (dy node)))
    (cond
      ((and (background-color decoration) (border-width decoration))
       (pdf:fill-and-stroke))
      ((background-color decoration)
       (pdf:fill-path))
      ((border-width decoration)
       (pdf:stroke)))))


(defmethod stroke-node-content ((node graph-node) data)
  (when data
    (pdf:set-color-fill '(0.0 0.0 0.0))
    (pdf:draw-centered-text (+ (x node) (* (dx node) 0.5))
			    (- (y node) (* (dy node) 0.5) (* 0.3 *node-label-font-size*))
			    (format nil "~a" data)
			    *node-label-font* *node-label-font-size*)))

(defmethod stroke-node-content ((node graph-node) (box box))
  (stroke box (content-x node) (content-y node)))


(defmethod stroke-edge ((edge graph-edge) data)
  (pdf:with-saved-state
      (pdf:set-color-stroke (color edge))
      (pdf:set-color-fill (color edge))
      (pdf:set-line-width (width edge))
    (let ((points (tighten-path (points edge) -3))
	  (head-arrow-type (getf (edge-arrows edge) :head))
	  (tail-arrow-type (getf (edge-arrows edge) :tail)))
      (stroke-arrow edge tail-arrow-type (points edge))
      (make-pdf-bezier-curve (points edge))
      (pdf:stroke)
      (stroke-arrow edge head-arrow-type (reverse-path (points edge)))
      (when (label edge)
	(pdf:set-color-fill (label-color edge))
	(pdf:draw-centered-text (label-x edge)(label-y edge)(label edge)
				*edge-label-font* *edge-label-font-size*)))))

(defmethod stroke-arrow (edge arrow-type path))

(defmethod stroke-arrow ((edge graph-edge) (arrow-type (eql :arrow)) path)
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

(defmethod stroke-arrow ((edge graph-edge) (arrow-type (eql :circle)) path)
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
    (pdf:fill-and-stroke)))
