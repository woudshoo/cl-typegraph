(in-package #:typeset)

(defparameter *cps-max-x* 12)
(defparameter *cps-max-y* 12)

(defparameter *cps-node-width* 100)
(defparameter *cps-node-height* 100)

(defparameter *cps-node-width-margin* 10)
(defparameter *cps-node-height-margin* 10)
;;; Creating Problem
(defun var-name-from-id-cps (id)  id)
;;; node the functions below are wrong, they lead to name clashes.
;;; probably should be a symbol or a list or something different
(defun var-name-from-id-cluster-ll (id) (concatenate 'string id "-ll"))
(defun var-name-from-id-cluster-ur (id) (concatenate 'string id "-ur"))

(defun seq-from-1-set-1 (a set b)
  (fset:with-last (fset:concat (fset:seq a) set) b))

(defmethod gen-graph-cps-data ((node graph-node) (problem cps:problem))
  (cps:add-2d-variable problem (var-name-from-id-cps (id node))
		       :max-x *cps-max-x* :max-y *cps-max-y*))

(defmethod gen-graph-cps-data ((edge abstract-graph-edge) (problem cps:problem))
  (let ((head  (var-name-from-id-cps (id (head edge))))
	(tail  (var-name-from-id-cps (id (tail edge)))))
    (case (direction edge)
      (:up    (cps:add-<y-constraint problem head tail))
      (:down  (cps:add-<y-constraint problem tail head))
      (:left  (cps:add-<x-constraint problem head tail))
      (:right (cps:add-<x-constraint problem tail head)))))

(defmethod gen-graph-cps-data ((cluster graph-cluster) (problem cps:problem))
  (let* ((id (id cluster))
	 (ll (var-name-from-id-cluster-ll id))
	 (ur (var-name-from-id-cluster-ur id))
	 (inside (cps::set-from-list (mapcar #'id (data cluster))))
	 (outside (fset:set-difference (cps:variables problem) inside)))
    (cps:add-constraint problem
			(make-instance 'cps:basic-2d-q1-<=-1-*-1
				       :var-seq (seq-from-1-set-1 ll (fset:set-difference inside (fset:set ll ur)) ur) ))
    (cps:add-constraint problem
			(make-instance 'cps:basic-2d-not-q1-<=-1-*-1
				       :var-seq (seq-from-1-set-1 ll (fset:set-difference outside (fset:set ll ur)) ur)))))


(defun make-graph-cps-problem (graph)
  (let ((problem (make-instance 'cps:basic-problem)))
    ;;; Add all basic  nodes
    (iter (for (nil node) in-hashtable (nodes graph))
      (gen-graph-cps-data node problem))
    ;;; Add high-level constraints
    (cps:add-constraint problem (make-instance 'cps::basic-all-different :variables (cps::variables problem)))
    ;;; Add all edge induced constraints
    (iter (for (nil edge) in-hashtable (edges graph))
      (gen-graph-cps-data edge problem))

    (iter (for (nil . nodes) in (rank-constraints graph))
      (cps:add-constraint problem (make-instance 'cps:basic-y-= :variables (cps::set-from-list (mapcar 'id nodes)))))

    (iter (for (id . nil) in-hashtable (clusters graph))
	  (cps:add-2d-variable problem (var-name-from-id-cluster-ll id)
			       :max-x *cps-max-x* :max-y *cps-max-y*)
	  (cps:add-2d-variable problem (var-name-from-id-cluster-ur id)
			       :max-x *cps-max-x* :max-y *cps-max-y*))
    (iter (for (nil cluster) in-hashtable (clusters graph))
	  (gen-graph-cps-data cluster problem))
    problem))



;;; Parsing Results

(defun group-by-fn (graph problem fn)
  "Returns the following structure:

((0 node-1 node-2 ...) (1 node-3 node-4 ...) ...)

which is a list of lists.
Each sublist corresponds to a column and is formatted as:

(CULUMN-NUMMER . NODES-IN-COLUMN)

The result is sorted increasingly on column"
  (let ((table (make-hash-table)))
    (iter
      (for (id node) in-hashtable (nodes graph))
      (for xy = (cps::any-value (cps::domain problem (var-name-from-id-cps id))))
      (push node (gethash (funcall fn xy) table (list))))
    (sort 
     (iter (for (c nodes) in-hashtable table)
       (collect (cons c nodes)))
     #'< :key #'car)))

(defun group-by-column (graph problem)
  (group-by-fn graph problem #'car))

(defun group-by-row (graph problem)
  (group-by-fn graph problem #'cdr))


(defun calculate-column-widths (graph problem)
  (let ((columns (group-by-column graph problem)))
    (loop :for (col . nodes) :in columns
	  :for pos = 0 :then (+ pos width *cps-node-width-margin*)
	  :for width =  (reduce #'max nodes :key #'dx :initial-value 0)
	  :collect (list col pos width))))

(defun calculate-row-heights (graph problem)
  (let ((rows (group-by-row graph problem)))
    (loop :for (col . nodes) :in (reverse rows)
	  :for pos = 0 :then (+ pos height *cps-node-height-margin*)
	  :for height =  (reduce #'max nodes :key #'dy :initial-value 0)
	  :collect (list  col (+ pos height) height))))



(defun process-nodes (graph problem)
  (let ((x-info (calculate-column-widths graph problem))
	(y-info (calculate-row-heights graph problem)))
    (labels ((xy (id)
	       (cps::any-value (cps::domain problem (var-name-from-id-cps id))))
	     (px (id)
	       (let ((xi (cdr (assoc (car (xy id)) x-info))))
		 (car xi)))
	     (py (id)
	       (let ((yi (cdr (assoc (cdr (xy id)) y-info))))
		 (car yi))))

      (iter (for (id node) in-hashtable (nodes graph))
	(setf (x node) (px id)
	      (y node) (py id))))))


(defun process-edges (graph problem)
  (flet ((attach-point (from to)
	   "Attachment point on FROM when connected to TO."
	   (let* ((f-x (x from))
		  (f-y (y from))
		  (f-dx (dx from))
		  (f-dy (dy from))
		  (t-x (x to))
		  (t-y (y to))
		  (d-x (- t-x f-x))
		  (d-y (- t-y f-y)))
	     (cond
	       ((>= d-x (abs d-y))     (cons (+ f-x f-dx)                   (- f-y (* 0.5 f-dy))))
	       ((>= d-y (abs d-x))     (cons (+ f-x (* 0.5 f-dx))           f-y))
	       ((>= (- d-y) (abs d-x)) (cons (+ f-x (* 0.5 f-dx))           (- f-y f-dy)))
	       ((>= (- d-x) (abs d-y)) (cons f-x                            (- f-y (* 0.5 f-dy))))))))
    (iter (for (nil edge) in-hashtable (edges graph))
      (let ((from (attach-point (tail edge) (head edge)))
	    (to (attach-point (head edge) (tail edge))))
	(setf (points edge) (bezier-list-for-line (car from) (cdr from) (car to) (cdr to)))))))

;;;;  Entry point
(defun compute-graph-layout-cps (graph)
  (iter (for (nil node) in-hashtable (nodes graph))
    (adjust-graph-node-size node (data node)))
  (let ((problem (make-graph-cps-problem graph))
	(solver (make-instance 'cps:basic-solver)))

    (setf problem (cps:solve solver problem))

    (unless problem (error "Could not calculate layout"))
    (process-nodes graph problem)
    (process-edges graph problem)
    (iter (for (nil cluster) in-hashtable (clusters graph))
	  (adjust-graph-node-size cluster (data cluster)))
    (setf (dx graph) 1200
	  (dy graph) 1200))

  graph)


