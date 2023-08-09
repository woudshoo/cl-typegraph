;; cl-typesetting/cl-typegraph copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package #:typeset)

(defun make-graph-file-id ()
  (format nil "F~d" (incf *graph-id-counter*)))

(defun gen-dot-attributes (s attributes &optional comma)
  (loop for (attribute value) in attributes do
	(if comma
	    (write-string ", " s)
	    (setf comma t))
	(write-string attribute s)
	(write-char #\= s)
	(write-line value s)))

(defmethod gen-graph-dot-data ((graph graph) s)
  (iter (for (nil node) in-hashtable (nodes graph))
    (adjust-graph-node-size node (data node)))
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

(defmethod gen-graph-dot-data ((edge abstract-graph-edge) s)
  (format s "~s -> ~s [label=\"~a\", arrowhead=none, color=\"~a\""
	  (id (tail edge)) (id (head edge))
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
	  (adjust-graph-node-size cluster (data cluster)))))
