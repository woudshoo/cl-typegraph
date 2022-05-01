;; cl-typesetting/cl-typegraph copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package #:typeset)


(defclass abstract-decoration ()
  ((background-color :accessor background-color :initarg :background-color :initform '(1.0 1.0 1.0))
   (background-transparency :accessor background-transparency :initarg :background-transparency :initform 1.0)
   (border-color :accessor border-color :initarg :border-color :initform '(0.0 0.0 0.0))
   (border-transparency :accessor border-transparency :initarg :border-transparency :initform nil)
   (border-width :accessor border-width :initarg :border-width :initform 1)
   (size-adjust :accessor size-adjust :initarg :size-adjust :initform 0)))


(defclass box-decoration (abstract-decoration) ())

(defparameter +box+ (make-instance 'box-decoration))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric stroke-decoration (thing decoration)
  (:documentation "Draw DECORATION for THING.  THING is a node or graph
and is used for positioning.   The DECORATION is what is drawn.")

  (:method ((node graph-positioning-mixin) decoration))

  (:method ((node graph-positioning-mixin) (decoration box-decoration))
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
	 (pdf:stroke))))))



