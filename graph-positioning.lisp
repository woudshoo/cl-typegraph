;; cl-typesetting/cl-typegraph copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package #:typeset)


(defclass graph-positioning-mixin ()
  ()
  (:documentation "Helper class to unify drawing of borders and adding margins/padding.

Used by graph-node and graph classes.
Basically, a GRAPH-POSITIONING-MIXIN object is considered to have an outside perimeter
to be used to attach edges to and and an inside that contains the content of the thing, e.g.
a label of a graph node.  The purpose of this mixin is to determine the difference
in space occupied of the content of the GRAPH-POSITIONING-MIXIN object (e.g. a node) and
the outside of the GRAPH-POSITIONING-MIXIN.  For this it considers the PADDDING and
the DECORATION of the GRAPH-POSITIONING-MIXIN.

Basically, it will calculate the space needed for the DECORATION of the content and
the PADDING of the content."))


(defgeneric x (thing)
  (:documentation "X position of THING inside encompassing graph")
  (:method ((thing graph-positioning-mixin)) 0))

(defgeneric y (thing)
  (:documentation "Y positioning of THING inside encompassing graph")
  (:method ((thing graph-positioning-mixin)) 0))

(defgeneric padding (thing)
  (:documentation "Padding of the the content of the THING.

For nodes this this is the padding between the decoration of the node
and the content of the node.

The format of the result is the same as what WITH-QUAD accepts.")
  (:method ((thing graph-positioning-mixin)) 0))

(defgeneric decoration (thing)
  (:documentation "The decoration of the THING.

This is relevant as it is used to determine the size of the decoration.
The size of the decoration is used to determine the outer box/size
of THING.   More precise, the SIZE-ADJUST of the the decoration is used to pad
the size of THING to create room for the decoration.")
  (:method ((thing graph-positioning-mixin)) nil))

(defgeneric size-adjust (thing)
  (:documentation "Size adjust for thing, result should be in a format WITH-QUAD understands")
  (:method (thing) 0))

(defgeneric content-offset-x (thing)
  (:documentation "Offset of the content of THING relative to the position of THING.")
  (:method ((node graph-positioning-mixin))
    (with-quad (l-d) (size-adjust (decoration node))
      (with-quad (l-p) (padding node)
	(+ l-d l-p )))))

(defgeneric content-offset-y (thing)
  (:documentation "Offset of the content of THING relative to the position of THING.")
  (:method ((node graph-positioning-mixin))
    (with-quad (l-d t-d) (size-adjust (decoration node))
      (with-quad (l-p t-p) (padding node)
	(+ t-d t-p )))))

(defgeneric content-x (thing)
  (:documentation "X coordinate of content of THING.

This is based on the X position of THING and the content offset")
  (:method  ((node graph-positioning-mixin))
    (+ (x node) (content-offset-x node))))

(defgeneric content-y (thing)
  (:documentation "Y coordinate of content of THING.

This is based on the Y position of THING and the content offset")
  (:method ((node graph-positioning-mixin))
    (- (y node) (content-offset-y node))))

(defgeneric  content-size-adjust-x (thing)
  (:documentation "Total amount of space between the width of the content and the width of the THING.")
  (:method ((node graph-positioning-mixin))
    (with-quad (l-a t-a r-a) (size-adjust (decoration node))
      (with-quad (l-p t-p r-p) (padding node)
	(+ l-a l-p r-a r-p)))))

(defgeneric content-size-adjust-y (thing)
  (:documentation "Total amount of space between the height of the content and the height of THING.")
  (:method ((node graph-positioning-mixin))
    (with-quad (l-a t-a r-a b-a) (size-adjust (decoration node))
      (with-quad (l-p t-p r-p b-p) (padding node)
	(+ t-a t-p b-a b-p)))))
