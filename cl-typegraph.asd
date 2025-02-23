;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; cl-typesetting copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package asdf)

(defsystem :cl-typegraph
  :name "cl-typegraph"
  :author "Marc Battyani <marc.battyani@fractalconcept.com>, Wim Oudshoorn <woudshoo@xs4all.nl"
  :version "0.5"
  :maintainer "Marc Battyani <marc.battyani@fractalconcept.com>"
  :licence "BSD like licence"
  :description "Common Lisp Graph Typesetting"
  :long-description "The cl-typegraph package is a stand-alone Common Lisp graph typesetting system."
  :perform (load-op :after (op cl-typegraph)
		    (pushnew :cl-typegraph cl:*features*))
  :serial t
  :components ((:file "config")
	       (:file "path-functions")
	       (:file "graph-positioning")
	       (:file "decorations")
	       (:file "graph-node")
	       (:file "graph-edge")
	       (:file "graph")
	       (:file "graph-dot")
	       (:file "graph-cps"))
  :depends-on (:cl-typesetting :cps :alexandria))

