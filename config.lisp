(in-package #:typeset)

(defvar *dot-command* "dot")
(defvar *dot-command-args* '("-Tplain-ext"))
(defvar *graph-file-prefix* "/tmp/")
(defvar *arrow-width* 2)
(defvar *arrow-length* 6)
(defvar *edge-label-font* (pdf:get-font "helvetica"))
(defvar *edge-label-font-size* 9)
(defvar *node-label-font* (pdf:get-font "helvetica"))
(defvar *node-label-font-size* 12)
(defvar *box-padding* '(4 2 4 2))
