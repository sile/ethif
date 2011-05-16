(defpackage ethif
  (:use :common-lisp :sb-alien)
  (:export get-name set-name
           get-index
           get-hwaddr set-hwaddr
           get-ip set-ip))
(in-package :ethif)

(defvar *muffle-compiler-note* '(sb-ext:muffle-conditions sb-ext:compiler-note))
