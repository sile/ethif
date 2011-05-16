(defpackage ethif
  (:use :common-lisp :sb-alien)
  (:export name set-name
           index
           hwaddr set-hwaddr
           ip set-ip all-ip
           flags
           flag
           mtu))
(in-package :ethif)

(deftype octet () '(unsigned-byte 8))

(defvar *muffle-compiler-note* '(sb-ext:muffle-conditions sb-ext:compiler-note))
