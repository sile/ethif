(defpackage ethif
  (:use :common-lisp :sb-alien)
  (:export name set-name
           index
           hwaddr set-hwaddr
           ip set-ip 
           if-list
           flags set-flags
           flag
           mtu
           netmask set-netmask
           broadaddr set-broadaddr
           dstaddr
           metric))
(in-package :ethif)

(deftype octet () '(unsigned-byte 8))

(defvar *muffle-compiler-note* '(sb-ext:muffle-conditions sb-ext:compiler-note))
