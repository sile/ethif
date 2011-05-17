(defpackage ethif
  (:use :common-lisp :sb-alien)
  (:export name rename
           index
           hwaddr set-hwaddr
           tx-queue-length set-tx-queue-length
           ipaddr set-ipaddr
           flags set-flags
           flag set-flag
           mtu set-mtu
           netmask set-netmask
           broadaddr set-broadaddr
           dstaddr set-dstaddr
           list-all-ip-assigned-interfaces))
(in-package :ethif)

(deftype octet () '(unsigned-byte 8))

(defvar *muffle-compiler-note* '(sb-ext:muffle-conditions sb-ext:compiler-note))
