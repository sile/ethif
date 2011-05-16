(in-package :ethif)

;; TODO: 整理
(define-alien-type size_t unsigned-long)
(define-alien-type socklen_t (unsigned 32))
(define-alien-type __be16 (unsigned 16))

(define-alien-type sockaddr
  (struct nil
    (family unsigned-short)
    (data   (array unsigned-char 14))))
(define-symbol-macro sockaddr.size (alien-size sockaddr :bytes))
(defmacro sockaddr.family (o) `(slot ,o 'family))
(defmacro sockaddr.data (o) `(slot ,o 'data))

(define-alien-type sockaddr-in
  (struct nil
    (family (unsigned 16))
    (port   (unsigned 16))
    (addr   (array (unsigned 8) 4))
    (__pad  (array char 8))))
(define-symbol-macro sockaddr-in.size (alien-size sockaddr-in :bytes))
(defmacro sockaddr-in.family (o) `(slot ,o 'family))
(defmacro sockaddr-in.port (o) `(slot ,o 'port))
(defmacro sockaddr-in.addr (o) `(slot ,o 'addr))

(define-alien-type ifreq
  (struct nil
    (name (array char #.+IFNAMSIZ+))
    (u (union nil
         (index int)
         (mtu int)
         (flags short)
         (hwaddr sockaddr)
         (sockaddr sockaddr-in)
         (__ (array char #.+IFNAMSIZ+))))))
(define-symbol-macro ifreq.size (alien-size ifreq :bytes))
(defmacro ifreq.name (o) `(slot ,o 'name))
(defmacro ifreq.index (o) `(slot (slot ,o 'u) 'index))
(defmacro ifreq.mtu (o) `(slot (slot ,o 'u) 'mtu))
(defmacro ifreq.flags (o) `(slot (slot ,o 'u) 'flags))
(defmacro ifreq.hwaddr (o) `(slot (slot ,o 'u) 'hwaddr))
(defmacro ifreq.sockaddr (o) `(slot (slot ,o 'u) 'sockaddr))

(define-alien-type ifconf
  (struct nil
    (len int)
    (_ (union nil
         (buf (* char))
         (req (* ifreq))))))
(define-symbol-macro ifconf.size (alien-size ifconf :bytes))
(defmacro ifconf.len (o) `(slot ,o 'len))
(defmacro ifconf.buf (o) `(slot (slot ,o '_) 'buf))
(defmacro ifconf.req (o) `(slot (slot ,o '_) 'req))
(defun ifconf.req-count (o) (values (floor (ifconf.len o) ifreq.size)))
                                           

(define-alien-type sockaddr-ll
  (struct nil
    (family   unsigned-short)
    (protocol __be16)
    (ifindex  int)
    (hatype   unsigned-short)
    (pkttype  unsigned-char)
    (halen    unsigned-char)
    (addr     (array unsigned-char 8))))
(define-symbol-macro sockaddr-ll.size (alien-size sockaddr-ll :bytes))
(defmacro sockaddr-ll.family (o) `(slot ,o 'family))
(defmacro sockaddr-ll.protocol (o) `(slot ,o 'protocol))
(defmacro sockaddr-ll.ifindex (o) `(slot ,o 'ifindex))
(defmacro sockaddr-ll.hatype (o) `(slot ,o 'hatype))
(defmacro sockaddr-ll.pkttype (o) `(slot ,o 'pkttype))
(defmacro sockaddr-ll.halen (o) `(slot ,o 'halen))
(defmacro sockaddr-ll.addr (o) `(slot ,o 'addr))