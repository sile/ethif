(in-package :ethif)

;; http://linux.die.net/man/7/netdevice
;; http://www.manpagez.com/man/4/networking/

;; TODO: (socket-domain :af-inet) (socket-type :sock-dgram)
(defun hwaddr (interface-name &key (socket-domain +AF_INET+) (socket-type +SOCK_DGRAM+))
  (declare #.*muffle-compiler-note*)
  (with-socket-fd (fd socket-domain socket-type)
    (with-ifreq (req :if-name interface-name)
      (ioctl fd +SIOCGIFHWADDR+ req :sap t)
      (to-lisp-octets (sockaddr.data (ifreq.hwaddr req)) 6))))

(defun index (interface-name &key (socket-domain +AF_INET+) (socket-type +SOCK_DGRAM+))
  (declare #.*muffle-compiler-note*)
  (with-socket-fd (fd socket-domain socket-type)
    (with-ifreq (req :if-name interface-name)
      (ioctl fd +SIOCGIFINDEX+ req :sap t)
      (ifreq.index req))))
        
(defun ip (interface-name &key (socket-domain +AF_INET+) (socket-type +SOCK_DGRAM+))
  (declare #.*muffle-compiler-note*)
  (with-socket-fd (fd socket-domain socket-type)
    (with-ifreq (req :if-name interface-name)
      (ioctl fd +SIOCGIFADDR+ req :sap t)
      (to-lisp-octets (sockaddr-in.addr (ifreq.sockaddr req)) 4))))

(defun name (index &key (socket-domain +AF_INET+) (socket-type +SOCK_DGRAM+))
  (declare #.*muffle-compiler-note*)
  (with-socket-fd (fd socket-domain socket-type)
    (with-ifreq (req :if-index index)
      (ioctl fd +SIOCGIFNAME+ req :sap t)
      (to-lisp-string (ifreq.name req)))))

(defun flags (interface-name &key (socket-domain +AF_INET+) (socket-type +SOCK_DGRAM+))
  (declare #.*muffle-compiler-note*)
  (with-socket-fd (fd socket-domain socket-type)
    (with-ifreq (req :if-name interface-name)
      (ioctl fd +SIOCGIFFLAGS+ req :sap t)
      (let ((flags (ifreq.flags req)))
        ;; TODO:
        (mapcar (lambda (sym)
                  (let ((nm (symbol-name sym)))
                    (intern (subseq nm 5 (1- (length nm))) :keyword)))
                (remove-if (lambda (flag-sym)
                             (zerop (logand (symbol-value flag-sym) flags)))
                           (apropos-list "+IFF_" :ethif)))))))

(defun mtu (interface-name &key (socket-domain +AF_INET+) (socket-type +SOCK_DGRAM+))
  (declare #.*muffle-compiler-note*)
  (with-socket-fd (fd socket-domain socket-type)
    (with-ifreq (req :if-name interface-name)
      (ioctl fd +SIOCGIFMTU+ req :sap t)
      (ifreq.mtu req))))

;; TODO:
;; (defun map )
(defun all-ip (&key (socket-domain +AF_INET+) (socket-type +SOCK_DGRAM+))
  (declare #.*muffle-compiler-note*)
  (with-socket-fd (fd socket-domain socket-type)
    (with-ifconf (conf)
      ;; get size
      (ioctl fd +SIOCGIFCONF+ conf :sap t)
      (with-alien ((reqs (array ifreq) (ifconf.req-count conf)))
        (values (ifconf.len conf) ifreq.size)
      ))))
