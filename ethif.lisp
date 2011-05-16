(in-package :ethif)

;; http://linux.die.net/man/7/netdevice
;; http://www.manpagez.com/man/4/networking/
;; http://linuxjm.sourceforge.jp/html/LDP_man-pages/man7/arp.7.html

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

(defun tx-que-len (interface-name &key (socket-domain +AF_INET+) (socket-type +SOCK_DGRAM+))
  (declare #.*muffle-compiler-note*)
  (with-socket-fd (fd socket-domain socket-type)
    (with-ifreq (req :if-name interface-name)
      (ioctl fd +SIOCGIFTXQLEN+ req :sap t)
      (ifreq.index req)))) ;; XXX:

(defun arp (interface-name &key (socket-domain +AF_INET+) (socket-type +SOCK_DGRAM+) ip)
  (declare #.*muffle-compiler-note*)
  (with-socket-fd (fd socket-domain socket-type)
    (with-arpreq (req :if-name interface-name :family socket-domain
                      :ip (or ip (ip interface-name)))
      (ioctl fd +SIOCGARP+ req :sap t)
      (to-lisp-octets (sockaddr.data (arpreq.ha req)) 6)
      )))

;; XXX: 未実装
(defun metric (interface-name &key (socket-domain +AF_INET+) (socket-type +SOCK_DGRAM+))
  (declare #.*muffle-compiler-note*)
  (with-socket-fd (fd socket-domain socket-type)
    (with-ifreq (req :if-name interface-name)
      (ioctl fd +SIOCGIFMETRIC+ req :sap t)
      (ifreq.metric req))))
        
(defun ip (interface-name &key (socket-domain +AF_INET+) (socket-type +SOCK_DGRAM+))
  (declare #.*muffle-compiler-note*)
  (with-socket-fd (fd socket-domain socket-type)
    (with-ifreq (req :if-name interface-name)
      (ioctl fd +SIOCGIFADDR+ req :sap t)
      (to-lisp-octets (sockaddr-in.addr (ifreq.sockaddr req)) 4))))

(defun broadaddr (interface-name &key (socket-domain +AF_INET+) (socket-type +SOCK_DGRAM+))
  (declare #.*muffle-compiler-note*)
  (with-socket-fd (fd socket-domain socket-type)
    (with-ifreq (req :if-name interface-name)
      (ioctl fd +SIOCGIFBRDADDR+ req :sap t)
      (to-lisp-octets (sockaddr-in.addr (ifreq.broadaddr req)) 4))))

(defun dstaddr (interface-name &key (socket-domain +AF_INET+) (socket-type +SOCK_DGRAM+))
  (declare #.*muffle-compiler-note*)
  (with-socket-fd (fd socket-domain socket-type)
    (with-ifreq (req :if-name interface-name)
      (ioctl fd +SIOCGIFDSTADDR+ req :sap t)
      (to-lisp-octets (sockaddr-in.addr (ifreq.dstaddr req)) 4))))

(defun netmask (interface-name &key (socket-domain +AF_INET+) (socket-type +SOCK_DGRAM+))
  (declare #.*muffle-compiler-note*)
  (with-socket-fd (fd socket-domain socket-type)
    (with-ifreq (req :if-name interface-name)
      (ioctl fd +SIOCGIFNETMASK+ req :sap t)
      (to-lisp-octets (sockaddr-in.addr (ifreq.netmask req)) 4))))

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

(defun if-list (&key (socket-domain +AF_INET+) (socket-type +SOCK_DGRAM+))
  (declare #.*muffle-compiler-note*)
  (with-socket-fd (fd socket-domain socket-type)
    (with-ifconf (conf)
      ;; get size
      (ioctl fd +SIOCGIFCONF+ conf :sap t)
      (let* ((count (ifconf.req-count conf))
             (reqs (make-alien ifreq count)))
        (prog1
            (progn 
              (setf (ifconf.req conf) reqs)
              (ioctl fd +SIOCGIFCONF+ conf :sap t)
              (loop FOR i FROM 0 BELOW count
                    FOR req = (deref reqs i)
                    COLLECT
                    (list (to-lisp-string (ifreq.name req))
                          (to-lisp-octets (sockaddr-in.addr (ifreq.sockaddr req)) 4)
                          )))
          (free-alien reqs))))))
