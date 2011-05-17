(in-package :ethif)

(declaim #.*muffle-compiler-note*)

(defun hwaddr (interface-name)
  (with-ifreq (req :if-name interface-name)
    (with-eth-ioctl (+SIOCGIFHWADDR+ req)
      (to-lisp-octets (sockaddr.data (ifreq.hwaddr req)) 6))))

(defun set-hwaddr (interface-name new-hwaddr)
  (with-ifreq (req :if-name interface-name
                   :ha-family +ARPHRD_ETHER+
                   :ha-hwaddr new-hwaddr)
    (with-eth-ioctl (+SIOCSIFHWADDR+ req)
      new-hwaddr)))

(defun index (interface-name)
  (with-ifreq (req :if-name interface-name)
    (with-eth-ioctl (+SIOCGIFINDEX+ req)
      (ifreq.index req))))

(defun tx-queue-length (interface-name)
  (with-ifreq (req :if-name interface-name)
    (with-eth-ioctl (+SIOCGIFTXQLEN+ req)
      (ifreq.txqlen req))))

(defun set-tx-queue-length (interface-name new-tx-queue-length)
  (with-ifreq (req :if-name interface-name
                   :int-val new-tx-queue-length)
    (with-eth-ioctl (+SIOCSIFTXQLEN+ req)
      new-tx-queue-length)))

(defun ipaddr (interface-name)
  (with-ifreq (req :if-name interface-name)
    (with-eth-ioctl (+SIOCGIFADDR+ req)
      (to-lisp-octets (sockaddr-in.addr (ifreq.sockaddr req)) 4))))

(defun set-ipaddr (interface-name new-addr)
  (with-ifreq (req :if-name interface-name
                   :pa-family +AF_INET+
                   :pa-addr new-addr)
    (with-eth-ioctl (+SIOCSIFADDR+ req)
      new-addr)))

(defun broadaddr (interface-name)
  (with-ifreq (req :if-name interface-name)
    (with-eth-ioctl (+SIOCGIFBRDADDR+ req)
      (to-lisp-octets (sockaddr-in.addr (ifreq.broadaddr req)) 4))))

(defun set-broadaddr (interface-name new-addr)
  (with-ifreq (req :if-name interface-name
                   :pa-family +AF_INET+
                   :pa-addr new-addr)
    (with-eth-ioctl (+SIOCSIFBRDADDR+ req)
      new-addr)))

(defun dstaddr (interface-name)
  (with-ifreq (req :if-name interface-name)
    (with-eth-ioctl (+SIOCGIFDSTADDR+ req)
      (to-lisp-octets (sockaddr-in.addr (ifreq.dstaddr req)) 4))))

(defun set-dstaddr (interface-name new-addr)
  (with-ifreq (req :if-name interface-name
                   :pa-family +AF_INET+
                   :pa-addr new-addr)
    (with-eth-ioctl (+SIOCSIFDSTADDR+ req)
      new-addr)))

(defun netmask (interface-name)
  (with-ifreq (req :if-name interface-name)
    (with-eth-ioctl (+SIOCGIFNETMASK+ req)
      (to-lisp-octets (sockaddr-in.addr (ifreq.netmask req)) 4))))

(defun set-netmask (interface-name new-netmask)
  (with-ifreq (req :if-name interface-name
                   :pa-family +AF_INET+
                   :pa-addr new-netmask)
    (with-eth-ioctl (+SIOCSIFNETMASK+ req)
      new-netmask)))

(defun name (index)
  (with-ifreq (req :if-index index)
    (with-eth-ioctl (+SIOCGIFNAME+ req)
      (to-lisp-string (ifreq.name req)))))

(defun rename (old-name new-name)
  (with-ifreq (req :if-name old-name)
    (strncpy (ifreq.new-name req) new-name +IFNAMSIZ+)
    (with-eth-ioctl (+SIOCSIFNAME+ req)
      new-name)))

(defun mtu (interface-name)
  (with-ifreq (req :if-name interface-name)
    (with-eth-ioctl (+SIOCGIFMTU+ req)
      (ifreq.mtu req))))

(defun set-mtu (interface-name new-mtu)
  (with-ifreq (req :if-name interface-name
                   :int-val new-mtu)
    (with-eth-ioctl (+SIOCSIFMTU+ req)
      new-mtu)))

(defun list-all-ip-assigned-interfaces ()
  (with-ifconf (conf)
    ;; 1] get interface count
    (with-eth-ioctl (+SIOCGIFCONF+ conf)
      (let* ((count (ifconf.req-count conf))
             (reqs  (make-alien ifreq count)))
        (setf (ifconf.req conf) reqs)
        (unwind-protect
            ;; 2] get interface list
            (with-eth-ioctl (+SIOCGIFCONF+ conf)
              (loop FOR i FROM 0 BELOW count
                    FOR req = (deref reqs i)
                COLLECT (to-lisp-string (ifreq.name req))))
          (free-alien reqs))))))

(defun flags (interface-name)
  (with-ifreq (req :if-name interface-name)
    (with-eth-ioctl (+SIOCGIFFLAGS+ req)
      (loop WITH flags = (ifreq.flags req)
            FOR (_ mask kw) IN *iff-assoc-list*
            WHEN (plusp (logand mask flags))
        COLLECT kw))))

(defun set-flags (interface-name flags)
  (with-ifreq (req :if-name interface-name
                   :flags
                   (loop FOR flag IN flags
                         FOR mask = (or (iff.key->int flag)
                                        (format *error-output* "~&; WARN# undefined flag specified: ~a~%" flag)
                                        0)
                         SUM mask))
    (with-eth-ioctl (+SIOCSIFFLAGS+ req)
      flags)))

(defun flag (interface-name flag)
  (declare (#.`(member ,@(mapcar #'third *iff-assoc-list*)) flag))
  (with-ifreq (req :if-name interface-name)
    (with-eth-ioctl (+SIOCGIFFLAGS+ req)
      (plusp (logand (iff.key->int flag) (ifreq.flags req))))))
                     
(defun set-flag (interface-name flag enable)
  (declare (#.`(member ,@(mapcar #'third *iff-assoc-list*)) flag))
  (with-ifreq (req :if-name interface-name)
    (with-eth-ioctl (+SIOCGIFFLAGS+ req)
      (setf (ifreq.flags req)
            (boole (if enable boole-ior boole-andc2) (ifreq.flags req) (iff.key->int flag)))
      (with-eth-ioctl (+SIOCSIFFLAGS+ req)
        enable))))

#+IGNORE
(defun metric (interface-name)
  (with-ifreq (req :if-name interface-name)
    (with-eth-ioctl (+SIOCGIFMETRIC+ req)
      (ifreq.metric req))))

(defun arp (interface-name &key (socket-domain +AF_INET+) (socket-type +SOCK_DGRAM+) ip)
  (declare #.*muffle-compiler-note*)
  (with-socket-fd (fd socket-domain socket-type)
    (with-arpreq (req :if-name interface-name :family socket-domain
                      :ip (or ip (ipaddr interface-name)))
      (ioctl fd +SIOCGARP+ req :sap t)
      (to-lisp-octets (sockaddr.data (arpreq.ha req)) 6)
      )))

;; (defun map )


