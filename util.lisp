(in-package :ethif)

(defun strncpy (buf str max)
  (dotimes (i (min (length str) max))
    (setf (deref buf i) (char-code (aref str i))))
  (when (< (length str) max)
    (setf (deref buf (length str)) (char-code #\Null)))
  buf)

(defun memcpy (alien-dst lisp-src size)
  (dotimes (i size)
    (setf (deref alien-dst i) (aref lisp-src i))))

(defun mem-zero-set (sap size)
  (let ((*p (cast sap (* (unsigned 8)))))
    (dotimes (i size sap)
      (setf (deref *p i) 0))))

(defun mkstr (&rest args)
  (format nil "~{~:@(~A~)~}" args))

(defmacro with-zeroset-alien ((var type) &body body)
  `(with-alien ((,var ,type))
     (mem-zero-set ,var ,(intern (mkstr type ".SIZE")))
     (locally
      ,@body)))

(defun make-socket-fd (domain type &key (protocol +ETH_P_DEFAULT+))
  (let ((fd (socket domain type protocol)))
    (when (/= fd -1)
      fd)))

(defmacro with-socket-fd ((fd domain type &key (protocol +ETH_P_DEFAULT+)) &body body)
  `(let ((,fd (make-socket-fd ,domain ,type :protocol ,protocol)))
     (when ,fd
       (unwind-protect
           (locally ,@body)
         (sb-unix:unix-close ,fd)))))
     
(defmacro with-ifreq ((var &key if-name if-index 
                           ha-family ha-hwaddr int-val 
                           pa-family pa-addr
                           flags) &body body)
  `(with-zeroset-alien (,var ifreq)
     (when ,if-name
       (strncpy (ifreq.name ,var) ,if-name +IFNAMSIZ+))
     (when ,if-index
       (setf (ifreq.index ,var) ,if-index))

     (when ,ha-family
       (setf (sockaddr.family (ifreq.hwaddr ,var)) ,ha-family))
     (when ,ha-hwaddr
       (memcpy (sockaddr.data (ifreq.hwaddr ,var)) ,ha-hwaddr 6))
     
     (when ,pa-family
       (setf (sockaddr-in.family (ifreq.sockaddr ,var)) ,pa-family))
     (when ,pa-addr
       (memcpy (sockaddr-in.addr (ifreq.sockaddr ,var)) ,pa-addr 4))
     
     (when ,flags
       (setf (ifreq.flags ,var) ,flags))

     (when ,int-val
       (setf (ifreq.index ,var) ,int-val)) ; XXX:
     ,@body))

(defun ioctl (fd cmd arg &key sap)
  (sb-unix:unix-ioctl fd cmd (if sap (alien-sap arg) arg)))

(defun eth-ioctl (cmd arg)
  (with-socket-fd (fd +AF_INET+ +SOCK_DGRAM+)
    (ioctl fd cmd arg :sap t)))

 (defun to-lisp-octets (alien-octets size)
  (let ((octets (make-array size :element-type 'octet)))
    (dotimes (i size octets)
      (setf (aref octets i) (deref alien-octets i)))))

(defun to-lisp-string (alien-string)
  (loop FOR i FROM 0 
        WHILE (plusp (deref alien-string i))
    COLLECT (deref alien-string i) INTO str
    FINALLY (return (map 'string #'code-char str))))

(defmacro with-ifconf ((var) &body body)
  `(with-zeroset-alien (,var ifconf)
     ,@body))

(defmacro with-arpreq ((var &key if-name family ip) &body body)
  `(with-zeroset-alien (,var arpreq)
     (when ,family
       (setf (sockaddr-in.family (arpreq.pa ,var)) ,family)) ;; XXX
     (when ,ip
       (dotimes (i 4)
         (setf (deref (sockaddr-in.addr (arpreq.pa ,var)) i)
               (aref ,ip i))))
     (setf (sockaddr.family (arpreq.ha ,var)) #x0100) ;; XXX: hwtype == eth == #x0100

     (when ,if-name
       (strncpy (arpreq.dev ,var) ,if-name +IFNAMSIZ+))
     ,@body))

(defun error-reason ()
  (sb-int:strerror (sb-alien:get-errno)))

(defmacro with-eth-ioctl ((cmd arg) &body body)
  `(if (eth-ioctl ,cmd ,arg)
       (multiple-value-bind (#1=#:fst #2=#:snd) (locally ,@body) 
         (values #1# #2#))
     (values nil (error-reason))))
