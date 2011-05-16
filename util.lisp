(in-package :ethif)

;; TODO: description
(defun strncpy (buf str max)
  (dotimes (i (min (length str) max))
    (setf (deref buf i) (char-code (aref str i))))
  (when (< (length str) max)
    (setf (deref buf (length str)) (char-code #\Null)))
  buf)

;; TODO: description
(defun mem-zero-set (sap size)
  (let ((*p (cast sap (* (unsigned 8)))))
    (dotimes (i size sap)
      (setf (deref *p i) 0))))

;; TODO: description
(defmacro with-zeroset-alien ((var type) &body body)
  `(with-alien ((,var ,type))
     (mem-zero-set ,var ,(mksym type ".SIZE"))
     ,@body))

(defun mksym (&rest args)
  (intern (format nil "~{~:@(~A~)~}" args)))

(defun make-socket-fd (domain type &key (protocol +ETH_P_DEFAULT+))
  (let ((fd (socket domain type protocol)))
    (when (/= fd -1)
      fd)))

(defmacro with-socket-fd ((fd domain type &key (protocol +ETH_P_DEFAULT+)) &body body)
  `(let ((,fd (make-socket-fd ,domain ,type :protocol ,protocol)))
     (unwind-protect
         (locally ,@body)
       (when ,fd
         (sb-unix:unix-close ,fd)))))
     
(defmacro with-ifreq ((var &key if-name if-index) &body body)
  `(with-zeroset-alien (,var ifreq)
     (when ,if-name
       (strncpy (ifreq.name ,var) ,if-name +IFNAMSIZ+))
     (when ,if-index
       (setf (ifreq.index ,var) ,if-index))
     ,@body))

(defun ioctl (fd cmd arg &key sap)
  (sb-unix:unix-ioctl fd cmd (if sap (alien-sap arg) arg)))

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
