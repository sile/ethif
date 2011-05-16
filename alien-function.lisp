(in-package :ethif)

(eval-when (:load-toplevel :execute)
  (define-alien-routine socket int (domain int) (type int) (protocol int)))
