(in-package :asdf)

(defsystem ethif
  :name "ethif"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description "show / manipulate ethernet interface"
 
  :serial t
  :components ((:file "package")
               (:file "alien-constant")
               (:file "alien-type")
               (:file "alien-function")
               (:file "util")
               (:file "ethif"))) 
