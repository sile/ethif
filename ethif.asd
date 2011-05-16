(in-package :asdf)

(defsystem ethif
  :name "ethif"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description "TODO: ... Ethernet Interface ..."
 
  :serial t
  :components ((:file "package")
               (:file "alien-constant")
               (:file "alien-type")
               (:file "alien-function")
               (:file "ethif"))) 
