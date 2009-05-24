(defpackage :versioned-objects.sysdef
  (:use :common-lisp :asdf))
(in-package :versioned-objects.sysdef)

(defsystem :versioned-objects
  :name "versioned objects"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "Trivial system to allow for the versioning of objects."
  :depends-on (:closer-mop)
  :components ((:file "versioned-objects")))