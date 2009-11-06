(defpackage :versioned-objects
  (:use :common-lisp
	:closer-mop)
  (:shadowing-import-from :closer-mop :defmethod :standard-generic-function :defmethod :defgeneric :standard-generic-function)
  (:export :versioned-object
	   :snapshot
	   :rm-snapshot
	   :restore
	   :changed-p
	   :snapshot-p
	   :snapshot-value))

(in-package :versioned-objects)

(defconstant +unbound-slot+ '+unbound-slot+)
(defparameter *auto-snap-init* :init "Set this to nil to stop the storage of the initial state in :init")

(defgeneric snapshot (object name)
  (:documentation "Takes a snapshot of the given class"))
(defgeneric restore (object version)
  (:documentation "Restores the object to the given version"))
(defgeneric changed-p (object version &rest options &key test)
  (:documentation "Indicates the class has changed with respect to the variables that have been set in the given version"))
(defgeneric snapshot-p (object version)
  (:documentation "Indicates whether or not <object> has a snapshot named <version>"))
(defgeneric snapshot-value (object version slot)
  (:documentation "Returns the value <object> had for slot <slot> in snapshot <snaphot>"))
(defgeneric rm-snapshot (object snapshot)
  (:documentation "Removes the snapshot <snapshot> from object <object>"))

(defclass versioned-object ()
  ((versioned-vals :initform nil)
   (versioned-variables :initform nil :initarg versioned-variables :accessor versioned-variables))
  (:documentation "This is an object that can store previous versions"))

(defmethod initialize-instance :after ((object versioned-object) &rest rest)
  (declare (ignore rest))
  (with-slots (versioned-variables)
      object
    (when (null versioned-variables)
      (setf versioned-variables (loop for key in (map 'list 'slot-definition-name (compute-slots (class-of object)))
				   when (not (eq key 'versioned-vals)) collect key))))
  (when *auto-snap-init* (snapshot object :init)))

(defmethod snapshot ((object T) name)
  T)
(defmethod snapshot ((object versioned-object) name)
  (let ((plist (loop for var in (slot-value object 'versioned-variables) collect
		    (cons var (if (slot-boundp object var)
				  (slot-value object var)
				  +unbound-slot+)))))
    (setf (getf (slot-value object 'versioned-vals) name) plist))
  object)

(defmethod restore ((object versioned-object) version)
  (with-slots (versioned-vals)
      object
    (loop for (var . val) in (getf versioned-vals version) 
       do (if (eql val +unbound-slot+)
	      (slot-makunbound object var)
	      (setf (slot-value object var) val))))
  object)

(defmethod changed-p ((vo versioned-object) version &key (test 'eql))
  (let ((vars (getf (slot-value vo 'versioned-vals) version)))
    (when vars
      (when (find-if (lambda (kv) (not (funcall test (slot-value vo (car kv)) (cdr kv)))) vars)
	T))))
(defmethod snapshot-p ((vo versioned-object) version)
  (getf (slot-value vo 'versioned-vals) version))
(defmethod snapshot-value ((vo versioned-object) version slot)
  (loop for (var . val) in (getf (slot-value vo 'versioned-vals) version)
       when (eql slot var) do (return-from snapshot-value val))
  nil)

(defmethod rm-snapshot ((vob versioned-object) snapshot)
  (setf (slot-value vob 'versioned-vals)
	(loop for (snpsht values) on (slot-value vob 'versioned-vals) by #'cddr when (not (eql snapshot snpsht)) append (list snpsht values))))