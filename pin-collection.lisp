(in-package :e/pin-collection)

;; a pin-collection is a collection of pins

;; each part has two pin collections - a collection of INPUT pins and a collection of OUTPUT pins

;; in this implementation, we define a pin-collection as a hash table indexed by e/pin:pin's

(defclass pin-collection ()
  ((pin-collection :accessor pin-collection :initform (make-hash-table :test 'equal))))

(defmethod from-list ((list-of-syms CONS))
  (let ((collection (make-instance 'pin-collection)))
    (mapc #'(lambda (sym)
              (setf (gethash sym (pin-collection collection))
                    (e/pin:make-pin sym)))
          list-of-syms)
    collection))

(defun make-empty-collection ()
  (let ((collection (make-instance 'pin-collection)))
    collection))

(defmethod as-list ((pc pin-collection))
  "return a list of pins"
  (let ((list nil))
    (maphash #'(lambda (sym pin)
                 (declare (ignore sym))
                 (push pin list))
             (pin-collection pc))
    list))

(defmethod ensure-member ((pc pin-collection) (pin-sym symbol) self str)
  (multiple-value-bind (pin success)
      (gethash pin-sym (pin-collection pc))
    (unless success
      (let ((name (e/part:fetch-name self)))
        (let ((fmtmsg (format nil "part ~S has no ~A pin ~S" name str pin-sym)))
          (error fmtmsg))))
    #+nil(assert success)
    pin))

(defmethod ensure-member ((pc pin-collection) (pin e/pin:pin) self str)
  (mapc #'(lambda (actual-pin)
            (when (e/pin:pin-equal pin actual-pin)
              (return-from ensure-member actual-pin)))  ;; pin might be a fake pin with same symbol name, use pin from as-list
        (as-list pc))
  (let ((name (e/part:fetch-name self)))
    (let ((fmtmsg (format nil "part ~S has no ~A pin ~S" name str pin-sym)))
      (error fmtmsg)))
  #+nil(assert nil))

(defmethod lookup-pin ((pc pin-collection) (pin e/pin:pin) self str)
  (ensure-member pc pin self str)) ;; same code as above

(defmethod lookup-pin ((pc pin-collection) (pin-sym symbol) self str)
  (ensure-member pc (e/pin:make-pin pin-sym) self str)) ;; same code as above
