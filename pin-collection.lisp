(in-package :e/pin-collection)

;; a pin-collection is a collection of pins

;; each part has two pin collections - a collection of INPUT pins and a collection of OUTPUT pins

;; in this implementation, we define a pin-collection as a hash table indexed by e/pin:pin's

(defclass pin-collection ()
  ((pin-collection :accessor pin-collection :initform (make-hash-table :test 'equal))))

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

(defmethod as-list ((pc (eql nil))) nil)

(defmethod set-parent ((pc pin-collection) parent)
  (maphash #'(lambda (sym pin)
               (declare (ignore sym))
               (e/pin:set-parent pin parent))
           (pin-collection pc)))

(defmethod set-parent ((pc (eql nil)) parent))

(defmethod from-list ((list-of-syms (eql nil)) make-class)
  (make-empty-collection))

(defmethod from-list ((list-of-syms CONS) make-class)
  (let ((collection (make-instance 'pin-collection)))
    (mapc #'(lambda (sym)
              (setf (gethash sym (pin-collection collection))
                    (e/pin:make-pin make-class sym)))
          list-of-syms)
    collection))

(defmethod ensure-member ((pc pin-collection) (pin-sym symbol) (self e/part:part) str)
  (multiple-value-bind (pin success)
      (gethash pin-sym (pin-collection pc))
    (unless success
      (let ((name (e/part:fetch-name self)))
        (let ((fmtmsg (format nil "part ~S has no ~A pin ~S" name str pin-sym)))
          (error fmtmsg))))
    #+nil(assert success)
    pin))

(defmethod ensure-member ((pc pin-collection) (pin-sym cl:symbol) (self e/part:part) str)
  (mapc #'(lambda (pin)
            (when (eq pin-sym (e/pin:as-symbol pin))
              (return-from ensure-member pin)))
        (as-list pc))
  (let ((name (e/part:fetch-name self)))
    (let ((fmtmsg (format nil "part ~S has no ~A pin ~S" name str pin-sym)))
      (error fmtmsg)))
  #+nil(assert nil))

(defmethod lookup-pin ((pc pin-collection) (pin cl:symbol) (self e/part:part) str)
  (ensure-member pc pin self str))

(defmethod set-parent-of-pins ((pc pin-collection) (parent e/part:part))
  (maphash #'(lambda (sym pin)
               (declare (ignore sym))
               (e/pin:set-parent pin parent))
           (pin-collection pc)))

(defmethod set-parent-of-pins ((pc (eql nil)) (parent e/part:part)))