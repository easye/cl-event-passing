(in-package :e/pin)

(defclass pin ()
  ((symbol :accessor symbol :initarg :symbol)
   (parent :accessor parent :initarg :parent)))
  

(defmethod pin-equal ((self pin) (other pin))
  (eq (symbol self) (symbol other)))

(defmethod as-symbol ((self pin))
  (symbol self))

(defclass leaf-input-pin (pin) ())
(defclass leaf-output-pin (pin) ())
(defclass schematic-input-pin (pin) ())
(defclass schematic-output-pin (pin) ())

(defun make-pin (parent make-class sym)
  (make-instance make-class :parent parent :symbol sym))

(defmethod set-parent ((self pin) parent)
  (setf (parent self) parent))
