(in-package :e/pin)

(defclass pin ()
  ((symbl :accessor symbl :initarg :symbol)
   (parent :accessor parent :initarg :parent)))
  

(defmethod pin-equal ((self pin) (other pin))
  (eq (symbol self) (symbol other)))

(defmethod as-symbol ((self pin))
  (symbl self))

(defclass input-pin (pin) ())
(defclass output-pin (pin) ())

(defclass leaf-input-pin (input-pin) ())
(defclass leaf-output-pin (output-pin) ())
(defclass schematic-input-pin (input-pin) ())
(defclass schematic-output-pin (output-pin) ())

(defun make-pin (make-class sym)
  (make-instance make-class :symbol sym))

(defmethod set-parent ((self pin) parent)
  (setf (parent self) parent))
