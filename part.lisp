(in-package :e/part)


(defmethod initialize-instance :after ((self part) &key)
  (e/dispatch:remember-part self))

(defmethod set-parent ((self part) (parent part))
  (setf (parent self) parent))

(defmethod has-first-time-p ((self part))
  (not (null (first-time-function self))))




(defmethod ensure-is-input-pin ((self part) (pin e/pin:pin))
  (let ((pin-sym (e/pin:as-symbol pin)))
    (e/pin-collection:ensure-member (in-pins self) pin-sym self "input")))

(defmethod ensure-is-input-pin ((self part) (pin-sym cl:symbol))
  (e/pin-collection:ensure-member (in-pins self) pin-sym self "input"))

(defmethod ensure-is-output-pin ((self part) (pin e/pin:pin))
  (let ((pin-sym (e/pin:as-symbol pin)))
    (e/pin-collection:ensure-member (out-pins self) pin-sym self "output")))

(defmethod ensure-is-output-pin ((self part) (pin-sym cl:symbol))
  (e/pin-collection:ensure-member (out-pins self) pin-sym self "output"))




(defmethod react ((self part) (msg e/message:message))
  (funcall (reactor self) self msg))

(defmethod push-input ((self part) (msg e/message:message))
  (declare (ignore (parent-schem)))
  (e/queue:q-push (inqueue self) msg))

(defmethod push-output ((self part) (msg e/message:message))
  (e/queue:q-push (outqueue self) msg))

(defmethod has-input-p ((self part))
  (not (e/queue:empty-p (inqueue self))))

(defmethod has-output-p ((self part))
  (not (e/queue:empty-p (outqueue self))))

(defmethod pop-input ((self part))
  (e/queue:q-pop (inqueue self)))

(defmethod outqueue-as-list ((self part))
  (e/queue:as-list (outqueue self)))

(defmethod output-pins-as-list ((self part))
  (e/pin-collection:as-list (out-pins self)))

(defmethod lookup-output-pin ((self part) (pin-sym cl:symbol))
  (ensure-is-output-pin self pin-sym)
  (e/pin-collection:lookup-pin (out-pins self) pin-sym self "output"))

(defmethod lookup-input-pin ((self part) (pin-sym cl:symbol))
  (ensure-is-input-pin self pin-sym)
  (e/pin-collection:lookup-pin (in-pins self) pin-sym self "input"))

(defmethod make-output-queue-empty ((self part))
  (setf (outqueue self) (e/queue:make-empty-queue)))

(defmethod fetch-name ((self part))
  (if (and (stringp (name self))
           (not (string= "" (name self))))
      (name self)
    self))
  
(defmethod set-parent-of-pins ((self e/part:part))
  (e/pin-collection:set-parent-of-pins (in-pins self) self)
  (e/pin-collection:set-parent-of-pins (out-pins self) self))
