(in-package :e/send)

(defgeneric send (e/part:part e/message:message)
  (:documentation "queue the output message on self's output queue - using OUTPUT pin names
   the messages in the queue will be released by the dispatcher"))

(defmethod send ((self e/part:part) (msg e/message:message))
  (e/part:ensure-message-contains-valid-output-pin self msg)
  (format *error-output* "~&SENDing message ~S on output pin ~S from part ~S~%" (e/message:data msg) (e/pin:as-symbol (e/message:pin msg)) (e/part:fetch-name self))
  (e/part:push-output self msg))

(defmethod inject ((self e/part:part) (msg e/message:message))
  "send a message in from the outside - message already contains correct input :pin
   starts Dispatcher, if not already started"
  (format *error-output* "~&INJECTing message ~S on pin ~S of part ~S~%" (e/message:data msg) (e/pin:as-symbol (e/message:pin msg)) (e/part:fetch-name self))
  (e/part:push-input self msg)
  (e/dispatch:Start-Dispatcher))


;; legal combinations:
;;
;l            leaf-in leaf-out schem-in schem-out
;; leaf-in      -        -        -        -        
;; leaf-out     ok       -       ok       ok
;; schem-in     ok       -       ok       ok
;; schem-out    ok       -       ok       ok

(defmethod deliver ((out-part e/part:part) (out-pin e/pin:pin) (destination-part e/part:part) (destination-pin e/pin:pin) data)
  (e/part:ensure-valid-output-pin out-part out-pin)
  (e/part:ensure-valid-input-pin  destination-part destination-pin)
  (e/part:push-input destination-part (e/message:make-message destination-pin data)))

(defmacro illegal ()
  (assert nil))

(defmacro legal ()
  (deliver out-part out-pin destination-part destination-pin data))

(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:leaf-input-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:leaf-input-pin) (data T)) (illegal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:leaf-input-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:leaf-outpu-pin) (data T)) (illegal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:leaf-input-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:schematic-input-pin) (data T)) (illegal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:leaf-input-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:schematic-output-pin) (data T)) (illegal))

(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:leaf-output-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:leaf-input-pin) (data T)) (legal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:leaf-output-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:leaf-output-pin) (data T)) (illegal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:leaf-output-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:schematic-input-pin) (data T)) (legal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:leaf-output-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:schematic-output-pin) (data T)) (legal))

(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:schematic-input-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:leaf-input-pin) (data T)) (legal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:schematic-input-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:leaf-output-pin) (data T)) (illegal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:schematic-input-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:schematic-input-pin) (data T)) (legal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:schematic-input-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:schematic-output-pin) (data T)) (legal))

(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:schematic-output-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:leaf-input-pin) (data T)) (legal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:schematic-output-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:leaf-output-pin) (data T)) (illegal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:schematic-output-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:schematic-input-pin) (data T)) (legal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:schematic-output-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:schematic-output-pin) (data T)) (legal))

  
