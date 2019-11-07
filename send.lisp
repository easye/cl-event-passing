(in-package :e/send)

(defgeneric send (e/part:part e/message:message)
  (:documentation "queue the output message on self's output queue - using OUTPUT pin names
   the messages in the queue will be released by the dispatcher"))

(defmethod send ((self e/part:part) (msg e/message:message))
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
;;  PINS      leaf-in leaf-out schem-in schem-out
;; leaf-in      -        -        -        -     
;; leaf-out     ok       -       ok       ok     
;; schem-in     ok       -       ok       ok     
;; schem-out    ok       -       ok       ok     
;;

;; deliver to top level
(defmethod deliver ((out-part T) (out-pin e/pin:pin) (destination-part (eql nil)) (destination-pin T) data)
  (format *standard-output* "~&output ~S on pin ~A~%" data out-pin))

(defmacro illegal ()
  `(error (format nil "illegal"))) ;; foil compiler into leaving this at runtime

(defmacro legal ()
  `(deliver out-part out-pin destination-part destination-pin data))

(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:leaf-input-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:leaf-input-pin) (data T)) (illegal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:leaf-input-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:leaf-output-pin) (data T)) (illegal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:leaf-input-pin) (destination-part e/schematic:schematic) (destination-pin e/pin:schematic-input-pin) (data T)) (illegal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:leaf-input-pin) (destination-part e/schematic:schematic) (destination-pin e/pin:schematic-output-pin) (data T)) (illegal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:leaf-input-pin) (destination-part (eql nil))   (destination-pin e/pin:schematic-output-pin) (data T)) (illegal))

(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:leaf-output-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:leaf-input-pin) (data T)) (legal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:leaf-output-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:leaf-output-pin) (data T)) (illegal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:leaf-output-pin) (destination-part e/schematic:schematic) (destination-pin e/pin:schematic-input-pin) (data T)) (legal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:leaf-output-pin) (destination-part e/schematic:schematic) (destination-pin e/pin:schematic-output-pin) (data T)) (legal))

(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:schematic-input-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:leaf-input-pin) (data T)) (legal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:schematic-input-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:leaf-output-pin) (data T)) (illegal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:schematic-input-pin) (destination-part e/schematic:schematic) (destination-pin e/pin:schematic-input-pin) (data T)) (legal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:schematic-input-pin) (destination-part e/schematic:schematic) (destination-pin e/pin:schematic-output-pin) (data T)) (legal))

(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:schematic-output-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:leaf-input-pin) (data T)) (legal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:schematic-output-pin) (destination-part e/leaf:leaf) (destination-pin e/pin:leaf-output-pin) (data T)) (illegal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:schematic-output-pin) (destination-part e/schematic:schematic) (destination-pin e/pin:schematic-input-pin) (data T)) (legal))
(defmethod deliver-message ((out-part e/part:part) (out-pin e/pin:schematic-output-pin) (destination-part e/schematic:schematic) (destination-pin e/pin:schematic-output-pin) (data T)) (legal))


;; cases where destination-part is SELF (NIL)
(defmethod deliver-message ((out-part T)           (out-pin e/pin:pin)                  (destination-part (eql nil))   (destination-pin T)                          (data T)) (legal))

  

;; leaf Parts can only output on output pins and input on input pins
;;
;; schematic Parts,though, can output on input/output pins and can input on input/output pins
;; (this may seem bizarre, but a schematic is a *hierarchical* Part, and it depends on whether you look on the *outside* of the Part or its *inside*
;; it's input pins look like input pins on the outside, but look like "output" pins on the inside and v.v.)

(defmethod deliver ((out-part e/part:part) (out-pin e/pin:output-pin) (destination-part e/part:part) (destination-pin e/pin:input-pin) data)
  (e/part:ensure-is-output-pin out-part out-pin)
  (e/part:ensure-is-input-pin  destination-part destination-pin)
  (e/part:push-input destination-part (e/message:make-message destination-pin data)))

(defmethod deliver ((out-part e/part:part) (out-pin e/pin:output-pin) (destination-part e/part:part) (destination-pin e/pin:output-pin) data)
  (e/part:ensure-is-output-pin out-part out-pin)
  (e/part:ensure-is-output-pin destination-part destination-pin)
  (e/part:push-input destination-part (e/message:make-message destination-pin data)))

(defmethod deliver ((out-part e/part:part) (out-pin e/pin:input-pin) (destination-part e/part:part) (destination-pin e/pin:input-pin) data)
  (e/part:ensure-is-input-pin out-part out-pin)
  (e/part:ensure-is-input-pin destination-part destination-pin)
  (e/part:push-input destination-part (e/message:make-message destination-pin data)))

(defmethod deliver ((out-part e/part:part) (out-pin e/pin:input-pin) (destination-part e/part:part) (destination-pin e/pin:output-pin) data)
  (e/part:ensure-is-input-pin  out-part out-pin)
  (e/part:ensure-is-output-pin destination-part destination-pin)
  (e/part:push-input destination-part (e/message:make-message destination-pin data)))

