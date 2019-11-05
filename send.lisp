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

