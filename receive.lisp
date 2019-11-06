(in-package :e/receive)

(defgeneric respond (e/part:part e/message:message)
  (:documentation "execute a response (a closure/function/callback) when a message arrives in the input queue
   the response may cause 0 or more calls to SEND"))

(defmethod respond (e/part:part e/message:message)
  (e/part:react part message))
