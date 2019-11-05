(in-package :e/wire)

(defmethod make-wire (&key (receivers cl:CONS))
  (make-instance 'wire :receivers receivers))

(defmethod deliver-message ((schem (eql nil)) (out-part e/part:part) (out-pin e/pin:pin) (wire e/wire:wire) (message e/message:message))
  nil)

(defmethod deliver-message ((schem e/schematic:schematic) (out-part e/part:part) (out-pin e/pin:pin) (wire e/wire:wire) (message e/message:message))
  ;; we don't copy messages/data - receiver must copy the message-data if it intends to mutate it
  (mapc #'(lambda (part-pin)
              (let ((destination-part (e/part-pin:part part-pin))
                    (destination-pin  (e/part-pin:pin  part-pin))
                    (data (e/message:data message)))
                (e/send:deliver-message out-part out-pin destination-part destination-pin (e/message:make-message destination-pin data))))
        (receivers wire)))
