(in-package :e/wire)

(defmethod make-wire (&key (receivers cl:CONS))
  (make-instance 'wire :receivers receivers))

(defmethod deliver-message ((schem e/schematic:schematic) (out-part e/part:part) (out-pin e/pin:pin) (wire e/wire:wire) (data T))
  ;; we don't copy messages/data - receiver must copy the message-data if it intends to mutate it
  (mapc #'(lambda (part-pin)
              (let ((destination-part (e/part-pin:part part-pin))
                    (destination-pin  (e/part-pin:pin  part-pin)))
                (e/send:deliver-message out-part out-pin destination-part destination-pin data)))
        (receivers wire)))

(defmethod deliver-message ((schem T) (out-part T) (out-pin e/pin:pin) (wire (eql nil)) (data T))
  (declare (ignore schem out-part wire))
  (format *standard-output* "~&output ~S on pin ~S~%" data out-pin))

