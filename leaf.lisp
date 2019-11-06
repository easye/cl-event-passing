(in-package :e/leaf)

(defclass leaf (e/part:part)
  ())

(defun make-leaf (&key (first-time nil)
                       (reactor nil)
                       (in-pins  nil)
                       (out-pins nil)
                       (name ""))
  (let ((leaf (make-instance 'leaf
                             :first-time first-time
                             :reactor reactor
                             :in-pins  (e/pin-collection:from-list in-pins  'e/pin:leaf-input-pin)
                             :out-pins (e/pin-collection:from-list out-pins 'e/pin:leaf-output-pin)
                             :name name)))
    (e/part:set-parent-of-pins leaf)
    leaf))

