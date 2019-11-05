(in-package :e/leaf)

(defclass leaf (e/part:part)
  ())

(defun make-leaf (&key (first-time nil)
                       (reactor nil)
                       (in-pins  nil)
                       (out-pins nil)
                       (name ""))
  (let ((ins (or in-pins (e/pin-collection:make-empty-collection)))
        (outs (or out-pins (e/pin-collection:make-empty-collection))))
    (let ((obj
           (make-instance 'leaf
                          :first-time first-time
                          :reactor reactor
                          :in-pins ins
                          :out-pins outs
                          :name name)))
      (e/pin-collection:set-parent ins obj)
      (e/pin-collection:set-parent outs obj)
      obj)))

