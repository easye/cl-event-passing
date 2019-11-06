(in-package :e/schematic)

(defclass schematic (e/part:part)
  ((child-wire-map :accessor child-wire-map :initarg :child-wire-map :initform (make-hash-table :test 'equal)) ;; pointers to wires between children parts (from self outputs)
   (self-input-wire-map :accessor self-input-wire-map :initarg :self-input-wire-map :initform (make-hash-table :test 'equal)) ;; pointers to wires from each of this schematic's inputs
   (instances :accessor instances :initform nil :initarg :instances)) ;; list of children parts
  (:default-initargs
   :reactor #'schematic-reactor))

(defmethod make-slot-for-each-output ((child e/part:part))
  (let ((hmap (make-hash-table)))
    (mapc #'(lambda (output-sym)
              (setf (gethash output-sym hmap) nil))
          (e/part:output-pins-as-list child))
    hmap))

(defmethod make-wire-map-slot-for-each-input ((self schematic) (in-pins e/pin-collection:pin-collection))
  (let ((hmap (make-hash-table)))
    (mapc #'(lambda (input-sym)
              (setf (gethash input-sym hmap) nil))
          (e/pin-collection:as-list in-pins))
    (setf (gethash nil (self-input-wire-map self)) hmap)))

(defmethod make-wire-map-slot-for-each-input ((self schematic) (in-pins (eql nil))))

(defun make-schematic (&key (in-pins nil) (out-pins nil) (first-time nil) (name ""))
  (let ((ins (e/pin-collection:from-list in-pins 'e/pin:schematic-input-pin))
        (outs (e/pin-collection:from-list out-pins 'e/pin:schematic-output-pin)))
    (let ((schem (make-instance 'schematic :in-pins ins :out-pins outs :first-time first-time :name name)))
      (e/pin-collection:set-parent-of-pins ins schem)
      (e/pin-collection:set-parent-of-pins outs schem)
      (make-wire-map-slot-for-each-input schem ins)
      schem)))

(defmethod add-instance ((self schematic) (instance e/part:part))
  (push instance (instances self))
  (e/part:set-parent instance self)
  (setf (gethash instance (child-wire-map self))
        (make-slot-for-each-output instance)))

(defun ensure-no-wire (hmap pin part str)
  (multiple-value-bind (val success)
      (gethash pin hmap)
    (unless success
      (let ((name (e/part:fetch-name part)))
        (let ((fmtmsg (format nil "~&part ~S has no ~A pin called ~S~%" name str (e/pin:as-symbol pin))))
          (error fmtmsg))))
    (unless (null val)
      (let ((name (e/part:fetch-name part)))
        (let ((fmtmsg (format nil "~&part ~S already has a wire on ~A pin ~S~%" name  str (e/pin:as-symbol pin))))
          (error fmtmsg))))
    t)) ;; all OK if we get here

(defun ensure-no-child-wire (hmap pin part str)
  (ensure-no-wire hmap pin part str))

(defun ensure-no-input-wire (hmap pin self str)
  (ensure-no-wire hmap pin self str))

(defmethod add-child-wire ((self schematic) (child-instance e/part:part) (child-output-pin e/pin:pin) (wire e/wire:wire))
  (multiple-value-bind (child-map success)
      (gethash child-instance (child-wire-map self))
    (cl:assert success)
    (ensure-no-child-wire child-map child-output-pin child-instance "output")
    (setf (gethash child-output-pin child-map)
          wire)))

(defmethod add-self-input-wire ((self schematic) (input-pin e/pin:pin) (wire e/wire:wire))
  ;; add a wire FROM an schematic's own input pin to internal parts (or its own output pin(s))
  (multiple-value-bind (self-input-wire-map success)
      (gethash nil (self-input-wire-map self))
    (cl:assert success) ;; broken if self doesn't have a valid map for its own self inputs
    (ensure-no-input-wire self-input-wire-map input-pin self "input")
    (setf (gethash input-pin (self-input-wire-map self))
          wire)))

(defmethod find-wire-for-input-pin ((self e/schematic:schematic) in-map (pin e/pin:pin))
  (multiple-value-bind (wire success)
      (gethash pin in-map)
    (unless success
      (let ((name (e/part:fetch-name self)))
        (let ((fmtstr (format nil "~&can't find wire in ~S for input pin ~S~%" name (e/pin:as-symbol pin))))
          (error fmtstr))))
    #+nil(assert success)
    wire))

(defmethod schematic-reactor ((self schematic) (msg e/message:message))
  "react to a single input message to a schematic - push the message inside the schematic
   to all parts attached to given input pin"
  (format *error-output* "~&schematic ~S reactor gets message ~S on pin ~S~%" (e/part:fetch-name self) (e/message:data msg) (e/pin:as-symbol (e/message:pin msg)))
  (let ((in-map (self-input-wire-map self))
        (schematic-input-pin (e/message:pin msg)))
    (let ((out-part self)
          (out-pin (e/message:pin msg)))
      (let ((wire (find-wire-for-input-pin self in-map out-pin)))
        (e/wire:deliver-message self out-part out-pin wire (e/message:data msg))))))

(defmethod find-wire-for-pin-inside-schematic ((schem (eql nil)) (child e/part:part) (child-out-pin e/pin:pin))
  nil)

(defmethod find-wire-for-pin-inside-schematic ((schem e/schematic:schematic) (child e/part:part) (child-out-pin e/pin:pin))
  (multiple-value-bind (child-part-output-map success)
      (gethash child (child-wire-map schem))
    (assert success)
    (multiple-value-bind (wire success)
        (gethash child-out-pin child-part-output-map)
      (assert success)
      wire)))

(defmethod push-input ((self e/schematic:schematic) (child (eql nil)) (msg e/message:message))
  ;; SENDing to an output of the schematic
  ;; return part that has new outputs, else return nil - in this case always (list self)
  ;; push-input always returns a list (or NIL, which is a list)
  (declare (ignore child))
  (e/part:push-output self msg))

(defmethod push-input ((self e/schematic:schematic) (child e/schematic:schematic) (msg e/message:message))
  ;; "normal" sending to a child part
  (declare (ignore self))
  (e/part:push-output self msg))

(defmethod push-input ((self e/schematic:schematic) (child e/leaf:leaf) (msg e/message:message))
  ;; "normal" sending to a child part
  (declare (ignore self))
  (e/part:push-input child msg))

