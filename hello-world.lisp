;; test todo: test that wire from schem input pin to schem output pin works
;; test todo: test that wire from schem input pin can split

(in-package :cl-event-passing)

(defun hello ()
  ;; lots of mundane, very explicit, bookkeeping here
  ;; - this is where a diagram compiler might help cut down on syntactic noise
  ;; - (in fact, if I hadn't started out with diagrams, I might never have arrived at this combination of features)
  (e/dispatch:reset-dispatcher)
  (let ((schem (e/schematic:make-schematic :name "schem"))
        (sender (e/leaf:make-leaf
                 :name "sender"
                 :first-time #'start-sender
                 :out-pins (e/pin-collection:from-list '(:out))))
        (receiver (e/leaf:make-leaf
                   :name "receiver"
                   :reactor #'receiver-display
                   :in-pins (e/pin-collection:from-list '(:in)))))
    (e/schematic:add-instance schem sender)
    (e/schematic:add-instance schem receiver)
    (let ((receiver-pair (e/part-pin:make-pair receiver (e/part:lookup-input-pin receiver :in))))
      (let ((wire (e/wire:make-wire :receivers (list receiver-pair))))
	;; wire is the wire between the sender's output (:out) and the receiver's input (:in)
	;; wire is added to the schematic's map, as output from sender's :out pin
        (e/schematic:add-child-wire schem sender (e/part:lookup-output-pin sender :out) wire))))
  (e/dispatch:Start-Dispatcher))

(defun hello2 ()
  (e/dispatch:reset-dispatcher)
  ;; wire straight from sender to output of schematic
  (let ((schem (e/schematic:make-schematic :out-pins (e/pin-collection:from-list '(:schem-out)) :name "schem"))
        (sender (e/leaf:make-leaf
                 :name "sender"
                 :first-time #'start-sender
                 :out-pins (e/pin-collection:from-list '(:out)))))
    (e/schematic:add-instance schem sender)
    (let ((receiver-pair (e/part-pin:make-pair nil (e/part:lookup-output-pin schem :schem-out))))
      (let ((wire (e/wire:make-wire :receivers (list receiver-pair))))
	;; wire is the wire between the sender's output (:out) and the schematic's output (:schem-out)
	;; wire is added to the schematic's map, as output from sender's :out pin
        (e/schematic:add-child-wire schem sender (e/part:lookup-output-pin sender :out) wire))))
  (e/dispatch:Start-Dispatcher))

(defun hello3 ()
  (e/dispatch:reset-dispatcher)
  ;; wire straight from first-time of schematic to output of schematic
  (let ((schem (e/schematic:make-schematic
                :name "schem"
                :first-time #'start-schematic-sender
                :out-pins (e/pin-collection:from-list '(:schem-out)))))
    (e/dispatch:Start-Dispatcher)))

(defun hello4 ()
  ;; schematic has an input :schem-in, injecting into that pin goes to sender, which then goes to receiver
  (e/dispatch:reset-dispatcher)
  (let ((schem (e/schematic:make-schematic
                :name "schem"
                :in-pins (e/pin-collection:from-list '(:schem-in))))
        (sender (e/leaf:make-leaf
                 :name "sender"
                 :reactor #'pass-through
                 :in-pins (e/pin-collection:from-list '(:in))
                 :out-pins (e/pin-collection:from-list '(:out))))
        (receiver (e/leaf:make-leaf
                   :name "receiver"
                   :reactor #'receiver-display
                   :in-pins (e/pin-collection:from-list '(:in)))))
    (e/schematic:add-instance schem sender)
    (e/schematic:add-instance schem receiver)
    (let ((receiver-pair (e/part-pin:make-pair receiver (e/part:lookup-input-pin receiver :in)))
          (sender-input-pair (e/part-pin:make-pair sender (e/part:lookup-input-pin sender :in))))
      (let (;; wire is the wire between the sender's output (:out) and the receiver's input (:in)
            ;; wire is added to the schematic's map, as output from sender's :out pin
            (wire (e/wire:make-wire :receivers (list receiver-pair)))
            
            ;; input-wire comes from the input of the shematic (:schem-in) to the input of the
            ;; sender part (:in (NB :in is also the name of the input pin of the receiver part))
            (input-wire (e/wire:make-wire :receivers (list sender-input-pair))))
        
        (e/schematic:add-child-wire schem sender (e/part:lookup-output-pin sender :out) wire)
        (e/schematic:add-self-input-wire schem (e/part:lookup-input-pin schem :schem-in) input-wire)))
    (e/send:inject schem (e/message:make-message (e/part:lookup-input-pin schem :schem-in) "hello4"))))

(defun hello5 ()
  ;; schematic has an input :schem-in, injecting into that pin goes to its own output :schem-out
  (e/dispatch:reset-dispatcher)
  (let ((schem (e/schematic:make-schematic
                :name "schem"
                :in-pins (e/pin-collection:from-list '(:schem-in))
                :out-pins (e/pin-collection:from-list '(:schem-out)))))
    (let ((self-receiver-pair (e/part-pin:make-pair nil (e/part:lookup-output-pin schem :schem-out))))
      (let (;; wire is the wire between self's input (:schem-in) and self's output (:schem-out)
            (wire (e/wire:make-wire :receivers (list self-receiver-pair))))
        (e/schematic:add-self-input-wire schem (e/part:lookup-input-pin schem :schem-in) wire)))
    (e/send:inject schem (e/message:make-message (e/part:lookup-input-pin schem :schem-in) "hello 5"))))

(defun hello6 ()
  ;; two layers of schematics, deeper layer contains two leaf parts, each using #'pass-through
  ;; inject->main-schem->child-schem->leaf1->leaf2->child-schem->main-schem->
  ;; see test6.drawio
  (e/dispatch:reset-dispatcher)
  (let ((main-schem (e/schematic:make-schematic
                     :name "main"
                     :in-pins (e/pin-collection:from-list '(:main-schem-in))
                     :out-pins (e/pin-collection:from-list '(:main-schem-out)))))
    (let ((child-schem (e/schematic:make-schematic
                        :name "child schematic"
                        :in-pins (e/pin-collection:from-list '(:child-schem-in))
                        :out-pins (e/pin-collection:from-list '(:child-schem-out))))
          (leaf1 (e/leaf:make-leaf
                  :name "leaf1"
                  :reactor #'pass-through
                  :in-pins (e/pin-collection:from-list '(:in))
                  :out-pins (e/pin-collection:from-list '(:out))))   ;; leaf1 and leaf2 use "pass-through", so they must have the same output pin name :out
          (leaf2 (e/leaf:make-leaf
                  :name "leaf2"
                  :reactor #'pass-through
                  :in-pins (e/pin-collection:from-list '(:in))
                  :out-pins (e/pin-collection:from-list '(:out)))))
      (e/schematic:add-instance main-schem child-schem)
      (e/schematic:add-instance child-schem leaf1)
      (e/schematic:add-instance child-schem leaf2)
      
      (let ((child-schem-input-receiver (e/part-pin:make-pair child-schem (e/part:lookup-input-pin child-schem :child-schem-in)))
            (leaf1-receiver (e/part-pin:make-pair leaf1 (e/part:lookup-input-pin leaf1 :in)))
            (leaf2-receiver (e/part-pin:make-pair leaf2 (e/part:lookup-input-pin leaf2 :in)))
            (child-schem-output-receiver (e/part-pin:make-pair child-schem (e/part:lookup-output-pin child-schem :child-schem-out)))
            (main-schem-output-receiver (e/part-pin:make-pair main-schem (e/part:lookup-output-pin main-schem :main-schem-out))))
        (let ((wire-main-to-child (e/wire:make-wire :receivers (list child-schem-input-receiver)))
              (wire-child-to-leaf1 (e/wire:make-wire :receivers (list leaf1-receiver)))
              (wire-leaf1-to-leaf2 (e/wire:make-wire :receivers (list leaf2-receiver)))
              (wire-leaf2-to-child (e/wire:make-wire :receivers (list child-schem-output-receiver)))
              (wire-child-to-main (e/wire:make-wire :receivers (list main-schem-output-receiver))))

          ;; add 5 wires
          (e/schematic:add-self-input-wire main-schem (e/part:lookup-input-pin main-schem :main-schem-in) wire-main-to-child)
          (e/schematic:add-self-input-wire child-schem (e/part:lookup-input-pin child-schem :child-schem-in) wire-child-to-leaf1)
          (e/schematic:add-child-wire child-schem leaf1 (e/part:lookup-output-pin leaf1 :out) wire-leaf1-to-leaf2)
          (e/schematic:add-child-wire child-schem leaf2 (e/part:lookup-output-pin leaf2 :out) wire-leaf2-to-child)
          (e/schematic:add-child-wire main-schem child-schem (e/part:lookup-output-pin child-schem :child-schem-out) wire-child-to-main)

          (let ()
            ;; start
            (e/send:inject main-schem (e/message:make-message (e/part:lookup-input-pin main-schem :main-schem-in) "hello 6"))))))))

(defun test ()
  (format *standard-output* "~&running hello~%")
  (hello)
  (format *standard-output* "~&~%running hello2~%")
  (hello2)
  (format *standard-output* "~&~%running hello3~%")
  (hello3)
  (format *standard-output* "~&~%running hello4~%")
  (hello4)
  (format *standard-output* "~&~%running hello5~%")
  (hello5)
  (format *standard-output* "~&~%running hello6~%")
  (hello6))

;; code / callbacks

(defmethod receiver-display ((self e/part:part) (msg e/message:message))
  (declare (ignore self))

  (case (e/pin:as-symbol (e/message:pin msg))

    (:in
     (cl:format cl:*standard-output* "~&receiver-display output: ~S~%"
                (e/message:data msg)))

    (otherwise
     (error (format nil "unsupported message sent to e:display /~S/" msg)))))

(defmethod rewrite-message-with-my-output ((self e/part:part) (out-sym cl:symbol) (msg e/message:message))
  "change input pin to output pin in copy of message"
  (e/part:ensure-message-contains-valid-input-pin self msg)
  (let ((outmsg (e/message:clone-with-pin msg (e/part:lookup-output-pin self out-sym))))
    (e/part:ensure-message-contains-valid-output-pin self outmsg)
    outmsg))

(defmethod pass-through ((self e/part:part) (msg e/message:message))
  (e/send:send self (rewrite-message-with-my-output self :out msg)))

(defmethod start-sender ((self e/part:part))
  (e/send:send self (e/message:make-message (e/part:lookup-output-pin self :out) "Hello")))
     
(defmethod start-schematic-sender ((self e/schematic:schematic))
  (e/send:send self (e/message:make-message (e/part:lookup-output-pin self :schem-out) "Hello")))
     
