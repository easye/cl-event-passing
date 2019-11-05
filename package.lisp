(defpackage :cl-event-passing
  (:use :cl)
  (:export
   #:hello))

(defpackage :e/pin-collection
  (:use :cl)
  (:export
   #:make-empty-collection
   #:pin-collection
   #:from-list
   #:as-list
   #:ensure-member
   #:lookup-pin))

(defpackage :e/part
  (:use :cl)
  (:export
   #:part
   #:parent
   #:set-parent
   #:fetch-name ;; for debug
   #:push-input
   #:pop-input
   #:has-input-p
   #:has-output-p
   #:push-output
   #:react
   #:has-first-time-p
   #:first-time-function
   #:lookup-input-pin
   #:lookup-output-pin
   #:make-output-queue-empty
   #:outqueue-as-list
   #:output-pins-as-list
   #:ensure-message-contains-valid-input-pin
   #:ensure-message-contains-valid-output-pin))

(defpackage :e/schematic
  (:use :cl)
  (:export
   #:make-schematic
   #:schematic
   #:wiring
   #:instances
   #:add-instance
   #:add-child-wire
   #:add-self-input-wire
   #:push-input
   #:find-wire-for-pin-inside-schematic))

(defpackage :e/leaf
  (:use :cl)
  (:export
   #:leaf
   #:make-leaf))

(defpackage :e/message
  (:use :cl)
  (:export
   #:make-message
   #:message
   #:pin
   #:data
   #:clone-with-pin))
   
(defpackage :e/dispatch
  (:use :cl)
  (:export
   #:clear-parts
   #:Start-Dispatcher
   #:remember-part
   #:reset-dispatcher))

(defpackage :e/wire-list
  (:use :cl)
  (:export
   #:make-wire-list
   #:wire-list
   #:add-wire))

(defpackage :e/wire
  (:use :cl)
  (:export
   #:make-wire
   #:wire
   #:ins
   #:outs
   #:outs-as-list
   #:deliver-message
   #:member-of-inputs-p))

(defpackage :e/part-pin
  (:use :cl)
  (:export
   #:pair
   #:make-pair
   #:part
   #:pin))

(defpackage :e/send
  (:use :cl)
  (:export
   #:send
   #:inject))

(defpackage :e/receive
  (:use :cl)
  (:export
   #:respond))

(defpackage :e/queue
  (:use :cl)
  (:export
   #:queue
   #:make-queue
   #:make-empty-queue
   #:q-push
   #:q-pop
   #:empty-p
   #:as-list))

(defpackage :e/pin
  (:use :cl)
  (:export
   #:pin
   #:make-pin
   #:pin-equal
   #:as-symbol))

(defpackage :e/pin-wire
  (:use :cl)
  (:export
   #:pin-wire
   #:make-pin-wire))
