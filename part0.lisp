(in-package :e/part)

(defclass part ()
  ((inqueue :accessor inqueue :initform (e/queue:make-queue))
   (outqueue :accessor outqueue :initform (e/queue:make-queue))
   (busy :accessor busy :initform nil) ;; NIL / T is part is "busy" (esp, schematic parts - shematics are busy if any child is busy)
   (in-pins :accessor in-pins :initarg :in-pins)  ;; list of input pins for this part (for checking legal receives)
   (out-pins :accessor out-pins :initarg :out-pins) ;; list of output pins for this part (for checking legal sends)
   (parent :accessor parent :initform nil :initarg :parent)
   (reactor :accessor reactor :initform nil :initarg :reactor) ;; reactor function / callback (self msg)
   (first-time-function :accessor first-time-function :initarg :first-time :initform nil) ;; function((self part))
   (name :accessor name :initarg :name)) ;; for debugging
  (:default-initargs
   :in-pins (e/pin-collection:make-empty-collection)
   :out-pins (e/pin-collection:make-empty-collection)
   :name ""))

   ;; Essay: Outqueue might seem superfluous, but don't optimize it away - it is
   ;; one of the essential elements of the act of: snipping dependencies and making parts
   ;; be truly *concurrent* (asynchronous).  Parts MUST NOT refer to their
   ;; peers (this removes possible dependencies).  Parts are WIRED to peers
   ;; via the wiring list contained in their parent (always a schematic or NIL).  Parts
   ;; can be wired only to peer parts contained in the same schematic
   ;; (hierarchy ==> simplicity ==> scalability) or to output pins
   ;; of the parent schematic

