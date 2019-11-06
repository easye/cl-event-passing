(defsystem "cl-event-passing"
  :depends-on (loops)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3)
                                         (safety 3)
                                         (speed 0)))
                    (funcall next))
  :components ((:module "source"
                        :pathname "./"
                        :components ((:file "package")
                                     (:file "wire0" :depends-on ("package"))
                                     (:file "part0" :depends-on ("package")) ;; part is needed by pin-collection, but results in circular dependency if we don't split it out
                                     (:file "queue" :depends-on ("package" "message"))
                                     (:file "pin" :depends-on ("package"))
                                     (:file "pin-collection" :depends-on ("package" "pin" "part0"))
                                     (:file "message" :depends-on ("package" "pin"))
                                     (:file "part" :depends-on ("package" "message" "pin-collection"))
                                     (:file "schematic" :depends-on ("package" "part0" "part" "leaf" "wire-list" "message"))
                                     (:file "leaf" :depends-on ("package" "pin-collection"))
                                     (:file "send" :depends-on ("package" "message" "part0" "part"))
                                     (:file "receive" :depends-on ("package" "part0" "part" "message"))
                                     (:file "wire-list" :depends-on ("package" "wire0" "pin"))
                                     (:file "wire" :depends-on ("package" "wire0" "message" "part0" "part" "schematic"))
                                     (:file "pin-wire" :depends-on ("package" "pin" "wire"))
                                     (:file "part-pin" :depends-on ("package" "part0" "part" "pin"))
                                     (:file "dispatch" :depends-on ("package" "part0" "part" "message"))
                                     (:file "hello-world" :depends-on ("package" "schematic" "leaf"
                                                                       "pin-collection" "pin" "part-pin" "part0" "part"
                                                                       "wire0" "wire" "wire-list" "dispatch"))))))
