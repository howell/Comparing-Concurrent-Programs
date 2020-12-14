#lang syndicate

;; Written by Sam Caldwell

(provide (struct-out tcp-in-datum))

(require syndicate/drivers/tcp2)

(message-struct tcp-in-datum (id v))

;; clients request datum input on TCP connection identified by ID by expressing interest
;;   (observe (tcp-in-datum ID _))
;;
;; in response, the driver will begin listening to tcp-in messages for connection ID, and once an entire datum V is read, send a message (tcp-in-datum ID V)

;; InputPort ID -> ThreadID
(define (start-datum-reader-background-thread in id)
  (thread
   (lambda ()
     (let loop ()
       (define datum (read in))
       (send-ground-message (tcp-in-datum id datum))
       (loop)))))

(spawn #:name 'tcp2-datum-reader-factory
  (during/spawn (observe (tcp-in-datum $id _))
    #:name (list 'tcp2-datum-reader id)
    (define-values (in out) (make-pipe))
    (define background-thread (start-datum-reader-background-thread in id))
    (on-stop (kill-thread background-thread))
    (on (message (tcp-in id $bs))
        (write-bytes bs out))
    (on (message (inbound (tcp-in-datum id $v)))
        (printf "Value: ~a\n" v)
        (send! (tcp-in-datum id v)))))
