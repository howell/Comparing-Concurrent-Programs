#lang racket

;; start by adding candidates and a leader who is able to consume candidate announcements

;; a Name is a String

(struct candidate (name) #:transparent)
(struct exit-loop () #:transparent)

;; There is a presence-oriented conversation where candidates announce themselves to their voting leader

(define (make-candidate name leader) 
  (thread (thunk (channel-put leader (candidate name)))))

(define (make-leader)
  (define leader-channel (make-channel))
  (thread
    (thunk
      (let loop ([candidates '()])
        (printf "we entered here\n")
        (sync
          (handle-evt
            leader-channel
            (lambda (k)
              (cond 
                [(candidate? k) 
                 (printf "candidate made an announcement: ~a\n" (candidate-name k))
                 (printf "total candidates: ~a\n" candidates)
                 (loop (cons (candidate-name k) candidates))]
                [else (printf "candidates: ~a\n" candidates)])))))))
            #|(match-lambda
              [(candidate name) 
               (printf "candidate made an announcement: ~a\n" name)
               (loop (cons name candidates))]
              [(exit-loop) (printf "candidates: ~a\n" candidates)]))))))|#
  leader-channel)

(define the-leader-channel (make-leader))
(make-candidate "Bernie" the-leader-channel)
(make-candidate "Biden" the-leader-channel)
(make-candidate "Tulsi" the-leader-channel)

(sleep 1)
