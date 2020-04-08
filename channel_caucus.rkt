#lang racket

;; start by adding candidates and a leader who is able to consume candidate announcements

;; a Name is a String

(struct candidate (name) #:transparent)
(struct voter (name vote-chan) #:transparent)

;; There is a presence-oriented conversation where candidates announce themselves to their voting leader

;; there is a presence-oriented conversation where voters announce themselves to their voting leader

(define (make-candidate name leader) 
  (thread (thunk (channel-put leader (candidate name)))))


(define (make-candidate-registry . candidates voter-registry)
  (define candidate-registry-channel (make-channel))
  (define candidate-structs (map cdr candidtes))
  (define candidate-chans   (map car candidtes))
  (thread 
    (thunk
      (let loop ()
        (sync
          (handle-evt
            candidate-registry-channel
            (λ
              (match
                [(get-candidates recv-chan) 
                 (channel-put recv-chan candidate-structs)])
              (loop))))))))

(define (make-voter-registry . voters)
  (define voter-registry-channel (make-channel))
  (define voter-structs (map cdr candidtes))
  (define voter-chans   (map car candidtes))
  ;; need to initialize all voters with the voter registry
  (thread
    (thunk
      (let loop ()
        (sync
          (handle-evt
            voter-registry-channel
            (match-lambda
              [(get-voters recv-chan)
               (channel-put recv-chan voter-structs)]
              [(round id candidates recv-chan)
               (for ([voter-chan voter-chans])
                 (channel-put voter-chan (round id candidates)))
               (let vote-loop ([votes '()])
                 (if (= (length votes) (length voters))
                   (channel-put recv-chan (all-votes votes))
                   (sync
                     (handle-evt
                       voter-registry-channel
                       (match-lambda
                         [(vote candidate-name)
                          (vote-loop (cons candidate-name votes))])))))])))))))


(define (make-voter name leader)
  (define voting-channel (make-channel))
  (thread (thunk (channel-put leader (voter name voting-channel)))))

(define (make-leader)
  (define leader-channel (make-channel))
  (thread
    (thunk
      (let loop ([candidates '()]
                 [voters '()])
        (printf "we entered here\n")
        (sync
          (handle-evt
            leader-channel
            (match-lambda
              ;; fix for tax-rate?
              [(candidate name)
               (printf "candidate made an announcement: ~a\n" name)
               (printf "total candidates: ~a\n" candidates)
               (loop (cons name candidates) voters)]
              [(voter name vote-chan)
               (printf "voter has registered: ~a\n" name)
               (loop candidates (cons (voter name vote-chan) voters))]
                ))))))
  leader-channel)

(define the-leader-channel (make-leader))


(make-candidate "Bernie" the-leader-channel)
(make-candidate "Biden" the-leader-channel)
(make-candidate "Tulsi" the-leader-channel)

(make-voter "Mitch" the-leader-channel)
(make-voter "Sam" the-leader-channel)

(sleep 1)
