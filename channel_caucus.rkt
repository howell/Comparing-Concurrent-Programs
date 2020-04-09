#lang racket

;; start by adding candidates and a leader who is able to consume candidate announcements

;; a Name is a String

(struct candidate (name tax-rate) #:transparent)
(struct voter (name vote-chan) #:transparent)
(struct request-data (recv-chan) #:transparent)
(struct round (id candidates recv-chan) #:transparent)
(struct vote (name round-id candidate) #:transparent)
(struct all-candidates (candidates) #:transparent)
(struct all-voters (voters) #:transparent)

;; There is a presence-oriented conversation where candidates announce themselves to their voting leader

;; there is a presence-oriented conversation where voters announce themselves to their voting leader

;; a little weird, but right now there's no need for this to do anything else, because of the candidate registry
(define (make-candidate name tax-rate) (candidate name tax-rate))

(define (make-candidate-registry . candidate-structs)
  (define candidate-registry-channel (make-channel))
  (thread 
    (thunk
      (let loop ()
        (sync
          (handle-evt
            candidate-registry-channel
            (λ (evt)
              (match evt
                [(request-data recv-chan) 
                 (channel-put recv-chan (all-candidates candidate-structs))])
              (loop)))))))
  candidate-registry-channel)

(define (make-voter-registry candidate-registry . voters)
  (define voter-registry-channel (make-channel))
  (define voter-structs (map car voters))
  (define voter-chans   (map cdr voters))
  (for ([voter-chan voter-chans]) (channel-put voter-chan (registration-confirmation voter-registry-channel)))
  (channel-put candidate-registry (request-data voter-registry-channel))
  (thread
    (thunk
      (let loop ()
        (sync
          (handle-evt
            voter-registry-channel
            (match-lambda
              [(all-candidates candidate-structs) 
               (for ([voter-chan voter-chans]) 
                 (channel-put voter-chan (eligible-candidates candidate-structs)))
               (loop)]
              [(request-data recv-chan)
               (channel-put recv-chan (all-voters voter-structs))
               (loop)]))))))
  voter-registry-channel)
  
(define (make-voter name)
  (define voter-channel (make-channel))
  (thread
    (thunk
      (let loop ([candidates '()]
                 [voter-registry null])
        (sync
          (handle-evt
            voter-channel
            (match-lambda
              [(registration-confirmation voter-registry-chan) (loop candidates voter-registry-chan)]
              [(eligible-candidates candidate-structs) (loop candidate-structs voter-registry)])))))))


(define (make-leader candidate-registry voter-registry)
  (define leader-channel (make-channel))
  (channel-put candidate-registry (request-data leader-channel))
  (channel-put voter-registry (request-data leader-channel))
  (thread
    (thunk
      (let loop ([candidates '()]
                 [voters '()])
        (sync
          (handle-evt
            leader-channel
            (match-lambda
              ;; endpoint: receiving candidates from candidate registry
              [(all-candidates candidate-structs)
               (printf "we got candidates! ~a\n" candidate-structs)
               (loop candidate-structs voters)]
              [(all-voters voter-structs)
               (printf "we got voters! ~a\n" voter-structs)
               (loop candidates voter-structs)]))))))
  leader-channel)


(define candidate-registry 
  (make-candidate-registry
    (make-candidate "Bernie" 35)
    (make-candidate "Biden" 50)
    (make-candidate "Tulsi" 2)))

(define voter-registry 
  (make-voter-registry 
    candidate-registry
    (make-voter "Mitch")
    (make-voter "Sam")))

(define the-leader-channel (make-leader candidate-registry voter-registry))

(sleep 1)
