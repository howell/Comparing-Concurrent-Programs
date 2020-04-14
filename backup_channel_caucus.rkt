(struct candidate (name tax-rate) #:transparent)
(struct voter (name) #:transparent)
(struct request-data (recv-chan) #:transparent)
(struct round (id candidates recv-chan) #:transparent)
(struct vote (name round-id candidate) #:transparent)
(struct all-candidates (candidates) #:transparent)
(struct all-voters (voters) #:transparent)
(struct registration-confirmation (registry-chan) #:transparent)
(struct eligible-candidates (cand-structs) #:transparent)
(struct results (votes) #:transparent)
(struct vote-request (cand-names round-id) #:transparent)

(define LEADER_STATES '(idle started waiting finished))

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
               (loop)]
              [(round id cand-names recv-chan)
               (for ([voter-chan voter-chans])
                 (channel-put voter-chan (vote-request cand-names id)))
               (let voting-loop ([votes '()])
                 (sync
                   (handle-evt
                     voter-registry-channel
                     (λ (evt)
                        (cond
                          [(vote? evt)
                           (if (= (+ (length votes) 1) (length voters))
                             (channel-put recv-chan (votes (cons evt votes)))
                             (loop (cons evt votes)))])))))]))))))
  voter-registry-channel)
  
(define (make-voter name rank-candidates)
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
              [(eligible-candidates candidate-structs) (loop (map candidate-name (rank-candidates candidate-structs)) voter-registry)]
              [(vote-request cand-names round-id)
               (define voting-for 
                 (for/first ([candidate (in-list candidates)]
                            #:when (member candidate candidates))
                            candidate))
               (channel-put voter-registry (vote round-id voting-for))
               (loop candidates voter-registry)]))))))
  (cons (voter name) voter-channel))

;; New logic:
;; 1. if the length of candidates and voters are both non-null -> start
;; 2. if not, go into regular sync

;; list of candidates should just be a list of names, not the structs
(define (make-leader candidate-registry voter-registry)
  (define leader-channel (make-channel))
  (channel-put candidate-registry (request-data leader-channel))
  (channel-put voter-registry (request-data leader-channel))
  (thread
    (thunk
      (let loop ([candidates '()]
                 [voters '()]
                 [leader-state 'idle])
        (match leader-state
          ['idle
            (when (and (>= (length candidates) 0) (>= (length voters) 0)) (loop candidates voters 'started))
           (sync-wrapper
             leader-channel
             (match-lambda
               [(all-candidates candidate-structs)
                (printf "we got candidates! ~a\n" candidate-structs)
                (loop (map candidate-name candidates) voters leader-state)]
               [(all-voters voter-structs)
                (printf "we got voters! ~a\n" voter-structs)
                (loop candidates voter-structs leader-state)]))]
          ['started
           (define round-id (gensym 'round))
           (channel-put (round round-id candidates leader-channel))
           (sync-wrapper
             leader-channel
             (match-lambda
               [(results votes)
                (define tallies (make-hash))
                (for ([vote votes])
                  (hash-update! tallies (vote-candidate vote) add1 0))
                (define front-runner (argmax (λ (n) (hash-ref tallies n 0)) candidates))
                (define their-votes (hash-ref tallies front-runner 0))
                (cond
                  [(> their-votes (/ (length voters) 2))
                   ;; maybe a winner should be embedded in the state as well?
                   (printf "The winner is: ~a!\n" front-runner)
                   (loop (candidates voters 'finished))]
                  [else
                   (define loser (argmin (λ (n) (hash-ref tallies n 0)) candidates))
                   (define next-candidates (remove loser candidates))
                   (loop (next-candidates voters 'started))])]))]
          ['finished #f]))))
  leader-channel)

(define (sync-wrapper channel func)
  (sync
    (handle-evt
      channel
      func)))

(define candidate-registry 
  (make-candidate-registry
    (make-candidate "Bernie" 35)
    (make-candidate "Biden" 50)
    (make-candidate "Tulsi" 2)))

(define (stupid-sort cand-name)
  (λ (candidates)
     (define candidate? (findf (λ (cand) (string=? cand-name (candidate-name cand))) candidates))
     (if candidate?
       (cons candidate? (remove candidate? candidates))
       (candidates))))

(define voter-registry 
  (make-voter-registry 
    candidate-registry
    (make-voter "Mitch" (stupid-sort "Bernie"))
    (make-voter "Sam" (stupid-sort "Bernie"))))

(define the-leader-channel (make-leader candidate-registry voter-registry))

(sleep 1)

