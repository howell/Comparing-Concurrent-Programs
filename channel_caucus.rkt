#lang racket
(require racket/set)

;; TODO need to do timeout management of voters both here and in syndicate version

;; a Name is a string

;; a Tax-Rate is a number

;; a Chan is a channel

;; a Candidate is a (candidate Name Tax-Rate)
(struct candidate (name tax-rate) #:transparent)

;; a Voter is a (voter Name Chan)
(struct voter (name voting-chan) #:transparent)

;; a Subscribe is a (subscribe Chan)
(struct subscribe (chan) #:transparent)

;; a Request-Msg is a (request-msg Chan)
(struct request-msg (chan) #:transparent)

;; a Request-Vote is a (request-vote Chan)
(struct request-vote (candidates chan) #:transparent)

;; a Vote is a (vote Name Name)
(struct vote (name candidate) #:transparent)

;; an All-Candidates is a (all-candidates [Setof Candidate])
(struct all-candidates (candidates) #:transparent)

;; an All-Voters is a (all-voters [Setof Voter])
(struct all-voters (voters) #:transparent)

;;;; ENTITIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Candidates
;; 2. Candidate Registry
;; 3. Voters
;; 4. Voter Registry
;; 5. Vote Leader

;; Should I list the structs associated with each conversation here?

;;;; CONVERSATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUBLISH CONVERSATIONS
;; 1. Candidates announce themselves to the Candidate Registry
;;
;;
;;
;; SUBSCRIBE CONVERSATIONS
;; 1. Subscribers to the Candidate Registry will:
;;    a. Immediately receive a message with all known candidates
;;    b. Will be sent an Up-To-Date list of Candidates if that list changes
;; --> Subscribers to the Candidate Registry include: Voters, Vote Leader
;; 
;; MESSAGE CONVERSATIONS
;; 1. The Voter Registry will reply to a message requesting the snapshot of current voters with the voters currently registered

;; -> Candidate-Registry channel
(define (make-candidate-registry)
  (define registration-chan (make-channel))
  (thread
    (thunk
      (let loop ([candidates (set)]
                 [subscribers (set)])
        (sync
          (handle-evt
            registration-chan
            (match-lambda
              [(candidate name tax-rate)
               (define new-candidates (set-add candidates (candidate name tax-rate)))
               (for ([subscriber (set->list subscribers)])
                 (channel-put subscriber (all-candidates new-candidates)))
               (loop new-candidates subscribers)]
              [(subscribe chan)
               (channel-put chan (all-candidates candidates))
               (loop candidates (set-add subscribers chan))]))))))
  registration-chan)

;; Name Tax-Rate Candidate-Registry -> ...
(define (make-candidate name tax-rate registration-chan)
  ;; TODO is this unnecessary?
  (thread 
    (thunk
      (channel-put registration-chan (candidate name tax-rate)))))

(define (make-voter-registry)
  (define registration-chan (make-channel))
  (thread
    (thunk
      (let loop ([voters (set)])
        (sync
          (handle-evt
            registration-chan
            (match-lambda
              [(voter name voter-chan) (loop (set-add voters (voter name voter-chan)))]
              [(request-msg recv-chan) 
               (channel-put recv-chan (all-voters voters))
               (loop voters)]))))))
  registration-chan)

(define (make-voter name rank-candidates voter-registry candidate-registry)
  (define voter-chan (make-channel))
  (thread
    (thunk
      (channel-put candidate-registry (subscribe voter-chan))
      (channel-put voter-registry (voter name voter-chan))
      (let loop ([candidates (set)]) ;; candidates just gets 're-assigned' by calling loop with a different argument, this is just to clarify type
        (sync
          (handle-evt
            voter-chan
            (match-lambda
              [(all-candidates curr-candidates) (loop curr-candidates)]
              [(request-vote available-candidates leader-chan)
               (define priorities (rank-candidates (set->list candidates)))
               (define voting-for
                 (for/first ([candidate (in-list priorities)]
                             #:when (member (candidate-name candidate) available-candidates))
                            (candidate-name candidate)))
               (printf "ABABABABA\n")
               (channel-put leader-chan (vote name voting-for))
               (loop candidates)])))))))

(define (make-vote-leader candidate-registry voter-registry)
  (define leader-chan (make-channel))
  (thread
    (thunk
      (sleep 1) ;; to make sure all other actors get situated first
      ;; How are you going to deal with candidates leaving in the middle of voting?
      (channel-put candidate-registry (subscribe leader-chan))
      ;; maybe this should be done after receiving candidates?
      ;; DECISION: once you're subscribed to candidates, begin voting phase

      ;; Setof Candidate -> Elected Message
      (define (begin-voting candidates)
        (define STATES '(START SETUP VOTING))
        (let loop ([curr-state 'START]
                   [voters (set)]) ;; this is just to demonstrate the type
          (match curr-state
            ['START
             (channel-put voter-registry (request-msg leader-chan))
             (loop 'SETUP voters)]
            ['SETUP
             (sync
               (handle-evt
                 leader-chan
                 (match-lambda
                   [(all-voters new-voters) (loop 'VOTING new-voters)])))]
            ['VOTING
             (for ([voter (set->list voters)])
               (channel-put (voter-voting-chan voter) 
                            (request-vote 
                              (map (λ (cand) (candidate-name cand)) (set->list candidates)) 
                              leader-chan)))
             (let voting-loop ([votes (hash)])
               (sync
                 (handle-evt
                   leader-chan
                   (match-lambda
                     [(vote name candidate)
                      (define new-votes (hash-update votes candidate add1 0))
                      (define num-votes (for/sum ([votes-for-cand (in-hash-values new-votes)]) votes-for-cand))
                      (printf "new-votes: ~a\n" new-votes)
                      (cond
                        [(= num-votes (set-count voters))
                         (define front-runner (argmax (λ (cand) (hash-ref new-votes (candidate-name cand) 0)) (set->list candidates)))
                         (define their-votes (hash-ref new-votes (candidate-name front-runner) 0))
                         (cond
                           [(> their-votes (/ num-votes 2)) front-runner]
                           [else
                             (define loser (argmin (λ (n) (hash-ref new-votes (candidate-name n) 0)) (set->list candidates)))
                             (printf "Loser: ~a\n" loser)
                             (printf "new-votes: ~a\n" new-votes)
                             ;; I think I have types wrong somewhere
                             ;; set of structs vs (Listof) Name?
                             (define next-candidates (set-remove candidates loser))
                             (begin-voting next-candidates)])]
                        [else (voting-loop new-votes)])]))))])))

      (sync
        (handle-evt
          leader-chan
          (match-lambda
            [(all-candidates new-candidates)
             (define winner (begin-voting new-candidates))
             (printf "We have a winner: ~a!\n" winner)]))))))


;; A subscriber used to print information for testing
(define (make-dummy-subscriber pub-sub-chan)
  (define updates (make-channel))
  (thread
    (thunk
      (channel-put pub-sub-chan (subscribe updates))
      (let loop ()
        (sync
          (handle-evt
            updates
            (λ (evt) (printf "Dummy Subscription: ~a\n" evt) (loop))))))))

;; a receiver used to print information from message-based conversations for testing
(define (make-dummy-receiver message-chan)
  (define messages (make-channel))
  (thread
    (thunk
      (channel-put message-chan (request-msg messages))
      (let loop ()
        (sync
          (handle-evt
          messages
          (λ (evt) (printf "Dummy Message: ~a\n" evt) (loop))))))))

(define (stupid-sort cand-name)
  (λ (candidates)
     (define candidate? (findf (λ (cand) (string=? cand-name (candidate-name cand))) candidates))
     (if candidate?
       (cons candidate? (remove candidate? candidates))
       (candidates))))

;;;;;;;;;;;; EXECUTION ;;;;;;;;;;;;

(define candidate-registry (make-candidate-registry))

(make-candidate "Bernie" 50 candidate-registry)
(make-candidate "Biden" 25 candidate-registry)
(make-candidate "Tulsi" 6 candidate-registry)

(define voter-registry (make-voter-registry))

(make-voter "ABC" (stupid-sort "Bernie") voter-registry candidate-registry)
(make-voter "DEF" (stupid-sort "Bernie") voter-registry candidate-registry)
(make-voter "GHI" (stupid-sort "Bernie") voter-registry candidate-registry)
(make-voter "JKL" (stupid-sort "Biden") voter-registry candidate-registry)
(make-voter "MNO" (stupid-sort "Biden") voter-registry candidate-registry)
(make-voter "PQR" (stupid-sort "Biden") voter-registry candidate-registry)
(make-voter "STU" (stupid-sort "Biden") voter-registry candidate-registry)
(make-voter "VWX" (stupid-sort "Biden") voter-registry candidate-registry)
(make-voter "YZZ" (stupid-sort "Biden") voter-registry candidate-registry)
(make-voter "111" (stupid-sort "Tulsi") voter-registry candidate-registry)
(make-voter "222" (stupid-sort "Tulsi") voter-registry candidate-registry)
(make-voter "333" (stupid-sort "Tulsi") voter-registry candidate-registry)
(make-voter "444" (stupid-sort "Tulsi") voter-registry candidate-registry)


(make-vote-leader candidate-registry voter-registry)

(sleep 10)

