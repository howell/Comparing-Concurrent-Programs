#lang racket
(require racket/set)

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
              [(all-candidates curr-candidates) (loop curr-candidates)])))))))



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
            (λ (evt) (printf "~a\n" evt) (loop))))))))

;; TODO make dummy messager?

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

(define voter-registry (make-voter-registry))

(make-dummy-subscriber candidate-registry)

(make-voter "ABC" (stupid-sort "Bernie") voter-registry candidate-registry)
(make-voter "DEF" (stupid-sort "Bernie") voter-registry candidate-registry)
(make-voter "GHI" (stupid-sort "Bernie") voter-registry candidate-registry)

(sleep 1)

