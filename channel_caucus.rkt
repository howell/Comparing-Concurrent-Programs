#lang racket
(require racket/set)


;; ???

(define caucus-log (make-logger 'caucus (current-logger)))

(define (log-caucus-evt evt . vals)
  (log-message caucus-log 'info (logger-name caucus-log) (apply format evt vals)))

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

;;;; CONVERSATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Publish Conversations
;; 1. Candidates publish their information to the Candidate Registry through a `candidate` struct sent to the Registry's channel
;; 2. Voters publish their information to the Voter Registry through a `voter` struct sent to the Registry's channel
;;
;; Subscribe Conversations
;; 1. Voters subscribe to the Candidate Registry to receive the most up-to-date list of available Candidates whenever a candidate registers.
;; 2. The Vote Leader subscribes to the Candidate Registry to receive the same information that voters do.
;; 
;; Message Conversations
;; 1. The Vote Leader sends a `request-msg` struct to the Voter Registry to receive the most up-to-date list of current voters.
;;
;; Voting Conversations
;; 1. The Voting Leader sends every Voter (through their `voter` struct) a request to vote through the `request-vote` struct, sending a List of valid candidate names.
;; 2. Voters reply by picking a name to vote for and sending the Vote Leader a `vote` struct.
;; 3. If one candidate has received a majority of votes, then that candidate is elected. If not, the least voted for candidate is removed, and voting begins again.
;;

;; Create the Candidate Registry thread and channel
(define (make-candidate-registry)
  (define registration-chan (make-channel))
  (define receive-roll-chan (make-channel))
  (thread
    (thunk
      (log-caucus-evt "The candidate registry is open for business!")
      (let loop ([candidates (set)]
                 [subscribers (set)])
        (sync
          (handle-evt
            registration-chan
            (match-lambda
              ;; a Candidate has registered!
              [(candidate name tax-rate) 
               (define new-candidates (set-add candidates (candidate name tax-rate)))
               (for ([subscriber (set->list subscribers)])
                 (channel-put subscriber (all-candidates new-candidates)))
               (loop new-candidates subscribers)]))
          (handle-evt
            receive-roll-chan
            (match-lambda
              ;; a channel has requested to be a Subscriber!
              [(subscribe chan)
               (channel-put chan (all-candidates candidates))
               (loop candidates (set-add subscribers chan))]))))))
  (values registration-chan receive-roll-chan))

;; Create a Candidate thread
;; Name Tax-Rate Candidate-Registry -> void
(define (make-candidate name tax-rate registration-chan)
  (thread 
    (thunk
      (log-caucus-evt "Candidate ~a has entered the race!" name)
      (channel-put registration-chan (candidate name tax-rate)))))

;; Create the Voter Registry thread and channel
(define (make-voter-registry)
  (define registration-chan (make-channel))
  (define receive-roll-chan (make-channel))
  (thread
    (thunk
      (log-caucus-evt "The Voter Registry has opened for business!")
      (let loop ([voters (set)])
        (sync
          (handle-evt
            registration-chan
            (match-lambda
              ;; A Voter has registered!
              [(voter name voter-chan) 
               (log-caucus-evt "Voter ~a has successfully registered!" name)
               (loop (set-add voters (voter name voter-chan)))]))
          (handle-evt
            receive-roll-chan
            (match-lambda
              ;; A request for voter data has been received!
              [(request-msg recv-chan) 
               (channel-put recv-chan (all-voters voters))
               (loop voters)]))))))
  (values registration-chan receive-roll-chan))

;; Make a Voter thread
(define (make-voter name rank-candidates voter-registry candidate-registry)
  (define receive-candidates-chan (make-channel))
  (define voting-chan (make-channel))
  (thread
    (thunk
      (log-caucus-evt "Voter ~a is registering!" name)
      (channel-put candidate-registry (subscribe receive-candidates-chan))
      (channel-put voter-registry (voter name voting-chan))
      (let loop ([candidates (set)]) ;; candidates just gets 're-assigned' by calling loop with a different argument, this is just to clarify type
        (sync
          (handle-evt
            receive-candidates-chan
            (match-lambda
              ;; A response from the Candidate Registry has been received!
              [(all-candidates curr-candidates) (loop curr-candidates)]))
          (handle-evt
            voting-chan
            (match-lambda
              ;; A request to vote has been received from the Vote Leader!
              [(request-vote available-candidates leader-chan)
               (define priorities (rank-candidates (set->list candidates)))
               (define voting-for
                 (for/first ([candidate (in-list priorities)]
                             #:when (member (candidate-name candidate) available-candidates))
                            (candidate-name candidate)))
               (channel-put leader-chan (vote name voting-for))
               (loop candidates)])))))))

;; Make the Vote Leader thread
(define (make-vote-leader candidate-registry voter-registry)
  (define retrieve-candidates-chan (make-channel))
  (define retrieve-voters-chan (make-channel))
  (define voting-chan (make-channel))
  (thread
    (thunk
      (sleep 1) ;; to make sure all other actors get situated first
      ;; TODO How are you going to deal with candidates leaving in the middle of voting?
      (log-caucus-evt "The Vote Leader is ready to run the caucus!")
      (channel-put candidate-registry (subscribe retrieve-candidates-chan))
      ;; DECISION: once you're subscribed to candidates, begin voting phase

      ;; Start a sequence of votes to determine an elected candidate
      ;; (Setof Candidate) -> Candidate
      (define (run-caucus candidates)
        (log-caucus-evt "The Vote Leader is beginning a new round of voting!")
        (define STATES '(SETUP VOTING))
        (let loop ([curr-state 'SETUP]
                   [voters (set)]) ;; this is just to demonstrate the type
          (match curr-state
            ['SETUP
             (channel-put voter-registry (request-msg retrieve-voters-chan))
             (sync
               (handle-evt
                 retrieve-voters-chan
                 (match-lambda
                   [(all-voters new-voters) (loop 'VOTING new-voters)])))]
            ['VOTING
             ;; TODO poorly formatted
             (for ([voter (set->list voters)])
               (channel-put (voter-voting-chan voter) 
                            (request-vote 
                              (map (λ (cand) (candidate-name cand)) (set->list candidates)) 
                              voting-chan)))
             (collect-votes voters candidates)])))

      ;; Determine winner of a round of voting or eliminate a candidate and move to the next one
      ;; (Setof Candidate) (Setof Voter) -> Candidate
      (define (collect-votes voters candidates)
        (let voting-loop ([votes (hash)])
          (sync
            (handle-evt
              voting-chan
              (match-lambda
                [(vote name candidate)
                 (log-caucus-evt "Voter ~a has voted for Candidate ~a!" name candidate)
                 (define new-votes (hash-update votes candidate add1 0))
                 (define num-votes (for/sum ([votes-for-cand (in-hash-values new-votes)]) votes-for-cand))
                 (cond
                   [(= num-votes (set-count voters))
                    (log-caucus-evt "Round of voting has ended! Time to tally the votes!")
                    (define front-runner (argmax (λ (cand) (hash-ref new-votes (candidate-name cand) 0)) (set->list candidates)))
                    (define their-votes (hash-ref new-votes (candidate-name front-runner) 0))
                    (cond
                      [(> their-votes (/ num-votes 2)) 
                       (log-caucus-evt "Candidate ~a has been elected!" (candidate-name front-runner))
                       front-runner]
                      [else
                        (define loser (argmin (λ (cand) (hash-ref new-votes (candidate-name cand) 0)) (set->list candidates)))
                        (log-caucus-evt "Candidate ~a has been eliminated from the race!" (candidate-name loser))
                        (define next-candidates (set-remove candidates loser))
                        (run-caucus next-candidates)])]
                   [else (voting-loop new-votes)])])))))
        
      (sync
        (handle-evt
          retrieve-candidates-chan
          (match-lambda
            [(all-candidates new-candidates)
             (define winner (run-caucus new-candidates))
             (printf "We have a winner: ~a!\n" (candidate-name winner))]))))))


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

(define-values (candidate-registration candidate-roll) (make-candidate-registry))
(define-values (voter-registration voter-roll) (make-voter-registry))

(make-candidate "Bernie" 50 candidate-registration)
(make-candidate "Biden" 25 candidate-registration)
(make-candidate "Tulsi" 6 candidate-registration)

(make-voter "ABC" (stupid-sort "Bernie") voter-registration candidate-roll)
(make-voter "DEF" (stupid-sort "Bernie") voter-registration candidate-roll)
(make-voter "GHI" (stupid-sort "Bernie") voter-registration candidate-roll)
(make-voter "JKL" (stupid-sort "Biden") voter-registration candidate-roll)
(make-voter "MNO" (stupid-sort "Biden") voter-registration candidate-roll)
(make-voter "PQR" (stupid-sort "Biden") voter-registration candidate-roll)
(make-voter "STU" (stupid-sort "Biden") voter-registration candidate-roll)
(make-voter "VWX" (stupid-sort "Biden") voter-registration candidate-roll)
(make-voter "YZZ" (stupid-sort "Biden") voter-registration candidate-roll)
;; (make-voter "CBA" (stupid-sort "Biden") voter-registration candidate-roll)
;; (make-voter "CAB" (stupid-sort "Biden") voter-registration candidate-roll)
(make-voter "111" (stupid-sort "Tulsi") voter-registration candidate-roll)
(make-voter "222" (stupid-sort "Tulsi") voter-registration candidate-roll)
(make-voter "333" (stupid-sort "Tulsi") voter-registration candidate-roll)
(make-voter "444" (stupid-sort "Tulsi") voter-registration candidate-roll)

(thread-wait (make-vote-leader candidate-roll voter-roll))
