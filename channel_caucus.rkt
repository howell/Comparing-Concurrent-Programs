#lang racket
(require racket/set)

(define caucus-log (make-logger 'caucus (current-logger)))

(define (log-caucus-evt evt . vals)
  (log-message caucus-log 'info (logger-name caucus-log) (apply format evt vals)))

;; TODO need to do timeout management of voters both here and in syndicate version

;; a Name is a string

;; a Tax-Rate is a number

;; a Threshold is a number

;; a Chan is a channel

;; a Candidate is a (candidate Name Tax-Rate Chan)
(struct candidate (name tax-rate results-chan) #:transparent)

;; a DropOut is a (drop-out Name)
(struct drop-out (name) #:transparent)

(struct loser (name) #:transparent)

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

;; a Tally is a (tally (Hashof Name . number))
(struct tally (votes) #:transparent)

;; an All-Candidates is a (all-candidates [Setof Candidate])
(struct all-candidates (candidates) #:transparent)

;; an All-Voters is a (all-voters [Setof Voter])
(struct all-voters (voters) #:transparent)

;;;; ASSUMPTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Voters don't try voting for multiple candidates
;; 3. Voters never try voting for candidates that are no longer available for voting
;; 4. Voters never leave early (with an announcement)
;; 5. Voters never leave early (without an announcement)
;; 6. Voters never try joining late (how is this even expressible...?)
;; 7. Voters never try to vote if they haven't registered (also not expressible...?)
;; 

;;;; FEATURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Multiple caucuses in a region!
;; 

;;;; ENTITIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Candidates        
;; 2. Candidate Registry
;; 3. Voters            
;; 4. Voter Registry    
;; 5. Vote Leader       
;; 

;;;; CONVERSATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Publish Conversations
;; 1. Candidates publish their information to the Candidate Registry through a `candidate` struct sent to the Registry's channel
;; 2. Voters publish their information to the Voter Registry through a `voter` struct sent to the Registry's channel
;; 3. Candidates can remove themselves from eligibility by sending a `drop-out` struct to the Candidate Registry
;;     -> This occurs when the candidate receives a number of votes below the candidate's threshold for staying in the race.
;;
;; Subscribe Conversations
;; 1. Voters subscribe to the Candidate Registry to receive the most up-to-date list of available Candidates whenever a candidate registers.
;; 2. The Vote Leader subscribes to the Candidate Registry to receive the same information that voters do.
;; 
;; Message Conversations
;; 1. The Vote Leader sends a `request-msg` struct to the Voter Registry to receive the most up-to-date list of current voters.
;; 2. The Vote Leader sends a `request-msg` struct to the Candidate Registry to receive the most up-to-date list of available candidates.
;;
;; Voting Conversations
;; 1. The Voting Leader sends every Voter (through their `voter` struct) a request to vote through the `request-vote` struct, sending a List of valid candidate names.
;; 2. Voters reply by picking a name to vote for and sending the Vote Leader a `vote` struct.
;; 3. If one candidate has received a majority of votes, then that candidate is elected. If not, the least voted for candidate is removed, and voting begins again.
;; 4. At the end of every round of voting, the tally of votes is sent to every Candidate.
;;

;;;; HELPERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; filters out all candidates that appear in the set
;; (Listof Candidate) (Setof Candidate) -> (Listof Candidate)
(define (filter-candidates candidates blacklist)
  (filter (λ (cand) (not (set-member? blacklist (candidate-name cand)))) candidates))


;; Create the Candidate Registry thread and channel
(define (make-candidate-registry)
  (define registration-chan (make-channel))
  (define receive-roll-chan (make-channel))
  (thread
    (thunk
      (log-caucus-evt "The candidate registry is open for business!")

      ;; TODO purpose statement + signature
      (define (update-subscribers subscribers candidates)
        (for ([subscriber (set->list subscribers)])
          (channel-put subscriber (all-candidates candidates))))

      (let loop ([candidates (set)]
                 [all-cand-names (set)]
                 [subscribers (set)])
        (sync
          (handle-evt
            registration-chan
            (match-lambda
              ;; a Candidate has registered!
              [(candidate name tax-rate results-chan) 
               (when (set-member? all-cand-names name) (loop candidates all-cand-names subscribers))
               (define new-candidates (set-add candidates (candidate name tax-rate results-chan)))
               (define new-names (set-add all-cand-names name))
               (update-subscribers subscribers new-candidates)
               (loop new-candidates new-names subscribers)]
              ;; a Candidate has dropped out!
              [(drop-out name)
               (define cand (for/first ([cand-struct (set->list candidates)]
                                       #:when (string=? name (candidate-name cand-struct)))
                                       cand-struct))
               (when (not cand) (loop candidates all-cand-names subscribers))
               (define new-candidates (list->set (filter-candidates (set->list candidates) (set (candidate-name cand)))))
               (update-subscribers subscribers new-candidates)
               (log-caucus-evt "Candidate ~a has successfully dropped out of the race!" (candidate-name cand))
               (loop new-candidates all-cand-names subscribers)]))

          (handle-evt
            receive-roll-chan
            (match-lambda
              ;; a channel has requested to be a Subscriber!
              [(subscribe chan)
               (channel-put chan (all-candidates candidates))
               (loop candidates all-cand-names (set-add subscribers chan))]
              ;; a channel has requested a snapshot of the current Candidates!
              [(request-msg chan)
               (channel-put chan (all-candidates candidates))
               (loop candidates all-cand-names subscribers)]))))))
  (values registration-chan receive-roll-chan))

;; Create a Candidate thread
;; Name Tax-Rate Candidate-Registry -> void
(define (make-candidate name tax-rate threshold registration-chan)
  (define results-chan (make-channel))
  (define cand-struct (candidate name tax-rate results-chan))
  (thread 
    (thunk
      (log-caucus-evt "Candidate ~a has entered the race!" name)
      (channel-put registration-chan cand-struct)
      (let loop ([in-the-race #t])
        (digest-results cand-struct threshold in-the-race registration-chan loop)))))

(define (make-stubborn-candidate name tax-rate threshold registration-chan)
  (define results-chan (make-channel))
  (define cand-struct (candidate name tax-rate results-chan))
  (thread
    (thunk
      (log-caucus-evt "Stubborn Candidate ~a has entered the race!" name)
      (channel-put registration-chan cand-struct)
      (let loop ([in-the-race #t])
        (when (not in-the-race) 
          (channel-put registration-chan cand-struct)
          (log-caucus-evt "Stubborn Candidate ~a is trying to re-enter the race!" name)
          (loop #t))
        (digest-results cand-struct threshold in-the-race registration-chan loop)))))

;; receive and handle votes from latest election round
;; Candidate Threshold Boolean Chan (Boolean -> void) -> void
(define (digest-results cand threshold in-the-race reg-chan loop-func)
  (define name (candidate-name cand))
  (define results-chan (candidate-results-chan cand))

  (define msg (channel-get results-chan))
  (match msg
    [(tally votes)
     (cond
      [(and in-the-race (< (hash-ref votes name 0) threshold))
       (channel-put reg-chan (drop-out name))
       (log-caucus-evt "Candidate ~a has submitted a request to drop out of the race!" name)
       (loop-func #f)]
      [else (loop-func #t)])]
    [(loser name)
     (loop-func #f)]))

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
  (define normal-voting
    (λ (existing-candidates available-candidates leader-chan)
       (define priorities (rank-candidates (set->list existing-candidates)))
       (define voting-for
         (for/first ([candidate (in-list priorities)]
                     #:when (member (candidate-name candidate) available-candidates))
                    (candidate-name candidate)))
       (channel-put leader-chan (vote name voting-for))))

  (voter-skeleton name rank-candidates normal-voting voter-registry candidate-registry))

  #|
  (define receive-candidates-chan (make-channel))
  (define voting-chan (make-channel))
  (thread
    (thunk
      (log-caucus-evt "Voter ~a is registering!" name)
      (channel-put candidate-registry (subscribe receive-candidates-chan))
      (channel-put voter-registry (voter name voting-chan))
      (let loop ([candidates (set)])
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
               (loop candidates)])))))))|#

(define (voter-skeleton name rank-candidates voting-procedure voter-registry candidate-registry)
  (define receive-candidates-chan (make-channel))
  (define voting-chan (make-channel))
  (thread
    (thunk
      (log-caucus-evt "Voter ~a is registering!" name)
      (channel-put candidate-registry (subscribe receive-candidates-chan))
      (channel-put voter-registry (voter name voting-chan))
      (let loop ([candidates (set)])
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
               (voting-procedure candidates available-candidates leader-chan)
               (loop candidates)])))))))

(define (make-greedy-voter name rank-candidates voter-registry candidate-registry)
  (define greedy-voting 
    (λ (existing-candidates available-candidates leader-chan)
         (define priorities (rank-candidates (set->list existing-candidates)))
         (define voting-for
           (for/first ([candidate (in-list priorities)]
                       #:when (member (candidate-name candidate) available-candidates))
                      (candidate-name candidate)))
         (define second-vote
           (for/first ([candidate (in-list priorities)]
                       #:when (and (member (candidate-name candidate) available-candidates) (not (string=? (candidate-name candidate) voting-for))))
                       (candidate-name candidate)))
         (channel-put leader-chan (vote name voting-for))
         (channel-put leader-chan (vote name (if second-vote second-vote voting-for)))))

  (voter-skeleton name rank-candidates greedy-voting voter-registry candidate-registry))

;; Make the Vote Leader thread
(define (make-vote-leader candidate-registry voter-registry)
  (define retrieve-candidates-chan (make-channel))
  (define retrieve-voters-chan (make-channel))
  (define voting-chan (make-channel))
  (thread
    (thunk
      (sleep 1) ;; to make sure all other actors get situated first
      (log-caucus-evt "The Vote Leader is ready to run the caucus!")
      ;; DECISION: once you're subscribed to candidates, begin voting phase

      ;; TODO ASSUMPTION: there are always both voters and candidates.


      ;; Start a sequence of votes to determine an elected candidate
      ;; (Setof Name) -> Candidate
      (define (run-caucus removed-candidates)
        (log-caucus-evt "The Vote Leader is beginning a new round of voting!")

        (channel-put candidate-registry (request-msg retrieve-candidates-chan))
        (channel-put voter-registry (request-msg retrieve-voters-chan))

        (define STATES '(SETUP VOTING))

        ;; Determine if the vote leader is ready to transition to the next state
        ;; (Setof Voter) (Setof Candidate) -> STATE
        (define (next-state voters candidates)
          (if (and (not (set-empty? voters)) (not (set-empty? candidates))) 'VOTING 'SETUP))

        ;; Transition (loop) on the next state for the vote leader
        ;; (Setof Voter) (Setof Candidate) (V C -> C) -> C
        (define (transition-states voters candidates loop)
          (loop (next-state voters candidates) voters candidates))

        (let loop ([curr-state 'SETUP]
                   [voters (set)]
                   [candidates (set)]) ;; this is just to demonstrate the type
          (match curr-state
            ['SETUP
             (sync
               (handle-evt
                 retrieve-voters-chan
                 (match-lambda
                   [(all-voters new-voters)
                    (transition-states new-voters candidates loop)]))
               (handle-evt
                 retrieve-candidates-chan
                 (match-lambda
                   [(all-candidates new-candidates)
                    (define eligible-candidates (list->set (filter-candidates (set->list new-candidates) removed-candidates)))
                    (transition-states voters eligible-candidates loop)])))]
            ['VOTING
             (for ([voter (set->list voters)])
               (channel-put (voter-voting-chan voter) 
                            (request-vote 
                              (map (λ (cand) (candidate-name cand)) (set->list candidates)) 
                              voting-chan)))
             (collect-votes voters candidates removed-candidates)])))

      ;; Determine winner of a round of voting or eliminate a candidate and move to the next one
      ;; (Setof Candidate) (Setof Voter) -> Candidate
      (define (collect-votes voters candidates removed-candidates)
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
                        (define losing-cand (argmin (λ (cand) (hash-ref new-votes (candidate-name cand) 0)) (set->list candidates)))
                        (for ([cand-struct (set->list candidates)]) 
                          (channel-put (candidate-results-chan cand-struct) (tally new-votes)))
                        (for/first ([cand-struct (set->list candidates)]
                                    #:when (string=? (candidate-name losing-cand) (candidate-name cand-struct)))
                                   (channel-put (candidate-results-chan cand-struct) (loser (candidate-name cand-struct))))
                        (log-caucus-evt "Candidate ~a has been eliminated from the race!" (candidate-name losing-cand))
                        (define next-candidates (set-remove candidates losing-cand))
                        (run-caucus (set-add removed-candidates (candidate-name losing-cand)))])]
                   [else (voting-loop new-votes)])])))))
        
      (define winner (run-caucus (set)))
      (printf "We have a winner: ~a!\n" (candidate-name winner)))))


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

(define (stupid-sort . cand-names)
  (define (compare-names first-cand second-cand) (string<? (candidate-name first-cand) (candidate-name second-cand)))

  (λ (candidates)
     (foldr 
       (λ (cand-name cands)
          (define candidate? (findf (λ (cand) (string=? cand-name (candidate-name cand))) cands))
          (if candidate?
            (cons candidate? (remove candidate? cands))
            cands))
       (sort candidates compare-names)
       cand-names)))

;;;;;;;;;;;; EXECUTION ;;;;;;;;;;;;

(define-values (candidate-registration candidate-roll) (make-candidate-registry))
(define-values (voter-registration voter-roll) (make-voter-registry))

(make-candidate "Bernie" 50 0 candidate-registration)
(make-candidate "Biden" 25 0 candidate-registration)
(make-candidate "Tulsi" 6 0 candidate-registration)
(make-candidate "Donkey" 1000000000000000 200 candidate-registration)
(make-candidate "Vermin Supreme" 35 0 candidate-registration)
(make-candidate "Vermin Supreme 2" 35 0 candidate-registration)
(make-candidate "Vermin Supreme 3" 35 0 candidate-registration)
(make-stubborn-candidate "1" 0 2000 candidate-registration)
;; (make-candidate "2" 0 0 candidate-registration)
;; (make-candidate "3" 0 0 candidate-registration)
;; (make-candidate "aaaaa" 0 100 candidate-registration)

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
(make-voter "113" (stupid-sort "Tulsi") voter-registration candidate-roll)
(make-voter "224" (stupid-sort "Tulsi") voter-registration candidate-roll)
(make-voter "335" (stupid-sort "Tulsi") voter-registration candidate-roll)
(make-voter "446" (stupid-sort "Tulsi") voter-registration candidate-roll)
(make-voter "112" (stupid-sort "Donkey") voter-registration candidate-roll)
(make-voter "223" (stupid-sort "Donkey") voter-registration candidate-roll)
(make-voter "334" (stupid-sort "Donkey") voter-registration candidate-roll)
(make-voter "445" (stupid-sort "Donkey") voter-registration candidate-roll)
(make-voter "446" (stupid-sort "1") voter-registration candidate-roll)
;; (make-voter "447" (stupid-sort "1") voter-registration candidate-roll)
;; (make-voter "448" (stupid-sort "1") voter-registration candidate-roll)
;; (make-voter "449" (stupid-sort "1") voter-registration candidate-roll)
;; (make-voter "450" (stupid-sort "1") voter-registration candidate-roll)

(thread-wait (make-vote-leader candidate-roll voter-roll))
