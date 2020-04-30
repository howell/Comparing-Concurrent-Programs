#lang racket
(require racket/set)

(define caucus-log (make-logger 'caucus (current-logger)))

;; Log information in a thread-safe manner
;; NOTE requires using `info@caucus` as the log-level when program is executed
(define (log-caucus-evt evt . vals)
  (log-message caucus-log 'info (logger-name caucus-log) (apply format evt vals)))

;; a Name is a string

;; a Tax-Rate is a number

;; a Threshold is a number

;; a Chan is a channel

;; a Region is a string

;; a Candidate is a (candidate Name Tax-Rate Chan)
(struct candidate (name tax-rate results-chan) #:transparent)

;; a DropOut is a (drop-out Name)
(struct drop-out (name) #:transparent)

;; a Loser is a (loser Name)
(struct loser (name) #:transparent)

;; a Voter is a (voter Name Region Chan)
(struct voter (name region voting-chan) #:transparent)

;; a Subscribe is a (subscribe Chan)
(struct subscribe (chan) #:transparent)

;; a Request-Msg is a (request-msg Chan)
(struct request-msg (chan) #:transparent)

;; a RequestVoters is a (request-voters Region Chan)
(struct request-voters (region leader-chan) #:transparent)

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

;; a DeclareLeader is a (declare-leader Region Chan)
(struct declare-leader (region manager-comm-chan) #:transparent)

;; a DeclareManager is a (declare-manager Chan)
(struct declare-manager (results-chan) #:transparent)

;; a DeclareWinner is a (declare-winner Name)
(struct declare-winner (candidate) #:transparent)


;;;; ASSUMPTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. Voters never try joining late (how is this even expressible...?) --> Hard to get right due to timing  (vote only after first round?)
;; 

;;;; ENTITIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Candidates        
;; 2. Candidate Registry
;; 3. Voters            
;; 4. Voter Registry    
;; 5. Vote Leader       
;; 6. Region Manager
;; 

;;;; CONVERSATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Publish Conversations
;; 1. Candidates publish their information to the Candidate Registry through a `candidate` struct sent to the Registry's channel
;; 2. Voters publish their information to the Voter Registry through a `voter` struct sent to the Registry's channel
;; 3. Candidates can remove themselves from eligibility by sending a `drop-out` struct to the Candidate Registry
;;     -> This occurs when the candidate receives a number of votes below the candidate's threshold for staying in the race.
;; 4. Vote Leaders publish their information to the Region Manager through a `declare-leader` struct
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
;; 5. When a candidate wins an election, the Vote Leader publishes that information to the Region Manager. When all caucuses have reported, the majority winner is elected.
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

      ;; Send a complete list of candidates to all subscribers of the candidate registry
      ;; (Setof Chan) (Setof Candidate) -> AllCandidates messages
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
               (log-caucus-evt "Candidate ~a has successfully subscribed!" name)
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
               (log-caucus-evt "There is a new subscriber to the candidate registry!")
               (channel-put chan (all-candidates candidates))
               (loop candidates all-cand-names (set-add subscribers chan))]
              ;; a channel has requested a snapshot of the current Candidates!
              [(request-msg chan)
               (log-caucus-evt "A new request for candidates has been received by the candidate registry!")
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

;; Create a Candidate that tries re-inserting itself into the race
;; Name TaxRate Threshold Chan -> void
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
      (let loop ([voters (hash)])
        (sync
          (handle-evt
            registration-chan
            (match-lambda
              ;; A Voter has registered!
              [(voter name region voter-chan) 
               (log-caucus-evt "Voter ~a has successfully registered!" name)
               (loop (hash-update voters region (λ (old) (set-add old (voter name region voter-chan))) (set)))]))
          (handle-evt
            receive-roll-chan
            (match-lambda
              ;; A request for voter data has been received!
              [(request-voters region recv-chan)
               (log-caucus-evt "The voters from region ~a have been requested!" region)
               (channel-put recv-chan (all-voters (hash-ref voters region (set))))
               (loop voters)]))))))
  (values registration-chan receive-roll-chan))

;; Make a Voter thread
(define (make-voter name region rank-candidates voter-registry candidate-registry)
  (define normal-voting
    (λ (existing-candidates available-candidates leader-chan)
       (define priorities (rank-candidates (set->list existing-candidates)))
       (define voting-for
         (for/first ([candidate (in-list priorities)]
                     #:when (member (candidate-name candidate) available-candidates))
                    (candidate-name candidate)))
       (log-caucus-evt "Voter ~a has submitted a vote for candidate ~a!" name voting-for)
       (announce-vote leader-chan (vote name voting-for))))

  (voter-skeleton name region normal-voting voter-registry candidate-registry))

;; Make a voter thread that produces a greedy voter (who votes multiple times)
(define (make-greedy-voter name region rank-candidates voter-registry candidate-registry)
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
       (log-caucus-evt "Greedy voter ~a is submitting two votes!" name)
       (announce-vote leader-chan (vote name voting-for))
       (announce-vote leader-chan (vote name (if second-vote second-vote voting-for)))))

  (voter-skeleton name region greedy-voting voter-registry candidate-registry))

;; Make a voter thread that always votes for the same candidate
(define (make-stubborn-voter name region favorite-candidate voter-registry candidate-registry)
  (define stubborn-voting
    (λ (existing-candidates available-candidates leader-chan)
       (log-caucus-evt "Stubborn voter ~a is voting for ~a again!" name favorite-candidate)
       (announce-vote leader-chan (vote name favorite-candidate))))

  (voter-skeleton name region stubborn-voting voter-registry candidate-registry))

;; Make a voter that sleeps through their vote (doesn't vote)
(define (make-sleepy-voter name region voter-registry candidate-registry)
  (define sleepy-voting
    (λ (x y z) (log-caucus-evt "Sleepy voter ~a has slept through their vote!" name)))

  (voter-skeleton name region sleepy-voting voter-registry candidate-registry))

;; Submit a vote to the vote leader
;; NOTE this is put in another thread to prevent deadlocks in voters
;; Chan Vote -> thread
(define (announce-vote leader-chan vote)
  (thread (thunk (channel-put leader-chan vote))))

;; Create a voter thread
;; Name Region ((Listof Candidate) (Listof Candidate) Chan -> thread w/vote) Chan Chan -> voter thread
(define (voter-skeleton name region voting-procedure voter-registry candidate-registry)
  (define receive-candidates-chan (make-channel))
  (define voting-chan (make-channel))
  (thread
    (thunk
      (log-caucus-evt "Voter ~a is registering!" name)
      (channel-put candidate-registry (subscribe receive-candidates-chan))
      (channel-put voter-registry (voter name region voting-chan))
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

;; Make the Vote Leader thread
;; Region Chan Chan Chan -> vote leader thread
(define (make-vote-leader region candidate-registry voter-registry manager-chan)
  (define retrieve-candidates-chan (make-channel))
  (define retrieve-voters-chan (make-channel))
  (define manager-comm-chan (make-channel))
  (define voting-chan (make-channel))
  (thread
    (thunk
      (channel-put manager-chan (declare-leader region manager-comm-chan))
      (define manager-results-chan (declare-manager-results-chan (channel-get manager-comm-chan)))
      (sleep 1) ;; to make sure all other actors get situated first
      (log-caucus-evt "The Vote Leader is ready to run the caucus!")

      ;; Start a sequence of votes to determine an elected candidate
      ;; (Setof Name) -> Candidate
      (define (run-caucus removed-candidates voting-whitelist)
        (log-caucus-evt "The Vote Leader is beginning a new round of voting!")

        (channel-put candidate-registry (request-msg retrieve-candidates-chan))
        (channel-put voter-registry (request-voters region retrieve-voters-chan))

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
                   [candidates (set)])
          (match curr-state
            ['SETUP
             (log-caucus-evt "Vote leader in region ~a is setting up the caucus!" region)
             (sync
               (handle-evt
                 retrieve-voters-chan
                 (match-lambda
                   [(all-voters new-voters)
                    (if (set-empty? voting-whitelist)
                      (transition-states new-voters candidates loop)
                      (transition-states (set-intersect new-voters voting-whitelist) candidates loop))]))
               (handle-evt
                 retrieve-candidates-chan
                 (match-lambda
                   [(all-candidates new-candidates)
                    (define eligible-candidates (list->set (filter-candidates (set->list new-candidates) removed-candidates)))
                    (transition-states voters eligible-candidates loop)])))]
            ['VOTING
             (log-caucus-evt "Vote leader in region ~a is issuing a vote!" region)
             (for ([voter (set->list voters)])
               (channel-put (voter-voting-chan voter) 
                            (request-vote 
                              (map (λ (cand) (candidate-name cand)) (set->list candidates)) 
                              voting-chan)))
             (collect-votes voters candidates removed-candidates)])))

      ;; Determine winner of a round of voting or eliminate a candidate and move to the next one
      ;; (Setof Candidate) (Setof Voter) -> Candidate
      (define (collect-votes voters candidates removed-candidates)
        (define vote-timeout (alarm-evt (+ (current-inexact-milliseconds) 500)))

        (let voting-loop ([whitelist (foldl (λ (voter curr-list) (hash-set curr-list (voter-name voter) voter)) (hash) (set->list voters))]
                          [voting-record (hash)]
                          [votes (hash)])

          ;; Determine winner if one candidate has received majority of votes, otherwise begin next round of voting
          ;; (Hashof Name -> Voter) (Hashof Name -> Name) (Hashof Name -> number) -> candidate msg to vote leader
          (define (count-votes whitelist voting-record votes)
            (define front-runner (argmax (λ (cand) (hash-ref votes (candidate-name cand) 0)) (set->list candidates)))
            (define their-votes (hash-ref votes (candidate-name front-runner) 0))
            (cond
              [(> their-votes (/ (hash-count voting-record) 2))
               (log-caucus-evt "Candidate ~a has been elected!" (candidate-name front-runner))
               front-runner]
              [else (next-round whitelist voting-record votes)]))

          ;; Remove the worst-performing candidate from the race and re-run caucus
          ;; (Hashof Name -> Voter) (Hashof Name -> Name) (Hashof Name -> number) -> candidate msg to vote leader
          (define (next-round whitelist voting-record votes)
            (define losing-cand (argmin (λ (cand) (hash-ref votes (candidate-name cand) 0)) (set->list candidates)))
            (for ([cand-struct (set->list candidates)]) 
              (channel-put (candidate-results-chan cand-struct) (tally votes)))
            (for/first ([cand-struct (set->list candidates)]
                        #:when (string=? (candidate-name losing-cand) (candidate-name cand-struct)))
                       (channel-put (candidate-results-chan cand-struct) (loser (candidate-name cand-struct))))
            (log-caucus-evt "Candidate ~a has been eliminated from the race!" (candidate-name losing-cand))
            (run-caucus (set-add removed-candidates (candidate-name losing-cand)) (list->set (hash-values whitelist))))

          ;; Determine if all votes for the round have concluded
          ;; (Hashof Name -> Voter) (Hashof Name -> Name) (Hashof Name -> number) -> candidate msg to vote leader
          (define (conclude-vote? whitelist voting-record votes)
            (if (= (hash-count voting-record) (hash-count whitelist))
              (count-votes whitelist voting-record votes)
              (voting-loop whitelist voting-record votes)))

          (sync
            (handle-evt
              voting-chan
              (match-lambda
                [(vote name candidate)
                 (cond
                   [(not (hash-has-key? whitelist name)) 
                    (log-caucus-evt "Invalid voter ~a has tried to vote!" name)
                    (conclude-vote? whitelist voting-record votes)]
                   [(hash-has-key? voting-record name)
                    (log-caucus-evt "Voter ~a has already voted! ~a is no longer a valid voter!" name name)
                    (conclude-vote? (hash-remove whitelist name) (hash-remove voting-record name) (hash-update votes (hash-ref voting-record name) sub1))]
                   [(andmap (λ (cand) (not (string=? candidate (candidate-name cand)))) (set->list candidates)) 
                    (log-caucus-evt "Voter ~a voted for candidate ~a, who isn't currently an eligible candidate!" name candidate)
                    (conclude-vote? (hash-remove whitelist name) voting-record votes)]
                   [else (conclude-vote? whitelist (hash-set voting-record name candidate) (hash-update votes candidate add1 0))])]))
            (handle-evt
              vote-timeout
              (λ (_) 
                 (log-caucus-evt "Round of voting in region ~a is over!" region)
                 (conclude-vote?
                   (make-immutable-hash (filter (λ (wl-pair) (hash-has-key? voting-record (car wl-pair))) (hash->list whitelist)))
                   voting-record
                   votes)))))) 
        
      (define winner (run-caucus (set) (set)))
      (log-caucus-evt "We have a winner ~a in region ~a!" (candidate-name winner) region)
      (channel-put manager-results-chan (declare-winner (candidate-name winner))))))

;; Create a region-manager thread and channel
;; Chan -> winner announcement
(define (make-region-manager main-chan)
  (define declaration-chan (make-channel))
  (define results-chan (make-channel))
  (thread
    (thunk
      (let loop ([vote-leaders (set)]
                 [caucus-results (hash)])
        (sync
          (handle-evt
            declaration-chan
            (match-lambda
              [(declare-leader region manager-comm-chan)
               (channel-put manager-comm-chan (declare-manager results-chan))
               (loop (set-add vote-leaders (declare-leader region manager-comm-chan)) caucus-results)]))
          (handle-evt
            results-chan
            (match-lambda
              [(declare-winner candidate)
               (define new-results (hash-update caucus-results candidate add1 0))
               (define num-winners (for/sum ([num-of-votes (in-hash-values new-results)]) num-of-votes))
               (cond
                 [(= num-winners (set-count vote-leaders))
                  (define front-runner (argmax (λ (pair) (cdr pair)) (hash->list new-results)))
                  (define front-runner-name (car front-runner))
                  (define their-votes (hash-ref new-results front-runner-name  0))
                  (cond
                    [(> their-votes (/ num-winners 2))
                     (log-caucus-evt "The winner of the region is ~a!" front-runner-name)
                     (channel-put main-chan front-runner-name)]
                    [else (loop vote-leaders new-results)])]
                 [else (loop vote-leaders new-results)])]))))))
  declaration-chan)

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

;; Return a function that sorts a Listof Name by putting a specified number of Names at the front of the list
;; (Listof Name) -> ((Listof Name) -> (Listof Name))
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
(define main-channel (make-channel))

;;;; GENERAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (candidate-registration candidate-roll) (make-candidate-registry))
(define-values (voter-registration voter-roll) (make-voter-registry))
(define manager-chan (make-region-manager main-channel))

(make-candidate "Bernie" 50 0 candidate-registration)
(make-candidate "Biden" 25 0 candidate-registration)
(make-candidate "Tulsi" 6 0 candidate-registration)
(make-candidate "Donkey" 1000000000000000 200 candidate-registration)
(make-candidate "Vermin Supreme" 35 0 candidate-registration)
(make-candidate "Steerpike" 0 0 candidate-registration)
(make-stubborn-candidate "ZZZ" 0 200000 candidate-registration)

;;;; Region 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-voter "XYZ" "Region1" (stupid-sort "Vermin Supreme") voter-registration candidate-roll)
(make-voter "FOO" "Region1" (stupid-sort "Vermin Supreme") voter-registration candidate-roll)
(make-voter "BAR" "Region1" (stupid-sort "Vermin Supreme") voter-registration candidate-roll)
(make-voter "BAZ" "Region1" (stupid-sort "Vermin Supreme") voter-registration candidate-roll)
(make-voter "012" "Region1" (stupid-sort "Biden") voter-registration candidate-roll)
(make-voter "123" "Region1" (stupid-sort "Biden") voter-registration candidate-roll)
(make-voter "234" "Region1" (stupid-sort "Biden") voter-registration candidate-roll)
(make-greedy-voter "ABC" "Region1" (stupid-sort "Bernie" "Tulsi") voter-registration candidate-roll)
(make-greedy-voter "DEF" "Region1" (stupid-sort "Bernie" "Tulsi") voter-registration candidate-roll)
(make-greedy-voter "GHI" "Region1" (stupid-sort "Bernie" "Tulsi") voter-registration candidate-roll)
(make-greedy-voter "JKL" "Region1" (stupid-sort "Biden" "Tulsi") voter-registration candidate-roll)
(make-greedy-voter "MNO" "Region1" (stupid-sort "Biden" "Tulsi") voter-registration candidate-roll)
(make-greedy-voter "PQR" "Region1" (stupid-sort "Biden" "Tulsi") voter-registration candidate-roll)
(make-stubborn-voter "345" "Region1" "Tulsi" voter-registration candidate-roll)
(make-stubborn-voter "456" "Region1" "ZZZ" voter-registration candidate-roll)
(make-stubborn-voter "567" "Region1" "ZZZ" voter-registration candidate-roll)
(make-stubborn-voter "678" "Region1" "ZZZ" voter-registration candidate-roll)
(make-stubborn-voter "789" "Region1" "ZZZ" voter-registration candidate-roll)
(make-stubborn-voter "457" "Region1" "ZZZ" voter-registration candidate-roll)
(make-stubborn-voter "568" "Region1" "ZZZ" voter-registration candidate-roll)
(make-stubborn-voter "679" "Region1" "ZZZ" voter-registration candidate-roll)
(make-stubborn-voter "790" "Region1" "ZZZ" voter-registration candidate-roll)

(make-sleepy-voter "0" "Region1" voter-registration candidate-roll)
(make-sleepy-voter "1" "Region1" voter-registration candidate-roll)
(make-sleepy-voter "2" "Region1" voter-registration candidate-roll)
(make-sleepy-voter "3" "Region1" voter-registration candidate-roll)
(make-sleepy-voter "4" "Region1" voter-registration candidate-roll)
(make-sleepy-voter "5" "Region1" voter-registration candidate-roll)
(make-sleepy-voter "6" "Region1" voter-registration candidate-roll)
(make-sleepy-voter "7" "Region1" voter-registration candidate-roll)
(make-sleepy-voter "8" "Region1" voter-registration candidate-roll)
(make-sleepy-voter "9" "Region1" voter-registration candidate-roll)

(make-vote-leader "Region1" candidate-roll voter-roll manager-chan)

;;;; Region 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-voter "999" "Region2" (stupid-sort "Steerpike") voter-registration candidate-roll)
(make-voter "998" "Region2" (stupid-sort "Steerpike") voter-registration candidate-roll)
(make-voter "997" "Region2" (stupid-sort "Steerpike") voter-registration candidate-roll)
(make-voter "996" "Region2" (stupid-sort "Steerpike") voter-registration candidate-roll)
(make-voter "995" "Region2" (stupid-sort "Steerpike") voter-registration candidate-roll)
(make-voter "994" "Region2" (stupid-sort "Steerpike") voter-registration candidate-roll)
(make-voter "993" "Region2" (stupid-sort "Steerpike") voter-registration candidate-roll)
(make-voter "992" "Region2" (stupid-sort "Steerpike") voter-registration candidate-roll)
(make-voter "991" "Region2" (stupid-sort "Steerpike") voter-registration candidate-roll)
(make-voter "990" "Region2" (stupid-sort "Steerpike") voter-registration candidate-roll)
(make-voter "989" "Region2" (stupid-sort "Donkey") voter-registration candidate-roll)
(make-voter "988" "Region2" (stupid-sort "Donkey") voter-registration candidate-roll)
(make-voter "987" "Region2" (stupid-sort "Donkey") voter-registration candidate-roll)
(make-voter "986" "Region2" (stupid-sort "Donkey") voter-registration candidate-roll)
(make-voter "985" "Region2" (stupid-sort "Donkey") voter-registration candidate-roll)
(make-voter "984" "Region2" (stupid-sort "Donkey") voter-registration candidate-roll)
(make-voter "983" "Region2" (stupid-sort "Donkey") voter-registration candidate-roll)

(make-vote-leader "Region2" candidate-roll voter-roll manager-chan)

;;;; Region 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-voter "999" "Region3" (stupid-sort "Steerpike") voter-registration candidate-roll)
(make-voter "998" "Region3" (stupid-sort "Steerpike") voter-registration candidate-roll)
(make-voter "997" "Region3" (stupid-sort "Steerpike") voter-registration candidate-roll)
(make-voter "996" "Region3" (stupid-sort "Steerpike") voter-registration candidate-roll)
(make-voter "995" "Region3" (stupid-sort "Steerpike") voter-registration candidate-roll)
(make-voter "994" "Region3" (stupid-sort "Steerpike") voter-registration candidate-roll)
(make-voter "993" "Region3" (stupid-sort "Steerpike") voter-registration candidate-roll)
(make-voter "992" "Region3" (stupid-sort "Steerpike") voter-registration candidate-roll)
(make-voter "991" "Region3" (stupid-sort "Steerpike") voter-registration candidate-roll)
(make-voter "990" "Region3" (stupid-sort "Steerpike") voter-registration candidate-roll)
(make-voter "989" "Region3" (stupid-sort "Donkey") voter-registration candidate-roll)
(make-voter "988" "Region3" (stupid-sort "Donkey") voter-registration candidate-roll)
(make-voter "987" "Region3" (stupid-sort "Donkey") voter-registration candidate-roll)
(make-voter "986" "Region3" (stupid-sort "Donkey") voter-registration candidate-roll)
(make-voter "985" "Region3" (stupid-sort "Donkey") voter-registration candidate-roll)
(make-voter "984" "Region3" (stupid-sort "Donkey") voter-registration candidate-roll)
(make-voter "983" "Region3" (stupid-sort "Donkey") voter-registration candidate-roll)

(make-vote-leader "Region3" candidate-roll voter-roll manager-chan)

(define msg (channel-get main-channel))
(printf "We have our winner! ~a\n" msg)
