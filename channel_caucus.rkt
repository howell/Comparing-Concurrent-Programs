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

;; a Request-Msg is a (request-msg Chan)
(struct request-msg (chan) #:transparent)

;; a RequestVoters is a (request-voters Region Chan)
(struct request-voters (region leader-chan) #:transparent)

;; a Request-Vote is a (request-vote Chan)
(struct request-vote (candidates chan) #:transparent)

;; a Vote is a (vote Name Name)
(struct vote (name candidate) #:transparent)

;; a ballot-results is a (ballot-results (Hashof Name . number))
(struct ballot-results (votes) #:transparent)

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

;;;;; REGISTRY STRUCTS ;;;;;

;; a Publish is a (publish Any)
(struct publish (val) #:transparent)

;; a Withdraw is a (withdraw Any)
(struct withdraw (val) #:transparent)

;; a Subscribe is a (subscribe Chan)
(struct subscribe (subscriber-chan) #:transparent)

;; a Message is a (message Chan)
(struct message (response-chan) #:transparent)

;; a Payload is a (payload [Setof Any])
(struct payload (data) #:transparent)

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
;; 4. At the end of every round of voting, the ballot-results of votes is sent to every Candidate.
;; 5. When a candidate wins an election, the Vote Leader publishes that information to the Region Manager. When all caucuses have reported, the majority winner is elected.
;;

;;;; HELPERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; filters out all candidates that appear in the set
;; (Listof Candidate) (Setof Candidate) -> (Listof Candidate)
(define (filter-candidates candidates blacklist)
  (filter (λ (cand) (not (set-member? blacklist (candidate-name cand)))) candidates))

(define (make-abstract-registry)
  (log-caucus-evt "Abstract registry is in business!")
  (define publisher-chan (make-channel))
  (define subscriber-chan (make-channel))

  (thread
    (thunk
      (let loop ([data (set)]
                [subscribers (set)])
        (sync
          (handle-evt
            publisher-chan
            (match-lambda
              [(publish val)
              (log-caucus-evt "New value ~a has been published!" val)
              (define new-data (set-add data val))
              (for ([subscriber subscribers]) (channel-put subscriber (payload new-data)))
              (loop new-data subscribers)]
              [(withdraw val)
              (log-caucus-evt "Value ~a is being removed from the registry!" val)
              (define new-data (set-remove data val))
              (loop new-data subscribers)]))
          (handle-evt
            subscriber-chan
            (match-lambda
              [(subscribe subscriber-chan)
              (log-caucus-evt "New subscriber ~a is following the registry!" subscriber-chan)
              (channel-put subscriber-chan (payload data))
              (loop data (set-add subscribers subscriber-chan))]
              [(message response-chan)
              (log-caucus-evt "Channel ~a has requested a message from the registry!" response-chan)
              (channel-put response-chan (payload data))
              (loop data subscribers)]))))))
      (values publisher-chan subscriber-chan))

;; Create a Candidate thread
;; Name Tax-Rate Candidate-Registry -> void
(define (make-candidate name tax-rate threshold registration-chan)
  (define results-chan (make-channel))
  (define cand-struct (candidate name tax-rate results-chan))
  (thread 
    (thunk
      (log-caucus-evt "Candidate ~a has entered the race!" name)
      (channel-put registration-chan (publish cand-struct))
      (let loop ()
        (digest-results cand-struct threshold registration-chan)
        (loop)))))

;; Create a Candidate that tries re-inserting itself into the race
;; Name TaxRate Threshold Chan -> void
(define (make-stubborn-candidate name tax-rate threshold registration-chan)
  (define results-chan (make-channel))
  (define cand-struct (candidate name tax-rate results-chan))
  (thread
    (thunk
      (log-caucus-evt "Stubborn Candidate ~a has entered the race!" name)
      (channel-put registration-chan (publish cand-struct))
      (let loop ()
        (define still-in-race? (digest-results cand-struct threshold registration-chan))
        (when (not still-in-race?)
          (channel-put registration-chan (publish cand-struct))
          (log-caucus-evt "Stubborn Candidate ~a is trying to re-enter the race!" name)
          (loop))))))

;; receive and handle votes from latest election round
;; Candidate Threshold Boolean Chan
(define (digest-results cand threshold reg-chan)
  (define name (candidate-name cand))
  (define results-chan (candidate-results-chan cand))

  (define msg (channel-get results-chan))
  (match msg
    [(ballot-results votes)
     (cond
      [(< (hash-ref votes name 0) threshold)
       (channel-put reg-chan (withdraw name))
       (log-caucus-evt "Candidate ~a has submitted a request to drop out of the race!" name)
       #f]
      [else #t])]
    [(loser name) #f]))

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
      (channel-put voter-registry (publish (voter name region voting-chan)))
      (let loop ([candidates (set)])
        (sync
          (handle-evt
            receive-candidates-chan
            (match-lambda
              ;; A response from the Candidate Registry has been received!
              [(payload curr-candidates) (loop curr-candidates)]))
          (handle-evt
            voting-chan
            (match-lambda
              ;; A request to vote has been received from the Vote Leader!
              [(request-vote available-candidates leader-chan)
               (voting-procedure candidates available-candidates leader-chan)
               (loop candidates)])))))))

;; Make the Vote Leader thread
;; Region Chan Chan Chan -> vote leader thread
(define (make-vote-leader region candidate-registry voter-registry results-chan)
  (define retrieve-candidates-chan (make-channel))
  (define retrieve-voters-chan (make-channel))
  (define voting-chan (make-channel))
  (thread
    (thunk
      (sleep 1) ;; to make sure all other actors get situated first
      (log-caucus-evt "The Vote Leader is ready to run the caucus!")

      ;; Start a sequence of votes to determine an elected candidate
      ;; (Setof Name) -> Candidate
      (define (run-caucus removed-candidates voting-whitelist)
        (define (receive-candidates removed-candidates)
          (channel-put candidate-registry (message retrieve-candidates-chan))
          (define cand-payload (channel-get retrieve-candidates-chan))
          (match cand-payload
            [(payload new-candidates)
            (list->set (filter-candidates (set->list new-candidates) removed-candidates))]))

        (define (receive-voters voter-whitelist)
          (channel-put voter-registry (message retrieve-voters-chan))
          (define voter-payload (channel-get retrieve-voters-chan))
          (match voter-payload
            [(payload new-voters)
             (if (set-empty? voting-whitelist)
               new-voters
               (set-intersect new-voters voting-whitelist))]))

        (define (issue-votes valid-voters eligible-candidates)
          (log-caucus-evt "Vote leader in region ~a is issuing a vote!" region)
          (define eligible-cand-names (map (λ (cand) (candidate-name cand)) (set->list eligible-candidates)))
          (for ([voter valid-voters])
            (channel-put (voter-voting-chan voter) (request-vote eligible-cand-names voting-chan))))
                

        (log-caucus-evt "The Vote Leader is beginning a new round of voting!")
        (define eligible-candidates (receive-candidates removed-candidates))
        (define valid-voters (receive-voters voting-whitelist))
        (issue-votes valid-voters eligible-candidates)
        (collect-votes valid-voters eligible-candidates removed-candidates))


      ;; Determine winner of a round of voting or eliminate a candidate and move to the next one
      ;; (Setof Candidate) (Setof Voter) -> Candidate
      (define (collect-votes voters candidates removed-candidates)
        (define VOTE-DEADLINE (+ (current-inexact-milliseconds) 500))

        ;; NOTE move this to just above the match?
        (define vote-timeout (alarm-evt VOTE-DEADLINE))

        (define (create-whitelist voters)
          (for/hash ([voter voters]) (values (voter-name voter) voter)))


        (let voting-loop ([whitelist (create-whitelist voters)]
                          [voting-record (hash)])

          ;; Determine winner if one candidate has received majority of votes, otherwise begin next round of voting
          ;; (Hashof Name -> Voter) (Hashof Name -> Name) (Hashof Name -> number) -> candidate msg to vote leader
          (define (count-votes whitelist voting-record)
            (define votes (foldl (λ (vote acc) (hash-update acc (cdr vote) add1 0)) (hash) (hash->list voting-record)))
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
            (for ([cand-struct candidates]) 
              (channel-put (candidate-results-chan cand-struct) (ballot-results votes)))
            (channel-put (candidate-results-chan losing-cand) (loser (candidate-name losing-cand)))

            (log-caucus-evt "Candidate ~a has been eliminated from the race!" (candidate-name losing-cand))
            (run-caucus (set-add removed-candidates (candidate-name losing-cand)) (list->set (hash-values whitelist))))

          ;; Determine if all votes for the round have concluded
          ;; (Hashof Name -> Voter) (Hashof Name -> Name) (Hashof Name -> number) -> candidate msg to vote leader
          (define (conclude-vote? whitelist voting-record)
            (if (= (hash-count voting-record) (hash-count whitelist))
              (count-votes whitelist voting-record)
              (voting-loop whitelist voting-record)))

          (define (try-cast-vote name candidate whitelist voting-record)
            (cond
              [(not (hash-has-key? whitelist name))
               (log-caucus-evt "Invalid voter ~a has tried to vote!" name)
               (values whitelist voting-record)]
              [(hash-has-key? voting-record name)
               (log-caucus-evt "Voter ~a has already voted! ~a is no longer a valid voter!" name name)
               (values (hash-remove whitelist name) (hash-remove voting-record name))]
              [(andmap (λ (cand) (not (string=? candidate (candidate-name cand)))) (set->list candidates))
               (log-caucus-evt "Voter ~a voted for candidate ~a, who isn't currently an eligible candidate!" name candidate)
               (values (hash-remove whitelist name) voting-record)]
              [else (values whitelist (hash-set voting-record name candidate))]))


          (sync
            (handle-evt
              voting-chan
              (match-lambda
                [(vote name candidate)
                 (define-values (new-whitelist new-voting-record) (try-cast-vote name candidate whitelist voting-record))
                 (conclude-vote? new-whitelist new-voting-record)]))

                 #|
              (match-lambda
                [(vote name candidate)
                 (cond
                   [(not (hash-has-key? whitelist name)) 
                    (log-caucus-evt "Invalid voter ~a has tried to vote!" name)
                    (conclude-vote? whitelist voting-record)]
                   [(hash-has-key? voting-record name)
                    (log-caucus-evt "Voter ~a has already voted! ~a is no longer a valid voter!" name name)
                    (conclude-vote? (hash-remove whitelist name) (hash-remove voting-record name))]
                   [(andmap (λ (cand) (not (string=? candidate (candidate-name cand)))) (set->list candidates)) 
                    (log-caucus-evt "Voter ~a voted for candidate ~a, who isn't currently an eligible candidate!" name candidate)
                    (conclude-vote? (hash-remove whitelist name) voting-record)]
                   [else (conclude-vote? whitelist (hash-set voting-record name candidate))])]))
                |#
            (handle-evt
              vote-timeout
              (λ (_) 
                 (log-caucus-evt "Round of voting in region ~a is over!" region)
                 (conclude-vote?
                   (make-immutable-hash (filter (λ (wl-pair) (hash-has-key? voting-record (car wl-pair))) (hash->list whitelist)))
                   voting-record))))))
        
      (define winner (run-caucus (set) (set)))
      (log-caucus-evt "We have a winner ~a in region ~a!" (candidate-name winner) region)
      (channel-put results-chan (declare-winner (candidate-name winner))))))

;; remove declaration behavior
;; add a list of regions as an argument to the region-manager
;; remove the declaration behavior in the vote leader, just the results behavior

;; Create a region-manager thread and channel
;; Chan -> winner announcement
(define (make-region-manager regions candidate-registry voter-registries main-chan)
  (define results-chan (make-channel))
  (thread
    (thunk
      (for ([region regions]
            [registry voter-registries])
        (make-vote-leader region candidate-registry registry results-chan))
      (let loop ([caucus-results (hash)])
        (define region-winner (channel-get results-chan))
        (match region-winner
          [(declare-winner candidate)
           (define new-results (hash-update caucus-results candidate add1 0))
           (define num-winners (for/sum ([num-of-votes (in-hash-values new-results)]) num-of-votes))
           (cond
             [(= num-winners (length regions))
              (define front-runner (argmax (λ (pair) (cdr pair)) (hash->list new-results)))
              (define front-runner-name (car front-runner))
              (log-caucus-evt "The winner of the region is ~a!" front-runner-name)
              (channel-put main-chan front-runner-name)]
             [else (loop new-results)])])))))

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
(define-values (candidate-registration candidate-roll) (make-abstract-registry))

(make-candidate "Bernie" 50 0 candidate-registration)
(make-candidate "Biden" 25 0 candidate-registration)
(make-candidate "Tulsi" 6 0 candidate-registration)
(make-candidate "Donkey" 1000000000000000 200 candidate-registration)
(make-candidate "Vermin Supreme" 35 0 candidate-registration)
(make-candidate "Steerpike" 0 0 candidate-registration)
(make-stubborn-candidate "ZZZ" 0 200000 candidate-registration)

;;;; Region 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (voter-registration-1 voter-roll-1) (make-abstract-registry))

(make-voter "XYZ" "Region1" (stupid-sort "Vermin Supreme") voter-registration-1 candidate-roll)
(make-voter "FOO" "Region1" (stupid-sort "Vermin Supreme") voter-registration-1 candidate-roll)
(make-voter "BAR" "Region1" (stupid-sort "Vermin Supreme") voter-registration-1 candidate-roll)
(make-voter "BAZ" "Region1" (stupid-sort "Vermin Supreme") voter-registration-1 candidate-roll)
(make-voter "012" "Region1" (stupid-sort "Biden") voter-registration-1 candidate-roll)
(make-voter "123" "Region1" (stupid-sort "Biden") voter-registration-1 candidate-roll)
(make-voter "234" "Region1" (stupid-sort "Biden") voter-registration-1 candidate-roll)
(make-greedy-voter "ABC" "Region1" (stupid-sort "Bernie" "Tulsi") voter-registration-1 candidate-roll)
(make-greedy-voter "DEF" "Region1" (stupid-sort "Bernie" "Tulsi") voter-registration-1 candidate-roll)
(make-greedy-voter "GHI" "Region1" (stupid-sort "Bernie" "Tulsi") voter-registration-1 candidate-roll)
(make-greedy-voter "JKL" "Region1" (stupid-sort "Biden" "Tulsi") voter-registration-1 candidate-roll)
(make-greedy-voter "MNO" "Region1" (stupid-sort "Biden" "Tulsi") voter-registration-1 candidate-roll)
(make-greedy-voter "PQR" "Region1" (stupid-sort "Biden" "Tulsi") voter-registration-1 candidate-roll)
(make-stubborn-voter "345" "Region1" "Tulsi" voter-registration-1 candidate-roll)
(make-stubborn-voter "456" "Region1" "ZZZ" voter-registration-1 candidate-roll)
(make-stubborn-voter "567" "Region1" "ZZZ" voter-registration-1 candidate-roll)
(make-stubborn-voter "678" "Region1" "ZZZ" voter-registration-1 candidate-roll)
(make-stubborn-voter "789" "Region1" "ZZZ" voter-registration-1 candidate-roll)
(make-stubborn-voter "457" "Region1" "ZZZ" voter-registration-1 candidate-roll)
(make-stubborn-voter "568" "Region1" "ZZZ" voter-registration-1 candidate-roll)
(make-stubborn-voter "679" "Region1" "ZZZ" voter-registration-1 candidate-roll)
(make-stubborn-voter "790" "Region1" "ZZZ" voter-registration-1 candidate-roll)

(make-sleepy-voter "0" "Region1" voter-registration-1 candidate-roll)
(make-sleepy-voter "1" "Region1" voter-registration-1 candidate-roll)
(make-sleepy-voter "2" "Region1" voter-registration-1 candidate-roll)
(make-sleepy-voter "3" "Region1" voter-registration-1 candidate-roll)
(make-sleepy-voter "4" "Region1" voter-registration-1 candidate-roll)
(make-sleepy-voter "5" "Region1" voter-registration-1 candidate-roll)
(make-sleepy-voter "6" "Region1" voter-registration-1 candidate-roll)
(make-sleepy-voter "7" "Region1" voter-registration-1 candidate-roll)
(make-sleepy-voter "8" "Region1" voter-registration-1 candidate-roll)
(make-sleepy-voter "9" "Region1" voter-registration-1 candidate-roll)

;;;; Region 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (voter-registration-2 voter-roll-2) (make-abstract-registry))
(make-voter "999" "Region2" (stupid-sort "Steerpike") voter-registration-2 candidate-roll)
(make-voter "998" "Region2" (stupid-sort "Steerpike") voter-registration-2 candidate-roll)
(make-voter "997" "Region2" (stupid-sort "Steerpike") voter-registration-2 candidate-roll)
(make-voter "996" "Region2" (stupid-sort "Steerpike") voter-registration-2 candidate-roll)
(make-voter "995" "Region2" (stupid-sort "Steerpike") voter-registration-2 candidate-roll)
(make-voter "994" "Region2" (stupid-sort "Steerpike") voter-registration-2 candidate-roll)
(make-voter "993" "Region2" (stupid-sort "Steerpike") voter-registration-2 candidate-roll)
(make-voter "992" "Region2" (stupid-sort "Steerpike") voter-registration-2 candidate-roll)
(make-voter "991" "Region2" (stupid-sort "Steerpike") voter-registration-2 candidate-roll)
(make-voter "990" "Region2" (stupid-sort "Steerpike") voter-registration-2 candidate-roll)
(make-voter "989" "Region2" (stupid-sort "Donkey") voter-registration-2 candidate-roll)
(make-voter "988" "Region2" (stupid-sort "Donkey") voter-registration-2 candidate-roll)
(make-voter "987" "Region2" (stupid-sort "Donkey") voter-registration-2 candidate-roll)
(make-voter "986" "Region2" (stupid-sort "Donkey") voter-registration-2 candidate-roll)
(make-voter "985" "Region2" (stupid-sort "Donkey") voter-registration-2 candidate-roll)
(make-voter "984" "Region2" (stupid-sort "Donkey") voter-registration-2 candidate-roll)
(make-voter "983" "Region2" (stupid-sort "Donkey") voter-registration-2 candidate-roll)

;;;; Region 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (voter-registration-3 voter-roll-3) (make-abstract-registry))
(make-voter "999" "Region3" (stupid-sort "Steerpike") voter-registration-3 candidate-roll)
(make-voter "998" "Region3" (stupid-sort "Steerpike") voter-registration-3 candidate-roll)
(make-voter "997" "Region3" (stupid-sort "Steerpike") voter-registration-3 candidate-roll)
(make-voter "996" "Region3" (stupid-sort "Steerpike") voter-registration-3 candidate-roll)
(make-voter "995" "Region3" (stupid-sort "Steerpike") voter-registration-3 candidate-roll)
(make-voter "994" "Region3" (stupid-sort "Steerpike") voter-registration-3 candidate-roll)
(make-voter "993" "Region3" (stupid-sort "Steerpike") voter-registration-3 candidate-roll)
(make-voter "992" "Region3" (stupid-sort "Steerpike") voter-registration-3 candidate-roll)
(make-voter "991" "Region3" (stupid-sort "Steerpike") voter-registration-3 candidate-roll)
(make-voter "990" "Region3" (stupid-sort "Steerpike") voter-registration-3 candidate-roll)
(make-voter "989" "Region3" (stupid-sort "Donkey") voter-registration-3 candidate-roll)
(make-voter "988" "Region3" (stupid-sort "Donkey") voter-registration-3 candidate-roll)
(make-voter "987" "Region3" (stupid-sort "Donkey") voter-registration-3 candidate-roll)
(make-voter "986" "Region3" (stupid-sort "Donkey") voter-registration-3 candidate-roll)
(make-voter "985" "Region3" (stupid-sort "Donkey") voter-registration-3 candidate-roll)
(make-voter "984" "Region3" (stupid-sort "Donkey") voter-registration-3 candidate-roll)
(make-voter "983" "Region3" (stupid-sort "Donkey") voter-registration-3 candidate-roll)


(make-region-manager (list "Region1" "Region2" "Region3") candidate-roll (list voter-roll-1 voter-roll-2 voter-roll-3) main-channel)

(define msg (channel-get main-channel))
(printf "We have our winner! ~a\n" msg)
