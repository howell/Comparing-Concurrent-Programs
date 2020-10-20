#lang typed/syndicate/roles

;; TODO - wrap timer driver
(assertion-struct later-than : LaterThanT (when))


(define-type-alias Name String)
(define-type-alias ID Symbol)
(define-type-alias TaxRate Int)
(define-type-alias Threshold Int)
(define-type-alias VoteCount Int)
(define-type-alias Region String)

(require-struct voter #:as VoterT #:from "caucus_struct.rkt")
(define-type-alias Voter (VoterT Name Region))

(require-struct vote #:as VoteT #:from "caucus_struct.rkt")
(define-type-alias Vote (VoteT Name ID Region Name))

(require-struct round #:as RoundT #:from "caucus_struct.rkt")
(define-type-alias Round (RoundT ID Region (List Name)))

(require-struct candidate #:as CandidateT #:from "caucus_struct.rkt")
(define-type-alias Candidate (CandidateT Name TaxRate))
(define (candidate-name [c Candidate])
  (match-define (candidate $name _) c)
  name)

(require-struct tally #:as TallyT #:from "caucus_struct.rkt")
(define-type-alias Tally (TallyT Name Region VoteCount))

(require-struct elected #:as ElectedT #:from "caucus_struct.rkt")
(define-type-alias Elected (ElectedT Name Region))

(require-struct winner #:as WinnerT #:from "caucus_struct.rkt")
(define-type-alias Winner (WinnerT Name))

(define (get-one-second-from-now -> Int)
  (+ (current-inexact-milliseconds) 1000))

(define-type-alias CandComms
  (U Candidate
     (Observe (TallyT Name ★/t ★/t))
     (Message Tally)))

(define-type-alias VoterComms
  (U Voter
     Vote
     (Observe (CandidateT ★/t ★/t))
     Candidate
     (Observe (RoundT ★/t Region ★/t))
     Round))

(define-type-alias LeaderComms
  (U Voter
     (Observe (VoterT ★/t Region))
     Candidate
     (Observe (CandidateT ★/t ★/t))
     Round
     Vote
     (Observe (VoteT ★/t ID Region ★/t))
     Elected
     (Message Tally)
     (Observe (LaterThanT Int))
     (LaterThanT Int)))

(define-type-alias τc
  (U CandComms
     VoterComms
     LeaderComms))

(define (spawn-candidate [name Name] [tax-rate TaxRate] [threshold Threshold])
  (spawn CandComms
    (start-facet cand
      (printf "Candidate ~a has entered the race!\n" name)
      (assert (candidate name tax-rate))
      (on (message (tally name $region $vote-count))
          (when (< vote-count threshold)
            (stop cand))))))


;; Name TaxRate Threshold -> Candidate
(define (spawn-stubborn-candidate [name Name] [tax-rate TaxRate] [threshold Threshold])
  (spawn (U CandComms (Observe Candidate))
    (start-facet be-stubborn
      (printf "Candidate ~a is going to be stubborn!\n" name)
      (on start (spawn-candidate name tax-rate threshold))
      (on (retracted (candidate name tax-rate))
          (spawn-candidate name tax-rate threshold)))))

;; Find the name of the highest ranked candidate
(define (ranked-vote [candidates (Set Candidate)] [round-candidates (List Name)] [rank-candidates (→fn (List Candidate) (List Candidate))]
                     -> Name)
  (define priorities (rank-candidates (set->list candidates)))
  ;; if no match found, voting-for is #f. Assume that doesn't happen.
  (define maybe-vote
    (for/first ([candidate (in-list priorities)]
                #:when (member? (candidate-name candidate) round-candidates))
      (candidate-name candidate)))
  (match maybe-vote
    [(some $v) v]
    [none (error "assume this doesn't happen")]))

(define (voter-behavior [voting-procedure (proc ID Region (List Name) (Set Candidate) → ★/t #:endpoints ((Shares Vote)))]
                        [name Name]
                        [region Region]
                        [register? Bool])
    (start-facet _
      (printf "Voter ~a is intending to register in region ~a!\n" name region)
      (define/query-set candidates (candidate $name:Name $tr:TaxRate) (candidate name tr))
      (when register? (assert (voter name region)))

      (during (round $id:ID region ($ round-candidates (List Name)))
        (voting-procedure id region round-candidates (ref candidates)))))

(define (voter-skeleton [voting-procedure (proc ID Region (List Name) (Set Candidate) → ★/t #:endpoints ((Shares Vote)))]
                        [name Name]
                        [region Region]
                        [register? Bool])
  (spawn VoterComms (voter-behavior voting-procedure name region register?)))

(define (spawn-voter [name Name] [region Region] [rank-candidates (→fn (List Candidate) (List Candidate))])
  (define (voting-procedure [id ID] [region Region] [round-candidates (List Name)] [candidates (Set Candidate)])
       (assert (vote name id region (ranked-vote candidates round-candidates rank-candidates))))

  (voter-skeleton voting-procedure name region #t))

(define (spawn-greedy-voter [name Name] [region Region] [first-candidate Name] [second-candidate Name])
  (define (voting-procedure [id ID] [region Region] [round-candidates (List Name)] [candidates (Set Candidate)])
       (if (member? first-candidate round-candidates)
         (assert (vote name id region first-candidate))
         (assert (vote name id region (first round-candidates))))
       (when (member? second-candidate round-candidates)
         (assert (vote name id region second-candidate))))

  (voter-skeleton voting-procedure name region #t))

(define (spawn-stubborn-voter [name Name] [region Region] [invalid-candidate Name])
  (define (voting-procedure [id ID] [region Region] [round-candidates (List Name)] [candidates (Set Candidate)])
       (assert (vote name id region invalid-candidate)))

  (voter-skeleton voting-procedure name region #t))

(define (spawn-leaving-voter [name Name] [region Region] [rank-candidates (→fn (List Candidate) (List Candidate))] [round-limit Int])
  (spawn VoterComms
    (start-facet count-rounds
      (field [round-count Int 0])

      (define (voting-procedure [id ID] [region Region] [round-candidates (List Name)] [candidates (Set Candidate)])
        (set! round-count (add1 (ref round-count)))
        (assert (vote name id region (ranked-vote candidates round-candidates rank-candidates))))

      (begin/dataflow
        (when (> (ref round-count) round-limit)
          (stop count-rounds)))

      (on start (voter-behavior voting-procedure name region #t)))))

;; Name [[Listof Candidate] -> [Listof Candidate]] Number -> Voter
(define (spawn-late-joining-voter [name Name] [region Region] [rank-candidates (→fn (List Candidate) (List Candidate))] [round-limit Int])
  (spawn VoterComms
    (start-facet count-rounds
      (field [round-count Int 0]
             [registered? Bool #f])

      (begin/dataflow
        (when (ref registered?)
          (start-facet reg (assert (voter name region)))))

      (define (voting-procedure [id ID] [region Region] [round-candidates (List Name)] [candidates (Set Candidate)])
        ;; Can also write this as the voter assertion with a #:when, but that is less clear I think
        (when (and (not (ref registered?)) (>= (ref round-count) round-limit))
          (set! registered? #t))
        (when (ref registered?)
          (assert (vote name id region (ranked-vote candidates round-candidates rank-candidates)))))

      (on start (voter-behavior voting-procedure name region #f)))))

;; Name [[Listof Candidate] -> [Listof Candidate]] -> Voter
(define (spawn-not-registered-voter [name Name] [region Region] [rank-candidates (→fn (List Candidate) (List Candidate))])
  (define (voting-procedure [id ID] [region Region] [round-candidates (List Name)] [candidates (Set Candidate)])
    (assert (vote name id region (ranked-vote candidates round-candidates rank-candidates))))

  (voter-skeleton voting-procedure name region #f))

(define (spawn-sleepy-voter [name Name] [region Region])
  (define (voting-procedure [id ID] [region Region] [round-candidates (List Name)] [candidates (Set Candidate)])
    #f)

  (voter-skeleton voting-procedure name region #t))

;; Region -> Leader
(define (spawn-leader [region Region])
  (spawn LeaderComms
    (start-facet vote-leader
      (printf "The Vote Leader for region ~a has joined the event!\n" region)
      (define/query-set voters (voter $name region) name)
      (define/query-set candidates (candidate $name _) name)

      (define (run-round [current-cands (Set Name)] [current-voters (Set Name)])
        (printf "still in the running: ~a\n" current-cands)
        (define round-id (gensym 'round))
        (start-facet run-a-round
          (field [valid-voters (Set Name) current-voters]
                 [still-in-the-running (Set Name) current-cands]
                 [voter-to-candidate (Hash Name Name) (hash)])

          (define (invalidate-voter [voter Name])
            (printf "Voter ~a in region ~a is now an invalid voter!\n" voter region)
            (set! voter-to-candidate (hash-remove (ref voter-to-candidate) voter))
            (set! valid-voters (set-remove (ref valid-voters) voter)))

          (printf "Candidates still in the running in ~a for region ~a: ~a\n" round-id region (ref still-in-the-running))
          (assert (round round-id region (set->list (ref still-in-the-running))))

          (on (retracted (voter $name region))
              (when (set-member? (ref valid-voters) name)
                (invalidate-voter name)))

          (on (retracted (candidate $name _))
              (printf "Candidate ~a in region ~a is now invalid!\n" name region)
              (when (set-member? (ref still-in-the-running) name)
                (set! still-in-the-running (set-remove (ref still-in-the-running) name))))

          (on (asserted (vote $who round-id region $for))
              ;; should the voter not be eliminated if they're not valid?
              (when (set-member? (ref valid-voters) who)
                (cond
                  [(or (hash-has-key? (ref voter-to-candidate) who)
                       (not (set-member? current-cands for)))
                   (invalidate-voter who)]
                  [else
                    (printf "Voter ~a has voted for candidate ~a in ~a in region ~a!\n" who for round-id region)
                    (set! voter-to-candidate (hash-set (ref voter-to-candidate) who for))])))

          (on start
            (start-facet wait
              (define one-sec-from-now (get-one-second-from-now))

              (on (asserted (later-than one-sec-from-now))
                  (printf "Timeout reached on this round!\n")
                  (define new-voters (for/set ([voter (ref valid-voters)]
                                               #:when (hash-has-key? (ref voter-to-candidate) voter))
                                       voter))
                  (set! valid-voters new-voters))))

          (begin/dataflow
            (define votes
              (for/fold ([votes (Hash Name Int) (hash)])
                        ([(voter cand) (ref voter-to-candidate)])
                (hash-update/failure votes cand add1 0)))

            (define num-voters (set-count (ref valid-voters)))
            (define num-voted (for/sum ([votes (in-hash-values votes)])
                                votes))

            (when (= num-voters num-voted)
              (printf "Tallying has begun for ~a in region ~a!\n" round-id region)
              (define front-runner (argmax (lambda ([n Name]) (hash-ref/failure votes n 0))
                                           (set->list (ref still-in-the-running))))
              (define their-votes (hash-ref/failure votes front-runner 0))
              ;; ASSUME: we're OK running a final round with just a single candidate
              (cond
                [(> their-votes (/ num-voters 2))
                (printf "Candidate ~a has been elected in region ~a at round ~a!\n" front-runner region round-id)
                (stop run-a-round
                  (start-facet over
                    (assert (elected front-runner region))))]
                [else
                  (for ([candidate (ref candidates)])
                    (send! (tally candidate region (hash-ref/failure votes candidate 0))))

                  (define loser (argmin (lambda ([n Name]) (hash-ref/failure votes n 0))
                                        (set->list (ref still-in-the-running))))
                  (printf "The front-runner for ~a in region ~a is ~a! The loser is ~a!\n" round-id region front-runner loser)
                  (define next-candidates (set-intersect (ref candidates) (set-remove (ref still-in-the-running) loser)))
                  (stop run-a-round #;TODO #;(run-round next-candidates (ref valid-voters)))])))))


      (define one-second-from-now (get-one-second-from-now))
      (on start
        (start-facet wait
          (on (asserted (later-than one-second-from-now))
              ;; ASSUME: at least one candidate and voter at this point
              (printf "The race has begun in region ~a!\n" region)
              (stop wait (run-round (ref candidates) (ref voters)))))))))
#|

;; Name -> [[Listof Candidate] -> [Listof Candidate]]
(define (stupid-sort cand-name)
  (λ (candidates)
     (define candidate? (findf (λ (cand) (string=? cand-name (candidate-name cand))) candidates))
     (if candidate?
       (cons candidate? (remove candidate? candidates))
       candidates)))

;; -> Manager
(define (spawn-manager regions)
  (spawn
    (field [caucus-results (hash)])
    (on-start (for ([region regions]) (spawn-leader region)))

    (on (asserted (elected $name $region))
        (caucus-results (hash-update (caucus-results) name add1 0))
        ;; FIXME name
        (define num-results
          (foldl (λ (num acc) (+ acc num)) 0 (hash-values (caucus-results))))
        (when (= num-results (length regions))
          (define-values (winning-candidate _)
            (for/fold ([best-cand #f] [their-votes -1])
                      ([(cand-name cand-votes) (in-hash (caucus-results))])
              (if (< cand-votes their-votes)
                (values best-cand their-votes)
                (values cand-name cand-votes))))
          (stop-current-facet 
            (react 
              (printf "The winner of the election is ~a!\n" winning-candidate)
              (assert (outbound (winner winning-candidate)))))))))


;; Assumptions made about the manager:
;; Every elected announcement is valid
;; The manager is up and running and properly configured before voting begins (there is now 'begin voting' announcement made by the manager)
;; no leader makes multiple elected announcements

;; Candidates do actually drop per caucus. Nice.
|#

(run-ground-dataspace (U τc (Observe Candidate))
  (spawn-candidate "Biden" 1000 1000000)
  (spawn-stubborn-candidate "Trump" 1 100000000000)
  (spawn-greedy-voter "Sam" "Texas" "Biden" "Trump"))
