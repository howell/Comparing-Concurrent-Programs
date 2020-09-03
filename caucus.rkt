#lang syndicate/actor
(require racket/set)
(require [only-in racket argmax argmin identity first filter-not])
(require/activate syndicate/drivers/timestate)

;; a Name is a (caucus-unique) String

;; an ID is a unique Symbol

;; a TaxRate is a Number

;; a Threshold is a Number

;; a VoteCount is a Number

;; a Region is a (caucus-unique) String

;; a Voter is a (voter Name Region)
(assertion-struct voter (name region))

;; a Leave is a (leave Name)
(assertion-struct leave (name))

;; a Vote is a (voter Name ID Region Name), where the first name is the voter and the
;; second is who they are voting for
(assertion-struct vote (voter round region candidate))

;; a Round is a (round ID Region (Listof Name))
(assertion-struct round (id region candidates))

;; a Candidate is a (candidate Name TaxRate Threshold)
(assertion-struct candidate (name tax-rate))

;; a Tally is a (tally Name Region VoteCount)
(assertion-struct tally (name region vote-count))

;; an Elected is a (elected Name Region)
(assertion-struct elected (name region))

;; a ValidRegion is a (valid-region Region)
(assertion-struct valid-region (region))

;; a Winner is a (winner Name)
(assertion-struct winner (name))

;; There are four actor roles:
;; - Caucus Leaders
;; - Candidates
;; - Voters
;; - Region Manager

;; There are two presence-oriented conversations:
;; Voters announce their presence through a Voter assertion
;; Candidates announce their presence through a Candidate assertion

;; There is a conversation about voting:

;; The Caucus Leader initiates a round of voting by making a Round assertion
;; with a new ID and the list of candidates still in the running. Voters vote in
;; a certain round by making a Vote assertion with the corresponding round ID,
;; their name, and the name of the candidate they are voting for.

;; There is an election results conversation, where the Caucus Leader announces
;; the winner with an Elected assertion

;; There is a conversation about the winner for a region. Each region is identified by
;; a name that voters explicitly register for. When a candidate is elected by a caucus,
;; they announce the election of that candidate and alert the region manager, who then
;; closes voting and declares a final winner when one of the candidates has received 
;; a plurality of the votes.

;; There are multiple bad actors.
;; - Stubborn Candidate: a candidate who tries to re-enter the race after having been dropped --> could be implemented differently than the way I have it now
;; - Greedy Voter: A voter that tries voting twice when possible.
;; - Stubborn Voter: A voter that always votes for the same candidate, even if that candidate isn't eligible.
;; - Leaving Voter: A voter who leaves the caucus before it has formally elected a winner.
;; - Late-Joining Voter: A voter who joins voting late (i.e. isn't available to vote for the first round).
;; - Unregistered Voter: A voter who votes without being registered to vote.

;; TODO
;; 3. Make sure everybody's ready for diffing
;; 4. Do the diffing

;; Name TaxRate Threshold -> Candidate
(define (spawn-candidate name tax-rate threshold)
  (spawn
    (printf "Candidate ~a has entered the race!\n" name)
    (assert (candidate name tax-rate))
    (on (message (tally name $region $vote-count))
        (when (< vote-count threshold)
          (stop-current-facet)))))

;; Name TaxRate Threshold -> Candidate
(define (spawn-stubborn-candidate name tax-rate threshold)
  (spawn
    (printf "Stubborn candidate ~a has entered the race!\n" name)
    (assert (candidate name tax-rate))
    (on (message (tally name $region $vote-count))
        (when (< vote-count threshold)
          (printf "Candidate ~a is trying to re-enter the race!\n" name)
          (stop-current-facet (spawn-stubborn-candidate name tax-rate threshold))))))

;; Assert a vote for a candidate based on a voter's preference for a list of candidates
;; [Listof Candidate] [Listof Name] [[Listof Candidate] -> [Listof Candidate]] Name ID Region -> Vote
(define (ranked-vote candidates round-candidates rank-candidates)
  (define priorities (rank-candidates (set->list candidates)))
  ;; if no match found, voting-for is #f. Assume that doesn't happen.
  (for/first ([candidate (in-list priorities)]
              #:when (member (candidate-name candidate) round-candidates))
              (candidate-name candidate)))

(define (voter-skeleton voting-procedure name region register?)
  (spawn
    ;; a print
    (define/query-set candidates (candidate $name $tr) (candidate name tr))
    (when register? (assert (voter name region)))

    (during (round $id region $round-candidates)
            (voting-procedure id region round-candidates candidates))))

(define (spawn-voter name region rank-candidates)
  (define (voting-procedure id region round-candidates candidates)
       (assert (vote name id region (ranked-vote (candidates) round-candidates rank-candidates))))

  (voter-skeleton voting-procedure name region #t))

(define (spawn-greedy-voter name region first-candidate second-candidate)
    (define (voting-procedure id region round-candidates candidates)
       (if (member first-candidate round-candidates)
         (assert (vote name id region first-candidate))
         (assert (vote name id region (first round-candidates))))
       (when (member second-candidate round-candidates)
         (assert (vote name id region second-candidate))))

  (voter-skeleton voting-procedure name region #t))

(define (spawn-stubborn-voter name region invalid-candidate)
    (define (voting-procedure id region round-candidates candidates)
       (assert (vote name id region invalid-candidate)))

  (voter-skeleton voting-procedure name region #t))

(define (spawn-leaving-voter name region rank-candidates round-limit)
  (define round-count 0)
    (define (voting-procedure id region round-candidates candidates)
       (when (> round-count round-limit)
         (raise "leaving voter exit")) ;; NOTE this doesn't work
       (set! round-count (add1 round-count))
       (assert (vote name id region (ranked-vote (candidates) round-candidates rank-candidates))))

  (voter-skeleton voting-procedure name region #t))

;; Name [[Listof Candidate] -> [Listof Candidate]] Number -> Voter
(define (spawn-late-joining-voter name region rank-candidates round-limit)
  (define round-count 0)
  (define registered? #f)
    (define (voting-procedure id region round-candidates candidates)
       ;; Can also write this as the voter assertion with a #:when, but that is less clear I think
       (when (and (not registered?) (>= round-count round-limit))
         (begin
           (set! registered? #t)
           (assert (voter name region))))
       (assert #:when registered? (vote name id region (ranked-vote (candidates) round-candidates rank-candidates))))

    (voter-skeleton voting-procedure name region #f))

;; Name [[Listof Candidate] -> [Listof Candidate]] -> Voter
(define (spawn-not-registered-voter name region rank-candidates)
    (define (voting-procedure id region round-candidates candidates)
       (assert (vote name id region (ranked-vote (candidates) round-candidates rank-candidates))))

  (voter-skeleton voting-procedure name region #f))

(define (spawn-sleepy-voter name region)
  (define voting-procedure (λ (_a _b _c _d) #f))

  (voter-skeleton voting-procedure name region #t))

;; Region -> Leader
(define (spawn-leader region)
  (spawn
    (printf "The Vote Leader for region ~a has joined the event!\n" region)
    (assert (valid-region region))
    (define/query-set voters (voter $name region) name)
    (define/query-set candidates (candidate $name _) name)

    ;; [Listof Name] -> Elected
    (define (run-round current-cands current-voters)
      (printf "still in the running: ~a\n" current-cands)
      (define round-id (gensym 'round))
      (react
        (field [valid-voters current-voters]
               [still-in-the-running current-cands]
               [voter-to-candidate (hash)]
               [votes (hash)])

        (define (invalidate-voter voter)
          (printf "Voter ~a in region ~a is now an invalid voter!\n" voter region)
          (when (hash-has-key? (voter-to-candidate) voter)
            (votes (hash-update (votes) (hash-ref (voter-to-candidate) voter) sub1)))
          (valid-voters (set-remove (valid-voters) voter)))

        (printf "Candidates still in the running in ~a for region ~a: ~a\n" round-id region (still-in-the-running))
        (assert (round round-id region (set->list (still-in-the-running))))

        (on (retracted (voter $name region)) 
            (when (set-member? (valid-voters) name) 
              (invalidate-voter name)))

        (on (retracted (candidate $name _))
            (printf "Candidate ~a in region ~a is now invalid!\n" name region)
            (when (set-member? (still-in-the-running) name)
              (still-in-the-running (set-remove (still-in-the-running) name))))

        (on (asserted (vote $who round-id region $for))
            ;; should the voter not be eliminated if they're not valid?
            (when (set-member? (valid-voters) who)
              (cond
                [(or (hash-has-key? (voter-to-candidate) who) 
                     (not (set-member? current-cands for))) 
                 (invalidate-voter who)]
                [else 
                  (printf "Voter ~a has voted for candidate ~a in ~a in region ~a!\n" who for round-id region)
                  (voter-to-candidate (hash-set (voter-to-candidate) who for))
                  (votes (hash-update (votes) for add1 0))])))

        (on-start
          (react
            (field [one-second-from-now (+ 1000 (current-inexact-milliseconds))])
            (on (asserted (later-than (one-second-from-now)))
                (printf "Timeout reached on this round!\n")
                (valid-voters
                  (list->set (filter (λ (voter) (hash-has-key? (voter-to-candidate) voter)) (set->list (valid-voters))))))))

        (begin/dataflow
          (define num-voters (set-count (valid-voters)))
          (define num-voted (for/sum ([votes (in-hash-values (votes))])
                              votes))

          (when (= num-voters num-voted)
            (printf "Tallying has begun for ~a in region ~a!\n" round-id region)
            (define front-runner (argmax (lambda (n) (hash-ref (votes) n 0))
                                        (set->list (still-in-the-running))))
            (define their-votes (hash-ref (votes) front-runner 0))
            ;; ASSUME: we're OK running a final round with just a single candidate
            (cond
              [(> their-votes (/ num-voters 2))
              (printf "Candidate ~a has been elected in region ~a at round ~a!\n" front-runner region round-id)
              (stop-current-facet
                (react
                  (assert (elected front-runner region))))]
              [else
                (for ([candidate (in-set (candidates))])
                  (send! (tally candidate region (hash-ref (votes) candidate 0))))

                (define loser (argmin (lambda (n) (hash-ref (votes) n 0))
                                    (set->list (still-in-the-running))))
                (printf "The front-runner for ~a in region ~a is ~a! The loser is ~a!\n" round-id region front-runner loser)
                (define next-candidates (set-intersect (candidates) (set-remove (still-in-the-running) loser)))
                (stop-current-facet (run-round next-candidates (valid-voters)))])))))


    (define one-second-from-now (+ 1000 (current-inexact-milliseconds)))
    (on-start
      (react
        (on (asserted (later-than one-second-from-now))
            ;; ASSUME: at least one candidate and voter at this point
            (printf "The race has begun in region ~a!\n" region)
            (stop-current-facet (run-round (candidates) (voters))))))))

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

;; Region Manager
(spawn-manager '("region1" "region2" "region3" "region4" "region5"))

;; First Caucus: Region 1
(spawn-voter "Rax" "region1" (stupid-sort "Tulsi"))
(spawn-voter "Bax" "region1" (stupid-sort "Tulsi"))
(spawn-voter "Tax" "region1" (stupid-sort "Tulsi"))
(spawn-voter "Lax" "region1" (stupid-sort "Tulsi"))
(spawn-voter "Fax" "region1" (stupid-sort "Tulsi"))
(spawn-voter "Kax" "region1" (stupid-sort "Tulsi"))
(spawn-voter "Joe" "region1" (stupid-sort "Bernie"))
(spawn-voter "Moe" "region1" (stupid-sort "Biden"))
(spawn-voter "Zoe" "region1" (stupid-sort "Bernie"))
(spawn-voter "Doe" "region1" (stupid-sort "Bernie"))
(spawn-voter "Wow" "region1" (stupid-sort "Bernie"))
(spawn-voter "Bob" "region1" (stupid-sort "Bernie"))
(spawn-greedy-voter "Abc" "region1" "Tulsi" "Biden")
(spawn-greedy-voter "Def" "region1" "Tulsi" "Biden")
(spawn-greedy-voter "Ghi" "region1" "Tulsi" "Biden")
(spawn-greedy-voter "Jkl" "region1" "Biden" "Tulsi")
(spawn-greedy-voter "Mno" "region1" "Biden" "Tulsi")
(spawn-greedy-voter "Pqr" "region1" "Biden" "Tulsi")
(spawn-stubborn-voter "Xyz" "region1" "Matthias")
(spawn-sleepy-voter "G" "region1")
(spawn-leaving-voter "AB1" "region1" (stupid-sort "1") -1)
(spawn-leaving-voter "AB2" "region1" (stupid-sort "1") -1)
(spawn-leaving-voter "AB3" "region1" (stupid-sort "1") -1)
(spawn-leaving-voter "AB4" "region1" (stupid-sort "2") -1)
(spawn-leaving-voter "AB5" "region1" (stupid-sort "2") -1)
(spawn-leaving-voter "AB6" "region1" (stupid-sort "2") -1)
(spawn-leaving-voter "AB7" "region1" (stupid-sort "2") -1)
(spawn-leaving-voter "AB8" "region1" (stupid-sort "3") -1)
(spawn-leaving-voter "AB9" "region1" (stupid-sort "3") -1)
(spawn-leaving-voter "AB0" "region1" (stupid-sort "3") -1)
(spawn-late-joining-voter "BA0" "region1" (stupid-sort "Tulsi") 1)
(spawn-late-joining-voter "BA1" "region1" (stupid-sort "Tulsi") 1)
(spawn-late-joining-voter "BA2" "region1" (stupid-sort "Tulsi") 1)
(spawn-late-joining-voter "BA3" "region1" (stupid-sort "Tulsi") 1)
(spawn-late-joining-voter "BA4" "region1" (stupid-sort "Tulsi") 1)
(spawn-late-joining-voter "BA5" "region1" (stupid-sort "Tulsi") 1)
(spawn-late-joining-voter "BA6" "region1" (stupid-sort "Tulsi") 1)
(spawn-late-joining-voter "BA7" "region1" (stupid-sort "Tulsi") 1)
(spawn-late-joining-voter "BA8" "region1" (stupid-sort "Tulsi") 1)
(spawn-late-joining-voter "BA9" "region1" (stupid-sort "Tulsi") 1)
(spawn-not-registered-voter "XY0" "region1" (stupid-sort "Tulsi"))
(spawn-not-registered-voter "XY1" "region1" (stupid-sort "Tulsi"))
(spawn-not-registered-voter "XY2" "region1" (stupid-sort "Tulsi"))
(spawn-not-registered-voter "XY3" "region1" (stupid-sort "Tulsi"))
(spawn-not-registered-voter "XY4" "region1" (stupid-sort "Tulsi"))
(spawn-not-registered-voter "XY5" "region1" (stupid-sort "Tulsi"))
(spawn-not-registered-voter "XY6" "region1" (stupid-sort "Tulsi"))
(spawn-not-registered-voter "XY7" "region1" (stupid-sort "Tulsi"))
(spawn-not-registered-voter "XY8" "region1" (stupid-sort "Tulsi"))
(spawn-not-registered-voter "XY9" "region1" (stupid-sort "Tulsi"))

;; Second Caucus: Region 2
(spawn-voter "AAA" "region2" (stupid-sort "Bernie"))
(spawn-voter "AAB" "region2" (stupid-sort "Donkey"))
(spawn-voter "AAC" "region2" (stupid-sort "Donkey"))
(spawn-voter "AAD" "region2" (stupid-sort "Biden"))
(spawn-voter "AAE" "region2" (stupid-sort "Biden"))
(spawn-voter "AAF" "region2" (stupid-sort "Biden"))
(spawn-voter "AAG" "region2" (stupid-sort "Biden"))
(spawn-voter "AAH" "region2" (stupid-sort "Biden"))
(spawn-voter "AAI" "region2" (stupid-sort "Biden"))
(spawn-voter "AAJ" "region2" (stupid-sort "Biden"))
(spawn-voter "AAK" "region2" (stupid-sort "Biden"))

;; Third Caucus: Region 3
(spawn-voter "AAL" "region3" (stupid-sort "Bernie"))
(spawn-voter "AAM" "region3" (stupid-sort "Bernie"))
(spawn-voter "AAN" "region3" (stupid-sort "Bernie"))
(spawn-voter "AAO" "region3" (stupid-sort "Bernie"))
(spawn-voter "AAP" "region3" (stupid-sort "Bernie"))
(spawn-voter "AAQ" "region3" (stupid-sort "Bernie"))
(spawn-voter "AAR" "region3" (stupid-sort "Bernie"))
(spawn-voter "AAS" "region3" (stupid-sort "Bernie"))
(spawn-voter "AAT" "region3" (stupid-sort "Biden"))
(spawn-voter "AAU" "region3" (stupid-sort "Biden"))
(spawn-voter "AAV" "region3" (stupid-sort "Biden"))

;; Fourth Caucus: Region 4
(spawn-voter "AAL" "region4" (stupid-sort "Biden"))
(spawn-voter "AAM" "region4" (stupid-sort "Biden"))
(spawn-voter "AAN" "region4" (stupid-sort "Biden"))
(spawn-voter "AAO" "region4" (stupid-sort "Biden"))
(spawn-voter "AAP" "region4" (stupid-sort "Biden"))
(spawn-voter "AAQ" "region4" (stupid-sort "Biden"))
(spawn-voter "AAR" "region4" (stupid-sort "Biden"))
(spawn-voter "AAS" "region4" (stupid-sort "Biden"))
(spawn-voter "AAT" "region4" (stupid-sort "Biden"))
(spawn-voter "AAU" "region4" (stupid-sort "Biden"))
(spawn-voter "AAV" "region4" (stupid-sort "Biden"))

;; Fifth Caucus: Region 5
(spawn-voter "AAW" "region5" (stupid-sort "Biden"))
(spawn-voter "AAX" "region5" (stupid-sort "Biden"))
(spawn-voter "AAY" "region5" (stupid-sort "Biden"))
(spawn-voter "AAZ" "region5" (stupid-sort "Biden"))
(spawn-voter "ABA" "region5" (stupid-sort "Biden"))
(spawn-voter "ABB" "region5" (stupid-sort "Biden"))
(spawn-voter "ABC" "region5" (stupid-sort "Biden"))
(spawn-voter "ABD" "region5" (stupid-sort "Biden"))
(spawn-voter "ABE" "region5" (stupid-sort "Biden"))
(spawn-voter "ABF" "region5" (stupid-sort "Biden"))
(spawn-voter "ABG" "region5" (stupid-sort "Biden"))

;; Candidates
(spawn-candidate "Bernie" 50 2)
(spawn-candidate "Biden" 25 1)
(spawn-candidate "Tulsi" 16 300)
(spawn-stubborn-candidate "Donkey" 0 1000)

(module+ main

  )
