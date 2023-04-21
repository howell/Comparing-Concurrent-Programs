#lang typed/syndicate

(require (except-in typed/syndicate/drivers/timestate activate!)
         (prefix-in timestate: (only-in typed/syndicate/drivers/timestate activate!)))

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

(define-type-alias ManagerComms
  (U LeaderComms
     (Observe (ElectedT ★/t ★/t))
     Elected
     (Outbound Winner)))

(define-type-alias τc
  (U CandComms
     VoterComms
     LeaderComms
     ManagerComms
     TimeStateDriver))

(define (spawn-candidate [name Name] [tax-rate TaxRate] [threshold Threshold])
  (spawn #:type CandComms
    (lift+define-role candidate-impl
    (start-facet cand
      (printf "Candidate ~a has entered the race!\n" name)
      (assert (candidate name tax-rate))
      (on (message (tally name $region $vote-count))
          (when (< vote-count threshold)
            (stop cand)))))))


;; Name TaxRate Threshold -> Candidate
(define (spawn-stubborn-candidate [name Name] [tax-rate TaxRate] [threshold Threshold])
  (spawn #:type (U CandComms (Observe Candidate))
    (lift+define-role stubborn-candidate-impl
    (start-facet be-stubborn
      (printf "Candidate ~a is going to be stubborn!\n" name)
      (on start (spawn-candidate name tax-rate threshold))
      (on (retracted (candidate name tax-rate))
          (spawn-candidate name tax-rate threshold))))))

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
    (lift+define-role voter-impl
    (start-facet _
      (printf "Voter ~a is intending to register in region ~a!\n" name region)
      (define/query-set candidates (candidate $name:Name $tr:TaxRate) (candidate name tr))
      (when register? (assert (voter name region)))

      (during (round $id:ID region ($ round-candidates (List Name)))
        (voting-procedure id region round-candidates (ref candidates))))))

(define (voter-skeleton [voting-procedure (proc ID Region (List Name) (Set Candidate) → ★/t #:endpoints ((Shares Vote)))]
                        [name Name]
                        [region Region]
                        [register? Bool])
  (spawn #:type VoterComms (voter-behavior voting-procedure name region register?)))

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
  (spawn #:type VoterComms
    (start-facet count-rounds
      (field [round-count : Int 0])

      (define (voting-procedure [id ID] [region Region] [round-candidates (List Name)] [candidates (Set Candidate)])
        (set! round-count (add1 (ref round-count)))
        (assert (vote name id region (ranked-vote candidates round-candidates rank-candidates))))

      (begin/dataflow
        (when (> (ref round-count) round-limit)
          (stop count-rounds)))

      (on start (voter-behavior voting-procedure name region #t)))))

;; Name [[Listof Candidate] -> [Listof Candidate]] Number -> Voter
(define (spawn-late-joining-voter [name Name] [region Region] [rank-candidates (→fn (List Candidate) (List Candidate))] [round-limit Int])
  (spawn #:type VoterComms
    (start-facet count-rounds
      (field [round-count : Int 0]
             [registered? : Bool #f])

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

(message-struct start-round : StartRoundT (candidates voters))

;; Region -> Leader
(define (spawn-leader [region Region])
  (spawn #:type LeaderComms
    (lift+define-role leader-impl
    (start-facet vote-leader
      (printf "The Vote Leader for region ~a has joined the event!\n" region)
      (define/query-set voters (voter $name region) name)
      (define/query-set candidates (candidate $name _) name)

      (define (run-round [current-cands (Set Name)] [current-voters (Set Name)])
        (printf "still in the running: ~a\n" current-cands)
        (define round-id (gensym 'round))
        (start-facet run-a-round
          (field [valid-voters current-voters]
                 [still-in-the-running current-cands]
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

          ;;(begin/dataflow
          (define (count!)
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
                  (stop run-a-round (realize! (start-round next-candidates (ref valid-voters))))])))

          (on start
              (start-facet wait
                (define one-sec-from-now (get-one-second-from-now))

                (on (asserted (later-than one-sec-from-now))
                    (printf "Timeout reached on this round!\n")
                    (define new-voters (for/set ([voter (ref valid-voters)]
                                                 #:when (hash-has-key? (ref voter-to-candidate) voter))
                                         voter))
                    (set! valid-voters new-voters)
                    (count!))))

))

      (on (realize (start-round ($ candidates (Set Name)) ($ voters (Set Name))))
          (run-round candidates voters))

      (define one-second-from-now (get-one-second-from-now))
      (on start
        (start-facet signup-wait
          (on (asserted (later-than one-second-from-now))
              ;; ASSUME: at least one candidate and voter at this point
              (printf "The race has begun in region ~a!\n" region)
              (stop signup-wait (realize! (start-round (ref candidates) (ref voters)))))))))))

;; Name -> [[Listof Candidate] -> [Listof Candidate]]
(define (stupid-sort [cand-name Name] -> (→fn (List Candidate) (List Candidate)))
  (lambda ([candidates (List Candidate)])
    (define candidate?
      (for/first ([cand candidates]
                  #:when (string=? cand-name (candidate-name cand)))
        cand))
    (match candidate?
      [(some $c)
       (cons c (remove c candidates))]
      [none candidates])))

;; -> Manager
(define (spawn-manager [regions (List Region)])
  (spawn #:type ManagerComms
    (lift+define-role manager-impl
    (start-facet manager
      (field [caucus-results (Hash Name Int) (hash)])
      (printf "Manager online\n")
      (on start (for ([region regions]) (spawn-leader region)))

      (on (asserted (elected $name $region))
          (set! caucus-results (hash-update/failure (ref caucus-results) name add1 0))
          ;; FIXME name
          (define num-results (for/sum ([num (in-hash-values (ref caucus-results))]) num))
          (when (= num-results (length regions))
            (match-define (tuple (some $winning-candidate) _)
              (for/fold ([best-cand (Maybe Name) none]
                         [their-votes : Int -1])
                        ([(cand-name cand-votes) (ref caucus-results)])
                (if (< cand-votes their-votes)
                    (tuple best-cand their-votes)
                    (tuple (some cand-name) cand-votes))))
            (stop manager
             (start-facet announce
              (printf "The winner of the election is ~a!\n" winning-candidate)
              (assert (outbound (winner winning-candidate)))))))))))

#|

;; Assumptions made about the manager:
;; Every elected announcement is valid
;; The manager is up and running and properly configured before voting begins (there is now 'begin voting' announcement made by the manager)
;; no leader makes multiple elected announcements

;; Candidates do actually drop per caucus. Nice.
|#

(define (startup)
  ;; this is a dumb hack to make up for typed-syndicate not doing capture-actor-actions
  (spawn #:type τc
   (start-facet boot
     (on start
         ;; Region Manager
         (spawn-manager (list "region1" "region2" "region3" "region4" "region5"))

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
         (spawn-stubborn-candidate "Donkey" 0 1000)))))

(module+ main
(run-ground-dataspace τc
                      (timestate:activate!)
                      (startup)))

(define-constructor* (reset-timer))
(define (timer-mock)
  (spawn
    (start-facet TimerMock
      (define (once)
        (start-facet one
            (on (asserted (observe (later-than $t:Int)))
                (start-facet ans
                  (assert (later-than t))
                  (on (asserted (later-than t))
                      (stop one)
                      (send! (reset-timer)))))))
      (on start (once))
      (on (message (reset-timer))
          (once)))))

(module+ test
  #;(verify-actors TT manager-impl)
  #;(define-type-alias TimerMock
    (Role (timer-driver)
          (During (Observe (LaterThanT Int))
                  (Shares (LaterThanT Int)))))
  (check-deadlock-free* manager-impl
                       voter-impl
                       candidate-impl
                       TimerMock)
  )
