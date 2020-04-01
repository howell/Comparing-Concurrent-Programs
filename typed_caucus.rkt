#lang typed/syndicate/roles

(define-type-alias Name String)

(define-type-alias ID Symbol)

(define-type-alias TaxRate Int)

(define-type-alias Threshold Int)

(define-type-alias VoteCount Int)

(define-type-alias Region String)

(define-constructor (candidate name tax-rate threshold)
  #:type-constructor CandidateT
  #:with Candidate (CandidateT Name TaxRate Threshold))


(define-type-alias candidate-type
  (U Candidate))

;;(: spawn-candidate (-> Name TaxRate Threshold Void))
(define (spawn-candidate [name: Name] [tax-rate: TaxRate] [threshold: Threshold]): candidate-type
;;(define (spawn-candidate name tax-rate threshold)
  (start-facet name
    (assert (candidate name tax-rate threshold))
    (on start (printf "Candidate ~a has entered the race!\n" name))))

(run-ground-dataspace candidate-type

#|
(spawn candidate-type
  (start-facet cand
    (field [name String "Biden"]
           [tax-rate Int 25]
           [threshold Int 1])
    (assert (candidate (ref name) (ref tax-rate) (ref threshold)))
    (on start
      (printf "Candidate ~a has entered the race!\n" (ref name))
      )))|#
(spawn-candidate "Biden" 25 1)
)


;;(spawn-candidate "Bernie" 50 2)
;;(spawn-candidate "Biden" 25 1)
;;(spawn-candidate "Tulsi" 16 300)

;;(module+ main

;;  )
