#lang syndicate/actor

(require "caucus.rkt")
(require/activate syndicate/drivers/timestate)

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