#lang racket

(require qi
         qi/probe
         relation
         (except-in "util.rkt"
                    infixes
                    unzip
                    zip-with
                    cut-by
                    by
                    cut
                    zip)
         seq/iso)

(define input
  (~>> ((read-input-file "14.txt"))
       parse-input
       (cut null)
       ->list))

(define template
  (~> (input) first first ->string))

(define rules
  (~>> (input) second (map (☯ (~> (-< first third) list)))))

(define-flow (apply-rule symbol pair)
  (~> (== ->char (~> ->list △)) (select 2 1 3) ▽ ->string))

(define-flow (trim-last pair*)
  (~> ->list △ (select 1 2) ▽ ->string))

(define ruleshash
  (make-hash
   (~>> (rules)
        (map (☯ (~> (-< first
                        (~> (-< second first)
                            apply-rule
                            trim-last))
                    cons))))))

(define-flow (pairify polymer)
  (~>> (infixes 2) ->list △))

(define-flow (apply-rules segment)
  ;; truncates the last char after applying the rule
  (~> (fanout 2) (hash-ref ruleshash _ _)))

(define-flow last-symbol
  (~> ->list last ->string))

(define-flow (polymerize polymer)
  (~> (-< (~> pairify (>< apply-rules))
          last-symbol) ; add back last symbol which was truncated
      ▽
      join))

(define (iterate polymer N)
  (~> (1 polymer)
      (feedback N
                (then (block 1))
                (ε (~> 1> displayln)
                   (-< (~> 1> add1)
                       (~> (block 1) polymerize))))))

(define alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(define (counts polymer)
  (~> (alphabet)
      ->list
      △
      (>< (~> (-< ->string
                  (~> (-< (clos equal?)
                          (~> (gen polymer) ->list △)) pass count))
              list))))

(define-flow max-and-min
  (~>> ▽ (sort #:key second >)))

(define-flow drop-zeroes
  (pass (~> second (not (= 0)))))

(define (reinforce N)
  (~> (template N)
      iterate
      counts
      drop-zeroes
      max-and-min
      (-< first last)
      (>< second)
      -))

(define part1 (reinforce 10))
part1
;; (define part2 (reinforce 40))
