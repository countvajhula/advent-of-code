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

(define-flow (matches-rule? rule pair)
  (~> (== first _) equal?))

(define-flow (apply-rule rule pair)
  (~> (when matches-rule? apply-rule*)))

(define-flow (apply-rule* rule pair)
  (~> (== second _) insert-symbol))

(define-flow (insert-symbol symbol pair)
  (~> (== ->char (~> ->list △)) (select 2 1 3) ▽ ->string))

(define-flow (pairify polymer)
  (~>> (infixes 2) ->list △))

(define-flow (trim-last pair*)
  (~> ->list △ (select 1 2) ▽ ->string))

(define-flow (apply-rules rules pair)
  ;; truncates the last char after applying the rule
  (~> (△ apply-rule) trim-last))

(define-flow last-symbol
  (~> ->list last ->string))

(define-flow (polymerize rules polymer)
  (-< (gen rules)
      (~> (-< (~> (== (clos apply-rules) pairify)
                  ><)
              (~> 2> last-symbol))
          ▽
          join)))

(define (iterate rules polymer N)
  (~> (1 rules polymer)
      (feedback N
                (then (block 1 2))
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

(define part1
  (~> (rules template 40)
      iterate
      counts
      drop-zeroes
      max-and-min
      (-< first last)
      (>< second)
      -))

part1
