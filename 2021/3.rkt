#lang racket

(require qi
         relation
         "util.rkt")

(define-flow binstring->decimal
  (~>> (string-append "#b") open-input-string read))

(define-flow bitcounts
  (~> △ (-< (~> (pass (= 0)) count)
            (~> (pass (= 1)) count))))

(define-flow mode-bit
  (~> > (if _ 0 1)))

(define-flow numerify
  (map (☯ (~>> ->list (map (☯ (~> ->string ->integer))))) _))

(define-flow gamma
  (~>> unzip △ (>< (~> bitcounts mode-bit)) ▽))

(define-flow epsilon
  (~> gamma △ (>< (if (= 0) 1 0)) ▽))

(define-flow binlist->binstring
  (~> △ (>< ->string) string-append))

(define-flow part1
  (~> numerify
      (-< gamma
          epsilon)
      (>< (~> binlist->binstring binstring->decimal))
      *))

(define (scrubber rate-flo)
  (☯ (~> numerify
         (-< 0 rate-flo △) ; start the flows the loop will need
         (feedback (while (~> (block 1 2) count (> 1)))
                   (then (block 1 2)) ; get rid of "scratch tape" at the end
                   (-< (~> 1> add1) ; the first flow (column number) to the next cycle
                       (~> (-< (~> (-< 1> 2>)
                                   (clos (~> (-< (~> (select 2 1) list-ref)
                                                 (~> (select 3 1) list-ref)) =)))
                               (block 1 2))
                           pass
                           (-< (~> ▽ rate-flo) ; second flow to the next cycle (computed rate value)
                               _))))))) ; third flow to the next cycle (the actual remaining data)

(define oxygen (scrubber gamma))

(define co2 (scrubber epsilon))

(define-flow part2
  (~> (-< oxygen co2)
      (>< (~> binlist->binstring binstring->decimal))
      *))

(define input (read-input-file "3.txt"))

(part1 input)
(part2 input)
