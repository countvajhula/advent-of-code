#lang racket

(require qi
         relation
         (only-in math
                  nth-prime)
         "util.rkt")

(define input
  (~>> ((read-input-file "4.txt"))
       parse-input
       (cut null)
       ->list))

(define numbers
  (~>> (input)
       (feedback 2 first)
       (string-split _ ",")
       (map ->number)))

(define boards
  (~>> (input) rest (map join)))

(struct annotated-board (board state)
  #:transparent)

(define boardstream (map (☯ (annotated-board 1)) boards))

(define primes
  (~> (0)
      (feedback 25
                (then (block 1))
                (-< (~> 1> add1)
                    (~> 1> nth-prime)
                    (block 1))) X ▽))

(define magic-numbers
  ;; product of primes corresponding to each row or column
  ;; of the 5 x 5 board. If a board has a checksum/state matching
  ;; any of these, or any multiple of these, it is a winning board.
  (~> (primes)
      (-< (~> (cut-by 5 _) △)
          (by 5 _)
          (~> (drop 1) (by 5 _))
          (~> (drop 2) (by 5 _))
          (~> (drop 3) (by 5 _))
          (~> (drop 4) (by 5 _)))
      (>< product)
      ▽))

(define (incorporate-number aboard number)
  (~> (aboard)
      (~> (-< annotated-board-board
              (~> (-< annotated-board-state
                      (~> annotated-board-board
                          (index-of number)
                          (if _ (list-ref primes _) 1))) *))
          annotated-board)))

(define-flow has-board-won?
  (~> annotated-board-state
      (-< (gen magic-numbers) _)
      (△ (~> X remainder))
      (any (= 0))))

(define-flow (called-numbers winning-number)
  (~> (-< (~>> (index-of numbers) add1 (clos take))
          (gen numbers))
      apply))

(define-flow bingo-round
  (~> (bundle (1) (~>> (list-ref numbers) (clos incorporate-number)) _)
      ><))

(define (bingo boardstream while-condition)
  (~> (boardstream) △ (-< (gen while-condition) ; the first 3 are the "control" inputs
                          (gen (☯ (-< (~> 2> last annotated-board-board)
                                      (~>> 1> sub1 (list-ref numbers) called-numbers))))
                          (gen (☯ (-< (~> 1> add1)
                                      (~> (bundle (2)
                                                  (clos (~> (-< (gen append) (group 1 _ list)) apply))
                                                  bingo-round)
                                          (-< (gen has-board-won?)
                                              1>
                                              (gen (☯ _))
                                              (block 1))
                                          sieve))))
                          0 '() _) ; the data inputs
      feedback))

(define-flow (score winning-board called-numbers)
  (~> (-< (~> 2> last)
          (~> (>< ->set)
              set-subtract
              ->list
              sum))
      *))

;; Questionable, but interesting: we parametrize the loop in a purely
;; functional way that "knows" about the structure of flows that it has
;; not seen. This allows us to use the same flow for both part 1 and
;; part 2. But this is questionable because changing the structure of
;; that flow would require changing these conditions too - they are
;; coupled. A "functional" kind of "spooky action at a distance"!
(define-flow part1
  (~> (bingo (☯ (~> 2> empty?)))
      score))

(define-flow part2
  (~> (bingo (☯ (~> (block 1 2) count (> 0))))
      score))

(part1 boardstream)
(part2 boardstream)
