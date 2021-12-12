#lang racket

(require qi
         relation
         (only-in math
                  nth-prime)
         "util.rkt")

(define primes
  (~> (0)
      (feedback 25
                (then (block 1))
                (-< (~> 1> add1)
                    (~> 1> nth-prime)
                    (block 1))) X ▽))

(define magic-numbers
  (~> (primes)
      (-< (~> (cut-by 5 _) △)
          (by 5 _)
          (~> (drop 1) (by 5 _))
          (~> (drop 2) (by 5 _))
          (~> (drop 3) (by 5 _))
          (~> (drop 4) (by 5 _)))
      (>< product)
      ▽))

(struct annotated-board (board state)
  #:transparent)

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

(define (bingo boardstream num-boards)
  (~> (boardstream)
      △
      (-< 0 '() _)
      (feedback (while (~> 2> length (< num-boards)))
                (then (-< (~> 2> last annotated-board-board)
                          (~>> 1> sub1 (list-ref numbers) called-numbers)))
                (-< (~> 1> add1)
                    (~> (bundle (2)
                                (clos (~> (-< (gen append) (group 1 _ list)) apply))
                                bingo-round)
                        (-< (gen has-board-won?)
                            1>
                            (gen (☯ _))
                            (block 1))
                        sieve)))))

(define-flow (score winning-board called-numbers)
  (~> (-< (~> 2> last)
          (~> (>< ->set)
              set-subtract
              ->list
              sum))
      *))

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

(define boardstream (map (☯ (annotated-board 1)) boards))

(define-flow part2
  (~> bingo
      score))

(part2 boardstream (length boardstream))
