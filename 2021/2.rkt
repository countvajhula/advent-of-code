#lang racket

(require qi
         "util.rkt")

(define input (read-input-file "2.txt"))

(define course
  (parse-input input))

(define (total-in-direction direction)
  (☯ (~> (pass (~> first (equal? direction))) (>< second) +)))

(define-flow part1
  (~> △ (-< (total-in-direction "forward")
            (~> (-< (total-in-direction "down")
                    (total-in-direction "up"))
                -))
      *))

(define-flow part2
  (~> △ (>> (switch (% (~> 1> first) _)
              [(equal? "forward")
               (-< (~> (-< 1> (~> 2> third list)) append)
                   (block 1))]
              [(equal? "down")
               (-< (~> (-< 1> (~> (-< (~> 2> third) (~> 1> second)) + list)) append)
                   (block 1))]
              [(equal? "up")
               (-< (~> (-< 1> (~> (-< (~> 2> third) (~> 1> second)) - list)) append)
                   (block 1))])
            '("forward" 0 0))
      X
      (pass (~> first (equal? "forward")))
      (>< (-< (take 2)
              (~> (-< "down"
                      (~> (-< third second) *))
                  list)))
      ▽
      part1))

(part2 course)
