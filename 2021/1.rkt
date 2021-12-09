#lang racket

(require qi
         "util.rkt")

(define input (read-input-file "1.txt"))

(define depths
  (parse-input input))

(define-flow init ; a list containing all but the last element
  (~> △ X (block 1) X ▽))

(define-flow differences ; part 1
  (~>> (-< rest init) (map -) △ (>< (when positive? 1)) +))

(define-flow differences-3 ; part 2
  (~>> (-< _
           rest
           (feedback 2 rest)) (zip-with +) differences))

(differences-3 depths)
