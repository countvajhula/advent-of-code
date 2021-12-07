#lang racket

(provide read-input
         read-input-file
         parse-input)

(require qi
         sugar)

(define-flow read-input
  port->lines)

(define-flow read-input-file
  (~> open-input-file read-input))

(define-flow singleton?
  (and (not null?) (~> rest empty?)))

(define (maybe-to-integer v)
  (with-handlers ([exn:fail? (λ (e) v)])
    (->int v)))

(define-flow parse-input
  (~> △ (>< (~>> string-split (map (☯ maybe-to-integer)) (if singleton? first _))) ▽))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (check-equal? (last (parse-input (read-input-file "1.txt"))) 8026)
  (check-equal? (first (parse-input (read-input (open-input-string "hello there\nblah blah"))))
                '("hello" "there")))
