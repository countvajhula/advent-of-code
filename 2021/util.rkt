#lang racket

(provide read-input
         read-input-file
         parse-input
         zip-with
         zip
         unzip
         cut
         by
         cut-by
         infixes)

(require qi
         relation)

(define-flow read-input
  port->lines)

(define-flow read-input-file
  (~> open-input-file read-input))

(define-flow singleton?
  (and (not null?) (~> rest empty?)))

(define (maybe-to-integer v)
  (with-handlers ([exn:fail? (λ (e) v)])
    (->integer v)))

(define-flow parse-input
  (~> △ (>< (~>> string-split (map (☯ maybe-to-integer)) (if singleton? first _))) ▽))

(define (zip-with op . seqs)
  (if (ormap empty? seqs)
      null
      (let ([vs (map first seqs)])
        (cons (apply op vs)
              (apply zip-with op (map rest seqs))))))

(define (zip . seqs)
  (apply zip-with list seqs))

(define unzip (curry apply zip))

(define (take-while pred seq)
  (match seq
    ['() null]
    [(cons v vs)
     (if (pred v)
         (cons v (take-while pred vs))
         null)]))

(define (drop-while pred seq)
  (match seq
    ['() null]
    [(cons v vs)
     (if (pred v)
         (drop-while pred vs)
         seq)]))

(define (cut-where pred seq)
  (let ([left (take-while (☯ (not pred)) seq)]
        [right (drop-while (☯ (not pred)) seq)])
    (values left right)))

(define (cut-when pred seq)
  (if (empty? seq)
      (list null)
      (let-values ([(chunk remaining) (cut-where pred seq)])
        (match remaining
          ['() (cons chunk null)]
          [(list _ vs ...)
           (cons chunk
                 (cut-when pred vs))]))))

(define (by cnt seq)
  (if (empty? seq)
      null
      (let ([head (first seq)]
            [tail (with-handlers
                    ([exn:fail:contract?
                      (λ (exn)
                        null)])
                    (drop seq cnt))])
        (cons head (by cnt tail)))))

(define (infixes len seq)
  (if (empty? seq)
      null
      (let ([infix (with-handlers ([exn:fail:contract? false.])
                     ;; convert to list or the exception would
                     ;; be deferred here
                     (->list (take seq len)))])
        (if infix
            (cons infix (infixes len (rest seq)))
            null))))

(define (cut-by n seq)
  (by n (infixes n seq)))

(define (cut elem seq)
  (cut-when (☯ (equal? elem)) seq))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (check-equal? (last (parse-input (read-input-file "1.txt"))) 8026)
  (check-equal? (first (parse-input (read-input (open-input-string "hello there\nblah blah"))))
                '("hello" "there")))
