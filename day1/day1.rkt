(define in (open-input-file "input"))

(define (convert-to-list freqchange)
  (let ([line (read-line in)])
    (if (string? line)
        (convert-to-list (append freqchange (list (string->number line))))
        freqchange)))

(define list-of-freq (convert-to-list empty))

(define (part1)
  (let ([line (read-line in)])
    (if (string? line)
        (+ (string->number line) (part1))
        0)))

(define (part2 freqs)
  (let ([line (read-line in)])
    (if (string? line)
        (let* ([next-freq (+ (last freqs) (string->number line))])
          (if (member next-freq freqs)
              (format "found: ~a" next-freq)
              (part2 (append freqs (list next-freq)))))
        (format "eof: ~a" (last freqs)))))


(define (part3 inputs freqs)
  (if (empty? inputs)
     (part3 list-of-freq freqs)
      ;(format "eof: ~a" (last freqs))
      (let ([next-freq (+ (last freqs) (first inputs))])
        (if (member next-freq freqs)
            (format "found: ~a" next-freq)
            (part3 (rest inputs) (append freqs (list next-freq)))))))
