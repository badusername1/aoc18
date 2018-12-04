(define in (open-input-file "input"))

(define (input-converter)
  (let ([line (read-line in)])
    (if (not (string? line))
        empty
        (cons (string-converter line) (input-converter)))))


(define (string-converter data)
  (rest (regexp-match* #px"[[:digit:]]+" data)))

(define (collect-coords xc yc w h)
  (if (= h 0)
      empty
      (append (build-list w (lambda (x) (list (+ xc x) (+ yc (- h 1)))))
              (collect-coords xc yc w (- h 1)))))

(define (get-collection s)
  (collect-coords (string->number (first s))
                  (string->number (second s))
                  (string->number (third s))
                  (string->number (fourth s))))

(define (intersect a b)
  (if (empty? a)
      empty
      (if (member (car a) b)
          (cons (car a) (intersect (cdr a) b))
          (intersect (cdr a) b))))

(define (match-lists a b)
  (if (empty? a)
      empty
      (remove-duplicates (append (match-each (first a) b) (match-lists (rest a) b)))))

(define (match-each current b)
  (if (empty? b)
      empty
      (if (equal? current (first b))
          (match-each current (rest b))
          (remove-duplicates (append (intersect (get-collection current)
                                                (get-collection (first b)))
                                     (match-each current (rest b)))))))

(define (part1)
  (length (match-lists raw-data raw-data)))

(define test-a (collect-coords 1 3 4 4))
(define test-b (collect-coords 3 1 4 4))

(define raw-data (input-converter))
