(define in (open-input-file "input"))

(define (part1)
  (sum-id (assemble-values empty) 0 0))

(define (sum-id tuples two three)
  (if (empty? tuples)
      (* two three)
      (sum-id (rest tuples)
              (+ two (first (first tuples)))
              (+ three (last (first tuples))))))

(define (assemble-values values)
  (let ([line (read-line in)])
    (if (not (string? line))
        values
        (assemble-values (append values (list (calc-category
                              (get-values (count-occurances (string->list line)
                                                            empty) empty))))))))

(define (calc-category values)
  (cond
   [(and (member 2 values) (member 3 values)) (list 1 1)]
   [(member 2 values) (list 1 0)]
   [(member 3 values) (list 0 1)]))

(define (get-values alist values)
  (if (empty? alist)
      values
      (get-values (rest alist) (append values (list (last (first alist)))))))

(define (count-occurances id occurances)
  (if (empty? id)
      occurances
      (let ([value (first id)])
        (count-occurances (rest id) (update-assoc value occurances)))))

(define (update-assoc value alist)
  (if (assoc value alist)
      (let ([new-value (list (first (assoc value alist))
                             (+ 1 (last (assoc value alist))))]
            [old-value (list value (last (assoc value alist)))])
        (list-set alist (index-of alist old-value) new-value))
      (append alist (list (list value 1)))))
