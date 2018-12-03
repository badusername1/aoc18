(define (make-list xc yc w h res)
  (if (= h 0)
      res
  (make-list xc yc w (- h 1) (append res (build-list w (lambda (x) (list (+ xc x) (+ yc (- h 1)))))))))
