(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define (flatten lat)
  (cond
    ((null? lat)
     '())
    ((atom? lat) (cons lat '()))
    ((pair? (car lat))
     (cons (flatten (car lat))
	   (flatten (cdr lat))))))

