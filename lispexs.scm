;; Note: Many of these can be re-written to be much more compact with foldr, foldl and filter. I leave that as an exercise to the reader :)

;1
(define (length-recur L)
  (cond ((null? L) 0)
	(else
	  (+ 1 (length-recur (cdr L))))))

(define (length-iter L)
  (define (length-iter-helper L n)
    (cond
      ((null? L) counter)
      (else
	(length-iter-helper (cdr L) (+ counter 1)))))
  (length-iter-helper L 0))

;2
(define (snoc L a)
  (cond 
    ((null? L) (cons a '()))
    (else
      (cons (car L) 
	    (snoc (cdr L) a)))))
(define (reverse-recur L)
  (cond
    ((null? L) '())
    (else
      (snoc (reverse-recur (cdr L)) (car L)))))

(define (reverse-iter L)
  (define (reverse-iter-helper input output)
    (cond
      ((null? input) output)
      (else
	(reverse-iter-helper (cdr input) (cons (car input) output)))))
  (reverse-iter-helper L '()))

;3
(define (reverse-recur* L)
  (cond
    ((null? L) '())
    ((pair? L) 
     (snoc (reverse-recur* (car L))
	   (reverse-recur* (cdr L))))
    (else
      (snoc (revrse-recur* (cdr L)) (car L)))))

(define (reverse-iter* L)
  (define (reverse-iter*-helper input output)
  (cond
    ((null? input) output)
    ((pair? (car input))
     (reverse-iter*-helper (cdr input)
		 (cons 
		   (reverse-iter*-helper (car input) '()) 
		   output)))
    (else
      (reverse-iter*-helper (cdr input) (cons (car input) output)))))
  (reverse-iter*-helper L '()))

;4
(define (prefix-sublists L)
  (define (prefix-sublists-helper L)
    (cond ((null? L) '())
	  (else
	    (cons L (prefix-sublists-helper (cdr L))))))
  (reverse-iter* (prefix-sublists-helper (reverse-iter* L))))

;5
(define (flatten L)
  (cond ((null? L) '())
        ((atom? L) (list L))
        (else (append (flatten (car L))
                      (flatten (cdr L))))))

;6
(define (cartesian L1 L2)
  (define (cartesian-helper L1 L2 currL2 output)
    (cond ((null? L1) (reverse-iter output))
          ((null? currL2) (cartesian-helper (cdr L1) L2 L2 output))
          (else
            (cartesian-helper L1 L2 (cdr currL2) (cons (cons (car L1) (cons (car currL2) '())) output)))))
  (cartesian-helper L1 L2 L2 '()))

;7
(define (sum-1-to-n n)
  (define (sum-1-to-n-helper n total)
    (cond ((= n 0) total)
          (else (sum-1-to-n-helper (- n 1) (+ n total)))))
  (sum-1-to-n-helper n 0))

;8
(define (largest L)
  (define (largest-helper L biggest)
    (cond ((null? L) biggest)
          ((> (car L) biggest) (largest-helper (cdr L) (car L)))
          (else (largest-helper (cdr L) biggest))))
  (largest-helper L (car L)))

;9
;Eager, so
; (define L '(1 2 3 4))
; (even? (length L)) -> #t
; (middle L) -> 3
(define (middle L)
  (define (middle-helper L n)
    (cond ((= n 0) (car L))
          (else (middle-helper (cdr L) (- n 1)))))
  (middle-helper L (floor (/ (length L) 2))))

;10
(define (member? a L)
  (cond ((null? L) #f)
	((equal (car L) a) #t)
	(else
	  (member? a (cdr L)))))

;11
(define (member?* a L)
  (cond ((null? L) #f)
	((pair? (car L)) (or (member?* a (car L)) (member?* a (cdr L))))
	((equal (car L) a) #t)
	(else
	  (member?* a (cdr L)))))

;12
(define (removeit a L)
  (cond ((null? L) '())
	((equal (car L) a) (removeit a (cdr L)))
	(else
	  (cons (car L) (removeit a (cdr L))))))

;13
(define (replace old new L)
  (cond ((null? L) '())
	((equal (car L) old) (cons new (replace old new (cdr L))))
	(else (cons (car L) (replace old new (cdr L))))))

;14
(define (remdups L)
  (cond ((null? L) '())
	((member? (car L) (cdr L))
	 (remdups (cdr L)))
	(else (cons (car L) (remdups (cdr L))))))

;15
;is x not a list?
(define (atom? x) 
  (not (pair? x)))
;two atoms equal?
(define (eqan? a1 a2)
  (cond ((and (number? a1) (number? a2)) (= a1 a2))
	((and (atom? a1) (atom? a2)) (eq? a1 a2))
	(else #f)))
;two lists equal?
(define (eqlist? L1 L2)
  (cond ((and (null? L1) (null? L2)) #t)
	((or (null? L1) (null? L2)) #f)
	(else
	  (and (equal (car L1) (car L2)) ; MUTUAL... wait for it...
	       (eqlist? (cdr L1) (cdr L2))))))
;two s-expressions equal?
(define (equal s1 s2)
  (cond ((and (atom? s1) (atom? s2)) (eqan? s1 s2)) 
	((or (atom? s1) (atom? s2)) #f)
	(else (eqlist? s1 s2)))) ; did you wait for it? ...RECURSION!

;16

;17
(define (quicksort L)
  (cond ((<= (length L) 1) L)
	(else
	  (append (quicksort (qpartition (cdr L) < (car L))) 
		  (cons (car L) ; pivot
			(quicksort (qpartition (cdr L) >= (car L))))))))

; returns all the elements in L cmp? pivot, where cmp? could be < or >= or..
(define (qpartition L cmp? pivot) 
  (cond ((null? L) '()) 
	((cmp? (car L) pivot) (cons (car L) (qpartition (cdr L) cmp? pivot)))
	(else (qpartition (cdr L) cmp? pivot))))

;18
(define (revrange n)
  (cond ((zero? n) '())
  	(else (cons n (revrange (- n 1))))))
(define (range n)
  (reverse-iter (revrange n)))

(define (zip L1 L2)
  (cond ((and (null? L1) (null? L2)) '())
	((null? L1) L2)
	((null? L2) L1)
	(else
	  (cons (cons (car L1) (cons (car L2) '()))
		(zip (cdr L1) (cdr L2))))))

(define lower '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
(define upper '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

(define charnummap 
  (append
    (zip lower (range (length lower)))
    (zip upper (map (lambda (x) 
		      (+ x (length lower))) 
		    (range (length upper))))))

(define numcharmap
  (reverse-iter (reverse-iter* charnummap))) 

(define (char->num c)
	(define (char->num-helper c mapping)
	  (cond ((null? mapping) #f) ; error
		((eq? (caar mapping) c)
		 (cadar mapping))
		(else
		  (char->num-helper c (cdr mapping)))))
	(char->num-helper c charnummap))

(define (num->char n)
	(define (num->char-helper n mapping)
	  (cond ((null? mapping) #f) ; error
		((eq? (caar mapping) n)
		 (cadar mapping))
		(else
		  (num->char-helper n (cdr mapping)))))
	(num->char-helper c numcharmap))

(define (string->num strng)
  (define (stringlist->num L)
    (cond ((null? L) '())
	  (else
	    (cons (char->num (car L)) (stringlist->num (cdr L))))))
  (stringlist->num (string->list strng)))

;19

;20
(define (true-for predicate L)
  (cond ((null? L) '())
	((predicate (car L)) (cons (car L) (true-for predicate (cdr L))))
	(else
	  (true-for predicate (cdr L)))))

;21
(define (mapit f L)
  (cond ((null? L) '())
	(else
	  (cons (f (car L)) (mapit f (cdr L))))))

;22

;23

;24
(define (numer x)
  (car x))
(define (denom x)
  (cadr x))
(define (make-rat numer denom)
  (cons numer (cons denom '())))

(define (r+ r1 r2)
  (make-rat
    (+ 
      (* (numer r1) (denom r2))
      (* (numer r2) (denom r1)))
    (* (denom r1) (denom r2))))

(define (r- r1 r2)
  (make-rat
    (-
      (* (numer r1) (denom r2))
      (* (numer r2) (denom r1)))
    (* (denom r1) (denom r2))))

(define (r* r1 r2)
  (make-rat (* (numer r1) (numer r2))
	    (* (denom r1) (denom r2))))

(define (r/ r1 r2)
  (make-rat (* (numer r1) (denom r2))
	    (* (denom r1) (numer r2))))

;25 and 26
(define (evaluate expr fix first second)
  (cond
    ((and (atom? (first expr)) (atom? (second expr)))     
     ((fix expr) (first expr) (second expr)))
    ((atom? (first expr))
     ((fix expr) (first expr) (evaluate (second expr) fix first second)))
    ((atom? (second expr))
     ((fix expr) (evaluate (first expr) fix first second) (second expr)))
    (else
      ((fix expr) (evaluate (first expr)) (evaluate (second expr) fix first second)))))

(define (prefix expr) (operate car expr))
(define prefix-first cadr)
(define prefix-second caddr)

(define (infix expr) (operate cadr expr))
(define infix-first car)
(define infix-second caddr)

(define (operate position expr)
  (cond
    ((eq? (position expr) '+) +)
    ((eq? (position expr) '-) -)
    ((eq? (position expr) '*) *)
    ((eq? (position expr) '/) /)))

;27
(define (compose f g)
  (lambda (x)
    (f (g x))))

;28
(define (fc f)
  (lambda (x)
    (lambda (y)
      (f x y))))

;29
(define (f fc)
  (lambda (x y)
    ((fc x) y)))

;30
