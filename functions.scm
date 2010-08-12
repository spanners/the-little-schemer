; asks if x is an atom
; returns boolean
(define atom?
  (lambda (x)
    (and 
      (not (pair? x)) 
      (not (null? x))
      )
    )
  )

; asks if list L is a list of atoms [lat]
; returns boolean
(define lat?
  (lambda (L)
    (cond
      ((null? L) 
       #t)
      ((atom? (car L)) 
       (lat? (cdr L)))
      (else 
        #f)
      )
    )
  )

; asks if a is a member of lat
; returns boolean
(define member?
  (lambda (a lat)
    (cond
      ((null? lat)
       #f)
      (else 
        (or
          (eq? (car lat) a)
          (member? a (cdr lat))
          )
        )
      )
    )
  )

; removes first occurrence a from lat
; returns list without a
(define rember
  (lambda (a lat)
    (cond
      ((null? lat)
       (quote ()))
      ((eq? (car lat) a)
       (cdr lat))
      (else
        (cons (car lat)
              (rember a (cdr lat))
              )
        )
      )
    )
  )

; takes a list of non-empty sub-lists
; returns a list containing the first element of each sub-list
(define firsts
  (lambda (L)
    (cond
      ((null? L) 
       (quote ()))
      (else
        (cons (car (car L))
              (firsts (cdr L)))
        )
      )
    )
  )

; inserts new to the right of the first occurrence of old in lat
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) 
       (quote ()))
      (else
        (cond
          ((eq? (car lat) old)
           (cons old
                 (cons new
                       (cdr lat))))
          (else
            (cons (car lat)
                  (insertR new old (cdr lat)))
            )
          )
        )
      )
    )
  )

; inserts new to the left of first occurrence of old in lat
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat)
       (quote ()))
      (else
        (cond
          ((eq? (car lat) old)
           (cons new
                 lat))
          (else
            (cons (car lat)
                  (insertR new old (cdr lat)))
            )
          )
        )
      )
    )
  )

; substitutes new in place of first occurrence of old in lat
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat)
       (quote ()))
      (else
        (cond
          ((eq? (car lat) old)
           (cons new
                 (cdr lat)))
          (else
            (cons (car lat)
                  (subst new old (cdr lat)))
            )
          )
        )
      )
    )
  )

; substitutes either the first occurrence of o1 or first occurrence of o2 in lat with new
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat)
       (quote ()))
      (else
        (cond
          ((or
             (eq? (car lat) o1)
             (eq? (car lat) o2))
           (cons new
                 (cdr lat)))
          (else
            (cons (car lat)
                  (subst2 new o1 o2 (cdr lat)))
            )
          )
        )
      )
    )
  )

; returns lat with all occurrences of a removed
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat)
       (quote ()))
      (else
        (cond
          ((eq? (car lat) a)
           ; remove and carry on
           (multirember a (cdr lat)))
          (else
            ; save and carry on
            (cons (car lat)
                  (multirember a (cdr lat)))
            )
          )
        )
      )
    )
  )


; increments n by 1
(define add1
  (lambda (n)
    (+ n 1)
    )
  )

; decrements n by 1
(define sub1
  (lambda (n)
    (- n 1)
    )
  )

; adds m to n
(define add
  (lambda (m n)
    (cond
      ((zero? n)
       m)
      (else
        (add1 (add m (sub1 n)))
        )
      )
    )
  )

; subtracts n from m
(define sub
  (lambda (m n)
    (cond
      ((zero? n)
       m)
      (else
        (sub1 (sub m (sub1 n)))
        )
      )
    )
  )

; multiplies m by n
(define mult
  (lambda (m n)
    (cond
      ((zero? n)
       0)
      (else
        (add m (mult m (sub1 n)))
        )
      )
    )
  )

; true if m is less than n
; false otherwise
(define lt
  (lambda (m n)
    (cond
      ((zero? n)
       #f)
      ((zero? m) 
       #t)
      (else
        (lt (sub1 m) (sub1 n))
        )
      )
    )
  )

; true if m is greater than n
; false otherwise
(define gt
  (lambda (m n)
    (cond
      ((zero? m)
       #f)
      ((zero? n)
       #t)
      (else
        (gt (sub1 m) (sub1 n))
        )
      )
    )
  )

; does m equal n?
(define equals
  (lambda (m n)
    (cond
      ((zero? m) 
       (zero? n))
      ((zero? n)
       #f)
      (else
        (equals (sub1 m) (sub1 n))
        )
      )
    )
  )

; m to the power n
(define pow
  (lambda (m n)
    (cond
      ((zero? m)
       1)
      (else
        (mult m (pow m (sub1 n)))
        )
      )
    )
  )

; m divide n
(define div
  (lambda (m n)
    (cond
      ((lt m n) 
       0)
      (else
        (add1 (div (- m n) n))
        )
      )
    )
  )

; add two tuples (lists of numbers) together
; (addtup '(1 2 3 4) '(4 3 2 1)) -> (5 5 5 5)
; tup1 and tup2 can be of unequal length
; (addtup '(3 5) '(4)) -> (7 5)
; addtup appends the rest of the longer list
(define addtup
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
        (cons (add (car tup1) (car tup2))
              (addtup (cdr tup1) (cdr tup2)))
        )
      )
    )
  )

; finds length of lat
(define len
  (lambda (lat)
    ((null? lat)
     0)
    (else
      (add1 (len (cdr lat)))
      )
    )
  )

; picks the nth element in lat, indexed from 1
(define pick
  (lambda (n lat)
    ((zero? (sub1 n)
            (car lat))
     (else
       (pick (sub1 n) (cdr lat))
       )
     )
    )
  )

; removes nth element from lat, indexed from 1
(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n))
       (cdr lat))
      (else
        (cons (car lat)
              (rempick (sub1 n) (cdr lat)))
        )
      )
    )
  )

; removes numbers from lat
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat)
       (quote ()))
      (else
        (cond
          ((number? (car lat))
           (no-nums (cdr lat)))
          (else
            (cons (car lat)
                  (no-nums (cdr lat)))
            )
          )
        )
      )
    )
  )

; returns numbers in lat
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat)
       (quote ()))
      (else
        (cond
          ((number? (car lat))
           (cons (car lat)
                 (all-nums (cdr lat))))
          (else
            (all-nums (cdr lat))
            )
          )
        )
      )
    )
  )

; determines if two atoms are the same
; true if same
; false otherwise
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and 
         (number? a1)
         (number? a2))
       (= a1 a2))
      ((or
         (number? a1)
         (number? a2))
       #f)
      (else
        (eq? a1 a2)
        )
      )
    )
  )

; counts the number of times a occurs in lat
(define occur
  (lambda (a lat)
    (cond
      ((null? lat)
       0)
      (else
        (cond
          ((eq? (car lat) a)
           (add1 (occur a (cdr lat))))
          (else
            (occur a (cdr lat))
            )
          )
        )
      )
    )
  )

; #t if n is 1
; #f otherwise
(define one?
  (lambda (n)
    (= n 1)
    )
  )

(define rempick
  (lambda (n lat)
    (cond
      ((one? n)
       (cdr lat))
      (else
        (cons (car lat)
              (rempick (sub1 n) (cdr lat)))
        )
      )
    )
  )

; removes all occurrences of atom a from list L
(define rember*
  (lambda (a L)
    (cond
      ; termination condition
      ((null? L)
       (quote ()))
      ; if first element is an atom
      ((atom? (car L)) 
       (cond
         ; ask if it's a
         ((eq? (car L) a)
          ; if so, remove and recurse on rest of L
          (rember* a (cdr L)))
         (else
           ; else save the atom and recurse on rest of L
           (cons (car L)
                 (rember* a (cdr L))))))
      ; not an atom
      (else
        ; recurse on the first sublist of L
        (cons (rember* a (car L))
              ; and the rest of L
              (rember* a (cdr L)))
        )
      )
    )
  )

; inserts new to the right of all occurrences of old in L
(define insertR*
  (lambda (new old L)
    (cond
      ((null? L)
       (quote ()))
      ((atom? (car L))
       (cond
         ((eq? (car L) old)
          (cons old
                (cons new
                      (insertR* new old (cdr L)))))
         (else
           (cons (car L)
                 (insertR* new old (cdr L))))))
      (else
        (cons (insertR* new old (car L))
              (insertR* new old (cdr L)))
        )
      )
    )
  )

; a simpler version I wrote. I removed the multiple conds and elses, and asked if it wasa cons pair (list) instead of an atom.
; atom? asks null? and pair? and the bigger insertR* has already asked null?, so this saves repeated computation.
; inserts new to the right of all occurrences of old in L
(define insertR*
  (lambda (new old L)
    (cond
      ((null? L)
       (quote ()))
      ((pair? (car L))
       (cons (insertR* new old (car L))
             (insertR* new old (cdr L))))
      ((eq? (car L) old)
       (cons old
             (cons new
                   (insertR* new old (cdr L)))))
      (cons (car L)
            (insertR* new old (cdr L)))
      )
    )
  )

; appends a at the end of lat
(define snoc
  (lambda (lat a)
    (cond
      ((null? lat)
       (cons a lat))
      (else
        (cons
          (car lat)
          (snoc (cdr lat) a))
        )
      )
    )
  )

; reverses list of atoms lat
(define revers
  (lambda (lat)
    (cond
      ((null? lat)
       (quote ()))
      (else
        (snoc
          (revers (cdr lat))
          (car lat))
        )
      )
    )
  )

; reverses list of S-expressions L
(define reverse*
  (lambda (L)
    (cond
      ((null? L)
       (quote ()))
      ((pair? (car L))
       (snoc
         (reverse* (cdr L))
         (reverse* (car L))))
      (else
        (snoc
          (reverse* (cdr L))
          (car L))
        )
      )
    )
  )

; counts occurences of a in L
(define occur* 
  (lambda (a L)
    (cond
      ((null? (car L))
       0)
      ((pair? (car L))
       (+ (occur* a (car L))
          (occur* a (cdr L))))
      ((eq? (car L) a)
       (+ 1 (occur* a (cdr L))))
      (occur* a (cdr L))
      )
    )
  )

; substitutes all occurences of old with new in L
(define subst*
  (lambda (new old L)
    (cond
      ((null? L)
       (quote ()))
      ((atom? (car L))
       (cond
         ((eq? (car L) old)
          (cons new
                (subst* new old (cdr L))))
         (else
           (cons (car L)
                 (subst* new old (cdr L))))))
      (else
        (cons (subst* new old (car L))
              (subst* new old (cdr L)))
        )
      )
    )
  )

; inserts new to the left of old in L
(define insertL*
  (lambda (new old L)
    (cond
      ((null? L)
       (quote ()))
      ((atom? (car L))
       (cond
         ((eq? (car L) old)
          (cons new
                (cons old
                      (insertL* new old (cdr L)))))
         (else
           (cons (car L)
                 (insertL* new old (cdr L))))))
      (else
        (cons (insertL* new old (car L))
              (insertL* new old (cdr L)))
        )
      )
    )
  )

; true if a is in L
; false otherwise
(define member*
  (lambda (a L)
    (cond
      ((null? L)
       (quote ()))
      ((atom? (car L))
       (or (eq? (car L) a)
           (member* a (cdr L))))
      (else
        (or (member* a (car L))
            (member* a (cdr L)))
        )
      )
    )
  )
