; asks if x is an atom
; returns boolean
(define (atom? x)
  (and (not (pair? x)) 
       (not (null? x))))

; asks if list L is a list of atoms [lat]
; returns boolean
(define lat?
  (lambda (L)
    (cond
      ((null? L) ; is L empty?
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
    (cond
      ((null? lat)
       0)
      (else
        (add1 (len (cdr lat)))
       )
     )
   )
 )

; picks the nth element in lat, indexed from 1
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n))
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
(define (eqan? a1 a2)
  (cond
    ((and (number? a1) (number? a2))
     (= a1 a2))
    ((or (number? a1) (number? a2)) 
     #f)
    (else (eq? a1 a2))))

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

; a simpler version I wrote. I removed the multiple conds and elses, and asked if it was a cons pair (list) instead of an atom.
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
; opposite of cons :)
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
       0)			; degenerate case 0
      ((pair? (car L))
       (+ (occur* a (car L))
          (occur* a (cdr L))))
      ((eq? (car L) a)
       (+ 1 (occur* a (cdr L)))); inductiive step 1
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

; finds the left most atom in a non-empty list
(define (leftmost L)
  (cond
    ((atom? (car L)) (car L))
    (else (leftmost (car L))
      )
    )
  )

; determines if two S-expressions s1 and s2 are the same
; an S-expression is an atom or a (possibly empty) list of S-expressions
(define (areequal? s1 s2)
  (cond
    ((and (atom? s1) (atom? s2))
     (eqan? s1 s2))
    ((or (atom? s1) (atom? s2)) #f)
    (else (eqlist? s1 s2)
      )
    )
  )

; determines if two lists l1 and l2 are the same (uses areequal?)
(define (eqlist? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    (else
      (and (areequal? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2)))
      )
    )
  )

; chapter 6

; asks if aexp is a numbered expression
; the expression is in infix notation, e.g.
; ((1 + 2) * 3) -> #t
(define (numbered? aexp)
  (cond
    ((atom? aexp) (number? aexp))
    (else
      (and (numbered? (car aexp))
	   (numbered? (car (cdr (cdr aexp)))))
      )
    )
  )

(define (first-sub-exp aexp)
  (car aexp))

(define (second-sub-exp aexp)
  (car (cdr (cdr aexp))))

(define (operator aexp)
  (car (cdr aexp)))

(define (value nexp)
  (cond
    ((atom? nexp) nexp)
    ((eq? (operator nexp) '+)
    (+ (value (first-sub-exp nexp)) 
       (value (second-sub-exp nexp))))
    ((eq? (operator nexp) '*)
    (* (value (first-sub-exp nexp)) 
       (value (second-sub-exp nexp))))
    ((eq? (operator nexp) '^)
    (expt (value (first-sub-exp nexp)) 
       (value (second-sub-exp nexp)))
      )
    )
  )

; THE EIGHTH COMMANDMENT : Use help functions to abstract from representations


; Oh man Church numerals

; So here we define zero to be '()
(define sero?
  (lambda (n)
    (null? n))
  )

; This is our church numeral successor function
(define edd1
  (lambda (n)
    (cons (quote ()) n))
  )

; Likewise for subtraction
(define zub1
  (lambda (n)
    (cdr n))
  )

; Plus
(define ples
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else
	(edd1 (ples n (zub1 m)))
	)
      )
    )
  )

; Q. Recall `lat?`
; A.
; asks if list L is a list of atoms [lat]
; returns boolean
(define lat?
  (lambda (L)
    (cond
      ((null? L) ; is L empty?
       #t)
      ((atom? (car L)) 
       (lat? (cdr L)))
      (else 
        #f)
      )
    )
  )

; Q. Do you remember what the value of (lat? ls) is where ls is (1 2 3)
; A. #t, of course.

; Q. What is (1 2 3) with our new numbers?
; A.
(define one-two-three
  '((()) (()()) (()()()))
  )

; Q. What is (lat ls?) where ls is ((()) (()()) (()()()))
; A. It is very false.
;
; In other words, (atom? '(())) --> #f.

; Q. Is that bad?
; A. You must beware of shadows.
;
; Well that's great, Friedman and Felleisen. But what does that mean?
;
; From http://groups.yahoo.com/neo/groups/sicp-vsg/conversations/topics/233
;
; By Pascal Bourguignon
;
;
; When you're abstracting you must be careful not to use functions from
; different layers. You don't use an electronic microscope to lay
; bricks in a wall. But you could use an electronic microscope to build
; molecules that you'd use to make the cement used to found the bricks
; you'll use to build the wall.
;
;
; So, the atom? I defined, in term of pair? is a function that works at
; the level of the conses we manipulate with the interpreter at hand.
;
; If you use these conses to build higher level abstractions, such as
; numbers, characters, strings and symbols, and high level conses, then
; you'll need a way to distinguish these different kinds of object, and
; you'll need an higher level high-atom? function.
;
; Let me paste thereafter some Common-Lisp code I have that will
; illustrate the point.
;
; (cl:let ((zero (minimal-lisp-user::|0|)))
; (cl:list (cl:atom zero) (minimal-lisp-user::atom zero)))
; --> (NIL T)
;
; The same object isn't an atom at the implementation level, but is an
; atom at the higher abstraction level. The first atom is cl:atom, the
; second is minimal-lisp-user::atom.
;
;
; Also, remember that lisp plays tricks already with conses and lists:
; lists are implemented with conses, and a list object is referenced by
; its first cons. This allows "clever" tricks such as applying cons
; functions to lists or lists functions to conses. But you have to be
; careful, because a list function applied to a cons that belongs to a
; tree or another kind of data structure may give surprizing results.
; (Try to use a list function on a circular list or a dotted-list).
; It's better to distinguish the two abstraction level, and use
; car/cdr/null with cons+nil and first/rest/endp with lists (or scheme's
; car/cdr/null? and first/rest/ it doesn't have end? !?). And if you
; use lists to implement trees, you'd rather use tree-node and
; tree-children rather than first and rest.
;
;
;
;
; (defpackage :minimal-lisp
; (:nicknames :ml)
; (:import-from :common-lisp :car :cdr :cons :quote :eq :cond :defun)
; (:export :car :cdr :cons :quote :eq :cond :defun))
;
; (defpackage :minimal-lisp-user
; (:nicknames :mlu)
; (:use :minimal-lisp))
;
; (in-package :minimal-lisp-user)
;
; ;; From here on, we only have the minimal set of primitives:
; ;; car cdr cons quote eq cond defun
;
; (defun make-integer (value) (cons (quote (())) value))
; (defun make-char (value) (cons (quote ((()))) value))
; (defun make-string (value) (cons (quote (((())))) value))
; (defun make-cons (value) (cons (quote ((((()))))) value))
; (defun make-symbol (value) (cons (quote (((((())))))) value))
;
; (defun get-value (tagged-value) (cdr tagged-value))
; (defun get-tag (tagged-value) (car tagged-value))
;
; (defun integerp (tagged-value) (eq (get-tag (make-integer nil))
;   (get-tag tagged-value)))
; (defun charp (tagged-value) (eq (get-tag (make-char nil))
;   (get-tag tagged-value)))
; (defun stringp (tagged-value) (eq (get-tag (make-string nil))
;   (get-tag tagged-value)))
; (defun consp (tagged-value) (eq (get-tag (make-cons nil))
;   (get-tag tagged-value)))
; (defun symbolp (tagged-value) (eq (get-tag (make-symbol nil))
;   (get-tag tagged-value)))
; (defun atom (tagged-value) (not (consp tagged-value)))
;
; (defun t () (quote (())))
; (defun nil () (quote ()))
; (defun null (a) (eq (nil) a))
; (defun and (a b) (cond (a b) ((nil))))
; (defun or (a b) (cond (a a) (b b)))
; (defun not (a) (cond (a (nil)) ((t))))
; (defun equiv (a b)
;   (or (and (null a) (null b)) 
;   (and (not (null a)) (not (null b)))))
;
;
; (defun internal-mantissa (a) (car (get-value a)))
; (defun internal-sign (a) (cdr (get-value a)))
;
; (defun error (err))
;
;
; (defun abs (a)
;   (cond ((integerp a) (make-integer (cons (internal-mantissa a) (nil))))
;         ((error (quote applying abs to non integer)))))
;
; (defun negate (a)
;   (cond ((integerp a) (make-integer (cons (internal-mantissa a)
;                                           (not (internal-sign a)))))
;         ((error (quote applying negate to non integer)))))
;
; (defun |0| () (make-integer (cons (nil) (nil))))
; (defun |1| () (make-integer (cons (t) (nil))))
; (defun |-1| () (make-integer (cons (t) (t))))
;
; (defun zerop (a) (null (internal-matissa a)))
; (defun positivep (a) (null (internal-sign a)))
;
; (defun internal= (am bm)
;   (cond ((and (null am) (null bm)))
;         ((or (null am) (null bm)) (nil))
;         ((internal= (car am) (car bm)))))
;
; (defun internal< (am bm)
;   (cond ((null bm) (nil))
;         ((null am) (t))
;         ((internal< (car am) (car bm)))))
;
; (defun = (a b) 
;   (and (internal= (internal-matissa a) (internal-mantissa b))
;        (equiv (internal-sign a) (internal-sign b))))
;
; (defun < (a b)
;   (cond ((and (positivep a) (positivep b))
;     (cond ((internal< (internal-matissa a)(internal-matissa b))) ((nil))))
;           ((and (not (positivep a)) (not (positivep b)))
;             (cond ((internal< (internal-matissa b)(internal-matissa a))) 
;               ((nil))))
;     ((positivep a) (nil))
;     ((t))));;<
;
; (defun > (a b) (not (or (< a b) (= a b))))
; (defun >= (a b) (not (< a b)))
; (defun <= (a b) (not (> a b)))
; (defun /= (a b) (not (= a b)))
;
;
; (defun internal+ (al bl)
;   (cond ((eq (quote ()) al) bl)
;         ((internal+ (car al) (cons bl (quote ()))))))
;
; (defun internal-reduce-value (val)
;   (cond
;         ((or (null (car val)) (null (cdr val))) val)
;         ((internal-reduce-value (cons (car (car varl)) (car (cdr val)))))))
;
;
; (defun + (a b)
;   (cond ((zerop a) b)
;         ((zerop b) a)
;         ((and (positivep a) (positivep b))
;           (make-integer (cons (internal+ (internal-mantissa a) (internal-mantissa b))
;                               (nil))))
;         ((and (not (positivep a)) (not (positivep b)))
;          (make-integer (cons (internal+ (internal-mantissa a) (internal-mantissa b))
;                              (t))))
;         ((positive a) (cond ((internal< (internal-mantissa b) (internal-mantissa a))
;                         (make-integer (cons (internal- (internal-mantissa a) (internal-mantissa b))
;                                             (nil))))
;         ((internal= (internal-mantissa b) (internal-mantissa a)) (|0|)) 
;         ((make-integer (cons (internal- (internal-mantissa b) (internal-mantissa a))
;                              (t))))))
;         ((cond ((internal< (internal-mantissa a) (internal-mantissa b)) 
;                  (make-integer (cons (internal- (internal-mantissa b) (internal-mantissa a))
;                                      (nil))))
;                ((internal= (internal-mantissa a) (internal-mantissa b)) (|0|))
;                ((make-integer (cons (internal- (internal-mantissa a) (internal-mantissa b))
;                                     (t))))))));;+
;
;
; (defun - (a b) (+ a (negate b)))
;
; (defun internal* (am bm)
;   (cond ((null am) am)
;         ((null bm) bm)
;         ((null (car am)) bm)
;         ((null (car bm)) am)
;         ;; a*b = b+(a-1)*b
;         ((internal+ bm (internal* (car am) bm)))));;internal*
;
; (defun * (a b)
;   (cond ((zerop a) a)
;         ((zerop b) b)
;         ((= (|1|) a) b)
;         ((= (|1|) b) a)
;         ((= (|-1|) a) (negate b))
;         ((= (|-1|) b) (negate a))
;         ((make-integer (cons (internal* (internal-mantissa a) (internal-mantissa b))
;                              (not (equiv (internal-sign a) (internal-sign b))))))));;*


; Chapter 7. Friends and Relations

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))
	)
      )
    )
  )

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((member? (car lat) (cdr lat)) 
       (makeset (cdr lat)))
      (else (cons (car lat) 
		  (makeset (cdr lat)))
	)
      )
    )
  )

(define makeset2
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cons (car lat)
		  (makeset 
		    (multirember (car lat) 
				 (cdr lat))))
	)
      )
    )
  )

(define subset?
  (lambda (set1 set2)
    (cond 
      ((null? set1) #t)
      (else (and (member? (car set1) set2) 
		 (subset? (cdr set1) set2))
        )
      )
    )
  )

(define eqset?
  (lambda (set1 set2)
   (and (subset? set1 set2) (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond 
      ((null? set1) #t)
      (else (or (member? (car set1) set2)
		(intersect? (cdr set1) set2))
	)
      )
    )

; See how intersect? and subset? are the same apart from using either AND or OR?

(define intersect
  (lambda (set1 set2)
    (cond 
      ((null? set1) (quote ()))
      ((member? (car set1) set2) 
       (cons (car set1) 
	     (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)
	)
      )
    )
  )

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) 
       (union (cdr set1) set2))
      (else (cons (car set1) 
		  (union (cdr set1) set2))
	)
      )
    )
  )

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2)
       (difference (cdr set1) set2))
      (else (cons (car set1)
		  (difference (cdr set1) set2))
	)
      )
    )
  )

; Hmm... how does this work?

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) 
		       (intersectall (cdr l-set)))
	)
      )
    )
  )
