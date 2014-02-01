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
      ((null? (cdr l-set)) (car l-set)) ; if the rest of the list is null, return the first element
      (else (intersect (car l-set) ; else return the intersection of the first element with... 
		       (intersectall (cdr l-set))) ; ...the recurred intersection of the rest.
	)
      )
    )
  )

(define a-pair?
  (lambda (x)
    (cond
      ((null? x) #f)
      ((atom? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f
	)
      )
    )
  )

(define first car)

(define second (lambda (pair) (car (cdr pair))))

(define build (lambda (s1 s2) (cons s1 (cons s2 (quote ())))))

(define third (lambda (l) (car (cdr (cdr l)))))

(define fun? (lambda (rel) (set? (firsts rel))))

(define revpair (lambda (pair) (build (second pair) (first pair))))

(define revrel 
  (lambda (rel)
    (cond 
      ((null? rel) (quote ()))
      (else
	(cons (revpair (car rel))
	      (revrel (cdr rel)))
        )
      )
    )
  )

(define one-to-one? 
  (lambda (rel)
    (fun? (revrel rel))))

;; Chapter 8 -- Lambda the Ultimate

(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) (quote ()))
      ((test? (car l) a) (cdr l))
      (else
	(cons (car l)
	      (rember-f test? a (cdr l)))
	)
      )
    )
  )

(define rember-f2
  (lambda (test?)
    (lambda (a l)
      (cond
	((null? l) (quote ()))
	((test? (car l) a) (cdr l))
	(else
	  (cons (car l)
		((rember-f2 test?) a (cdr l)))
	  )
	)
      )
    )
  )

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
	((null? l) (quote ()))
	((eq? (car l) old) (seq new old (cdr l)))
	(else
	  (cons (car l)
		((insert-g seq) new old l))
	  )
	)
      )
    )
  )

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define seqrem
  (lambda (new old l)
    l))

(define seqsubst
  (lambda (new old l)
    (cons new l)))

(define rember-g (lambda (new l) ((insert-g seqrem) #f new l)))
(define insertL-g (insert-g seqL))
(define insertR-g (insert-g seqR))
(define subst-g (insert-g seqsubst))

; The Ninth Commandment: Abstract common patterns with a new function.
;
(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x (quote +)) +)
      ((eq? x (quote *)) *)
      (else pow
	)
      )
    )
  )

(define value-g
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
	((atom-to-function
	   (operator nexp))
	 (value (first-sub-exp nexp))
	 (value (second-sub-exp nexp)))
	)
      )
    )
  )

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
	((null? lat) (quote ()))
	((test? (car lat) a) ((multirember-f test?) a (cdr lat)))
	(else
	  (cons (car lat)
		((multirember-f test?) a (cdr lat)))
	  )
	)
      )
    )
  )

(define eq-tuna?
  (lambda (x)
    (eq? x (quote tuna))))

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) (quote ()))
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else
	(cons (car lat)
	      (multiremberT test? (cdr lat)))
	)
      )
    )
  )

; remove all instances of a in lat and place it in the right list
; place the rest of it in the left list
; using these lambda functions to collect them
; and finally, call col on the result!
(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col (quote ()) (quote ())))
      ((eq? (car lat) a)
       (multirember&co a
		       (cdr lat)
		       (lambda (newlat seen)
			 (col newlat
			      (cons (car lat) seen)))))
      (else
	(multirember&co a
			(cdr lat)
			(lambda (newlat seen)
			  (col (cons (car lat) newlat) 
			       seen)))
	)
      )
    )
  )

; lat is (strawberries tuna and swordfish)

; So if we set col to be a-friend and pass it to multirember&co in the call:
; (multirember&co (quote tuna) (quote (strawberries tuna and swordfish)) a-friend)
(define a-friend
  (lambda (x y)
    (null? y)))

; new-friend is what the first lambda in multirember&co becomes
(define new-friend
  (lambda (newlat seen)
    (a-friend newlat
	     (cons (quote tuna) seen))))

; and latest-friend is what the second lambda in multirember&co becomes
(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons (quote and) newlat) 
	      seen)))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) oldL)
       (cons new
	     (cons oldL
		   (multiinsertLR new oldL oldR
				  (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons oldR
	     (cons new
		   (multiinsertLR new oldL oldR
				  (cdr lat)))))
      (else
	(cons (car lat)
	      (multiinsertLR new oldL oldR (cdr lat)))
	)
      )
    )
  )

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat)
       (col (quote ()) 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR
			 (cdr lat)
			 (lambda (newlat L R)
			   (col (cons new
				      (cons oldL newlat)) (add1 L) R))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR
			 (cdr lat)
			 (lambda (newlat L R)
			   (col (cons oldR 
				      (cons new newlat)) L (add1 R)))))
      (else
	(multiinsertLR&co new oldL oldR
			  (cdr lat)
			  (lambda (newlat L R)
			   (col (cons (car lat) newlat) L R)))
	)
      )
    )
  )

(define even?
  (lambda (n)
    (= 0 (modulo n 2))))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) 
       (cond
	 ((even? (car l))
	  (cons (car l)
		(evens-only* (cdr l))))
	 (else (evens-only* (cdr l)))))
      (else
	(cons (evens-only* (car l))
	      (evens-only* (cdr l)))
	)
      )
    )
  )

(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l) (col (quote ()) 1 0))
      ((atom? (car l))
       (cond
	 ((even? (car l))
	  (evens-only*&co (cdr l)
			  (lambda (newl p s)
			    (col (cons (car l) newl) 
				 (* (car l) p) s))))
	 (else
	   (evens-only*&co (cdr l)
			   (lambda (newl p s)
			     (col newl
				  p (+ (car l) s)))))))
      (else
	(evens-only*&co (car l)
			(lambda (al ap as)
			  (evens-only*&co (cdr l)
					  (lambda (dl dp ds)
					    (col (cons al dl)
						 (* ap dp)
						 (+ as ds))))))
	)
      )
    )
  )

(define the-last-friend
  (lambda (newl product sum)
    (cons sum
	  (cons product
		newl))))

; Chapter 9. ...and Again, and Again, and Again,...

(define eternity
  (lambda (x)
    (eternity x)))

(define length
  (lambda (l)
    (cond
      ((null? l) 0)
      (else
	(add1 (length (cdr l)))
	)
      )
    )
  )

; determines the length of the empty list
(lambda (l)
  (cond
    ((null? l) 0)
    (else 
      (add1 (eternity (cdr l)))
      )
    )
  )

; determines the length of lists that contain 1 or fewer items
(lambda (l)
  (cond
    ((null? l) 0)
    (else
      (add1
	((lambda (l)
	   (cond
	     ((null? l) 0)
	     (else
	       (add1 (eternity (cdr l))))))
	 (cdr l)))
      )
    )
  )

; determines the length of lists that contain 2 or fewer items
(lambda (l)
  (cond
    ((null? l) 0)
    (else
      (add1
	((lambda (l)
	   (cond
	     ((null? l) 0)
	     (else
	       (add1
		 ((lambda (l)
		   (cond
		     ((null? l) 0)
		     (else
		       (add1 (eternity (cdr l))))))
		  (cdr l))))))
	 (cdr l)))
      )
    )
  )

(define five
(((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1
	       ((mk-length mk-length)
		(cdr l)))))))) '(1 2 3 4 5)))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

; 10. What Is the Value of All of This?

(define entry1 (quote ((tuna beans toast butter) (fish pulses base spread))))
(define entry2 (quote ((dog cat budgie ferret) (woof meow tweet silence))))

(define new-entry build)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
			  (first entry)
			  (second entry)
			  entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car values))
      (else
	(lookup-in-entry-help name (cdr names) (cdr values) entry-f)
	)
      )
    )
  )

(define table1 (cons entry1 (cons entry2 (quote ()))))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else
	(lookup-in-entry name (car table) 
			 (lambda (name)
			   (lookup-in-table name
					    (cdr table)
					    table-f)))
	)
      )
    )
  )

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else
	(list-to-action e)
	)
      )
    )
  )

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
	 ((eq? (car e) (quote quote))
	  *quote)
	 ((eq? (car e) (quote lambda))
	  *lambda)
	 ((eq? (car e) (quote cond))
	  *cond)
	 (else *application)))
      (else 
	*application
	)
      )
    )
  )

(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else 
	(build (quote primitive) e)
	)
      )
    )
  )

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define initial-table
  (lambda (name)
    (car (quote ()))))

(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
	   (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of third)


; WARNING! We violate the First Commandment here as we don't ask (null? lines), so one of the questions in every cond better be true!
(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines))
		table))
      ((meaning (question-of (car lines)) 
		table)
       (meaning (answer-of (car lines))
		table))
      (else 
	(evcon (cdr lines) table)
	)
      )
    )
  )

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x (quote else)))
      (else
	#f
	)
      )
    )
  )

(define question-of first)

(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define e1 '(cond (coffee klatsch) (else party)))

(define table2 '(((coffee) (#t)) ((klatsch party) (5 (6)))))

(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote ()))
      (else
	(cons (meaning (car args) table)
	      (evlis (cdr args) table))
	)
      )
    )
  )

(define *application
  (lambda (e table)
    (apply-it
      (meaning (function-of e) table)
      (evlis (arguments-of e) table))))

(define function-of car)

(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

(define apply-it
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitive
	 (second fun) vals))
      ((non-primitive? fun)
       (apply-closure
	 (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (cons (first vals) (second vals)))
      ((eq? name (quote car))
       (car (first vals)))
      ((eq? name (quote cdr))
       (cdr (first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))
      ((eq? name (quote eq?))
       (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
       (:atom? (first vals)))
      ((eq? name (quote zero?))
       (zero? (first vals)))
      ((eq? name (quote add1))
       (add1 (first vals)))
      ((eq? name (quote sub1))
       (sub1 (first vals)))
      ((eq? name (quote number?))
       (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) (quote primitive))
       #t)
      ((eq? (car x) (quote non-primitive))
       #t)
      (else
	#f
	)
      )
    )
  )

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
	     (extend-table
	       (new-entry
		 (formals-of closure)
		 vals)
	       (table-of closure)))))

(define closure1
  '(
    (
     ((u v w)
      (1 2 3))
     ((x y z)
      (4 5 6))
     )
    (x y)
    (cons z x)
    )
  )

(define vals1 '((a b c) (d e f)))

; Yes, it's time for a banquet.
