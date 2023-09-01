;Tyler Stovsky
;In this homework, I created three functions called SEQ, SUMS, and ANON. SEQ takes in an integer argument N and returns the Nth Padovan number,
;which it does using the recurrence relation SEQ(N) = SEQ(N - 1) + SEQ(N - 2) + SEQ(N - 3).
;SUMS also takes in an integer argument N, and returns the number of additions required by SEQ. It uses similar logic to SEQ to accomplish this.
;The number of sums is simply the number of sums required by the previous 3 Padovan numbers, followed by 2 more for the new number.
;ANON takes in a TREE that represents a tree, and returns an anonymized version with the same structure by turning the elements into 0s. 
;If we see an atom, we can make it a 0, otherwise if we see a list we can break and build the list back up, finding the atoms along the way and converting them.

;1
;This function takes in a single integer argument N (N >= 0) and returns the Nth Padovan number.
(defun SEQ (N)
  ;This is the base case. If N < 3, then the Padovan number is 1 by definition.
  (if (< N 3)
      1
      ;If N >= 3, then we must follow the recurrence relation given by the homework specifications.
      ;SEQ(N) = SEQ(N - 1) + SEQ(N - 2) + SEQ(N - 3)
      (+ (SEQ (- N 1)) (SEQ (- N 2)) (SEQ (- N 3)))
    )
  )

;(write (SEQ 0))
;(terpri)
;(write (SEQ 1))
;(terpri)
;(write (SEQ 2))
;(terpri)
;(write (SEQ 3))
;(terpri)
;(write (SEQ 4))
;(terpri)
;(write (SEQ 5))
;(terpri)
;(write (SEQ 6))
;(terpri)
;(write (SEQ 7))
;(terpri)
;(write (SEQ 8))
;(terpri)
;(write (SEQ 9))
;(terpri)
;(write (SEQ 15))
;(terpri)
;(write (SEQ 25))
;(terpri)

;2
;This function takes in a single integer argument N (N >= 0) and returns the number of additions required by the SEQ function to compute the Nth Padovan number.
(defun SUMS (N)
  ;This is the base case. If N < 3, then there are no additions required, since the Nth Padovan number is always 1 by definition.
  (if (< N 3)
      0
      ;If N >= 3, then we must compute the number of additions.
      ;To do this, we need to add together however many additions the previous 3 Padovan numbers required, then add two for the addition of the new number.
      ;SUMS(N) = SUMS(N - 1) + SUMS(N - 2) + SUMS(N - 3) + 1
      (+ (SUMS (- N 1)) (SUMS (- N 2)) (SUMS (- N 3)) 2)
    )
  )

;(write (SUMS 0))
;(terpri)
;(write (SUMS 1))
;(terpri)
;(write (SUMS 2))
;(terpri)
;(write (SUMS 3))
;(terpri)
;(write (SUMS 4))
;(terpri)
;(write (SUMS 5))
;(terpri)
;(write (SUMS 6))
;(terpri)
;(write (SUMS 7))
;(terpri)
;(write (SUMS 8))
;(terpri)
;(write (SUMS 9))
;(terpri)
;(write (SUMS 15))
;(terpri)
;(write (SUMS 25))
;(terpri)

;3
;This function takes in a single argument TREE that represents a tree, and returns an anonymized tree with the same structure,
;but where all symbols and numbers in the tree are replaced by 0.
(defun ANON (TREE)
  (cond
    ;This is the first base case.  If TREE is empty, then we can simply return an empty list.
   ((not TREE) '())
    ;This is the second base case. If TREE is just an atom, which is a basic variable (number, character, string, etc.), then we return 0.
   ((atom TREE) 0)
    ;Otherwise, TREE is a list. So, we need to recursively call the function on car and cdr, which is the first item of the list and the rest of the list.
    ;Then we are able to use merge the two results together using cons.
    (t (cons (ANON(car TREE)) (ANON(cdr TREE))))
   )
  )

;(write (ANON '42))
;(terpri)
;(write (ANON 'FOO))
;(terpri)
;(write (ANON '(((L E) F) T)))
;(terpri)
;(write (ANON '(5 FOO 3.1 -0.2)))
;(terpri)
;(write (ANON '(1 (FOO 3.1) -0.2)))
;(terpri)
;(write (ANON '(((1 2) (FOO 3.1)) (BAR -0.2))))
;(terpri)
;(write (ANON '(R (I (G (H T))))))
;(terpri)
;(write (ANON '()))
;(terpri)
;(write (ANON '(())))
;(terpri)
