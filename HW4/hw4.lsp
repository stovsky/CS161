;
; Graph coloring to SAT conversion
;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw4.lsp")
  );end defun

; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).
					;
; Given by the formula, variable index = (n - 1) * k + c
(defun node2var (n c k)
  (+ c (* k (- n 1)))
 )



; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
					;
; If the variable index of node n, color c, k possible colors is v, then:
; At least one color means we must have v or v+1 or v+2 or ... or v+k  
; This is simply represented as (v1 v2 v3 ... vk)
(defun at-least-one-color (n c k)
  (let ((v (node2var n c k)))
  (cond
   ; Base case: If c is the last possible color, just return this as a list.
   ((eql c k) (list v))
   ; Otherwise, add v to list and recursively call until we reach the last possible color.
   (t (cons v (at-least-one-color n (+ c 1) k)))
   )
  )
)

; EXERCISE: Fill this function
; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."


; Trivial function which negates an integer n.
(defun neg (n)
  (- 0 n)
  )


; Suppose we have two variables, x and y. Then to have at most one of them, this is not(x and y).
; If we have multiple variables, then each pair of variables must follow this rule.
; Using DeMorgan's law, not(x and y) = not(x) or not(y)
; With our variable indices and our representation, this is ((-v -v1) (-v -v2) ... (-v -vk) (-v1 -v2) ... (-vk-1 -vk))
; We need a helper variable that we can increase while still maintaining the knowledge of c
(defun amoc-helper (n c k inc)
  (let ((v (node2var n c k)) (v1 (node2var n (+ inc 1) k)))
  (cond
   ; Base case: If we reach k - 1 (since we are adding 1 in our output) then just return a list containing a list of the negated current node value and its next negated pair value.
   ((eql inc (- k 1)) (list (list (neg v) (neg v1))))
   ; Otherwise, we create the same list as in the base case, but construct it recursively to keep incrementing.
   (t (cons (list (neg v) (neg v1)) (amoc-helper n c k (+ inc 1))))
   )
  )
  )


; We now need to call our helper function on each color, so we get every pair.
(defun at-most-one-color (n c k)
  (cond
   ; Base case: If c is bigger than k - 1, just return nil.
   ((> c (- k 1)) '())
   ; Base case: If c is equal to k - 1, then just call the helper function on just this color.
   ((eql c (- k 1)) (amoc-helper n c k c))
   ; Otherwise, we need to recursively increment c until we reach k. This way we get every possible pair.
   (t (append (amoc-helper n c k c) (at-most-one-color n (+ c 1) k)))
   )
  )



; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."

; Exactly one is just the combination of at-least-one and at-most-one.
; Since we want the set {1, 2, ..., k}, c = 1 in this case.
(defun generate-node-clauses (n k)
  (cons (at-least-one-color n 1 k) (at-most-one-color n 1 k))
  )

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."

; In this functon, we need to iterate through each color and create a clause which ensures that the two nodes will not both be that color.
(defun gec-helper (e k inc)
  (let ((xv (node2var (car e) inc k)) (yv (node2var (car (cdr e)) inc k)))
    (cond
     ; Base case: If we are at the last possible color k, then just return the clause that is at most 1 of x value and y value.
     ((eql inc k) (list (list (neg xv) (neg yv))))
     ; Otherwise, we are not at the end so we recursively call, constructing the base case clauses along the way.
     (t (cons (list (neg xv) (neg yv)) (gec-helper e k (+ inc 1))))
    )
    )
  )

; All the work is done in the helper function.
; We want set {1, 2, ..., k}, so we start at 1.
(defun generate-edge-clauses (e k)
  (gec-helper e k 1)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun

(graph-coloring-to-sat "graph1-4.txt" "sat1.txt" 4)
