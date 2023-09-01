; Tyler Stovsky 705512370
;CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )


(defun isWall (v)
  (= v wall)
  )


(defun isBox (v)
  (= v box)
  )


(defun isKeeper (v)
  (= v keeper)
  )


(defun isStar (v)
  (= v star)
  )


(defun isBoxStar (v)
  (= v boxstar)
  )


(defun isKeeperStar (v)
  (= v keeperstar)
  )


;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  )
;end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  )
;end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
					;

; This function takes in a list r which represents one row of a state.
; It outputs true if the row contains a box, and false otherwise.
(defun contains-box (r)
  (cond
   ; If we reach the end, no box was found.
   ((null r) nil)
   ; If the first element is a box, return true.
   ((isBox (car r)) t)
   ; Otherwise, keep looking at the rest of the list to see if there is a box.
   (t (contains-box (cdr r)))

   )
  )



; This function takes in a state s.
; It returns true if it is a goal state, and false otherwise.
; If there is a box in the state, then we are not at a goal state since a box is no longer represented as a box when it reaches a goal.
(defun goal-test (s)
  (cond
   ; If we reach the end of the state, then we didn't find a box so this is a goal state.
   ((null s) t)
   ; If we found a box in a row, then we are not at a goal state.
   ((contains-box (car s)) nil)
   ; Otherwise, we need to keep checking the state to see if there is a box later.
   (t (goal-test (cdr s)))
   )
  )


;end defun


; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 

; This function takes in a list x and an integer k and returns the kth element in the list.
(defun find-kth (k x)
  (cond
   ; If we reach the end, or x is not a list, then return 1 which represents a wall.
   ((null x) 1)
   ((atom x) 1)
   ; Base case: if k = 0, then just return the first element in the list.
   ((= k 0) (car x))
   ; Otherwise, keep going through the list until k = 0.
   (t (find-kth (- k 1) (cdr x)))
   )
  )


; This function takes in a list x, an integer k and a value v and returns a new list where the kth element is replaced with the element v.
(defun replace-kth (k x v)
  (cond
   ; If we reach the end then return nil.
   ((null x) nil)
   ; Base case: if k = 0, then return a list of v constructed with the rest of the list.
   ((= k 0) (cons v (cdr x)))
   ; Otherwise, we haven't found the kth element yet so keep looking through the list.
   (t (cons (car x) (replace-kth (- k 1) (cdr x) v)))
   )
  )


; This function takes in a list x and returns the length of the list.
(defun find-length (x)
  (cond
   ; If list is empty return 0.
   ((not x) 0)
   ; Otherwise keep adding 1 until we reach the end.
   (t (+ (find-length (cdr x)) 1))
   )
  )


; This function takes in a state s, a row r, a column c and a value v and returns a new state where the value at (r, c) is replaced by the value v.
(defun set-square (s r c v)
  (cond
   ; Don't set the square if r or c are out of bounds.
   ((> r (- (find-length s) 1)) nil)
   ((> c (- (find-length (find-kth 0 s)) 1)) nil)
   ; Find the row, then update the new value at in the correct column, then return the whole new state by replacing the entire state with the new row.
   (t (replace-kth r s (replace-kth c (find-kth r s) v)))
   )
  )



; This function takes in a state s, a row r and a column c and returns the integer at coordinates (r, c).
(defun get-square (s r c)
  ; Find the rth row, and then find the cth element in that row.
  (find-kth c (find-kth r s))
  )


; This function takes in a state s, the position of the keeper (x, y) and the value of the keeper v and returns the new value once the keeper is moved.
(defun update-keeper (s x y v)
  (cond
   ; If the keeper is on a star, then when it is moved a star will remain.
   ((eql v keeperstar) (set-square s x y star))
   ; If the keeper is by itself, then when it is moved a blank will remain.
   ((eql v keeper) (set-square s x y blank))
   )

  )


; This function takes in a state s, a direction d, and the position of the keeper (x, y) and returns the new state after a keeper moves in the direction d (if it is valid).
; 0: Down, 1: Up, 2: Right, 3: Left
(defun try-move (s d x y)
  ; The keeper must move if valid, so set new_s to be the new state if the keeper were to move.
  (let* ((new_s (update-keeper s x y (get-square s x y))))
  (cond
   ; Case 1: Try to move down
   ((eql d 0)
    (cond
     ; Star -> Keeperstar
     ((isStar (get-square s (+ x 1) y)) (set-square new_s (+ x 1) y keeperstar))
     ; Blank -> Keeper
     ((isBlank (get-square s (+ x 1) y)) (set-square new_s (+ x 1) y keeper))
     ; Moving a boxstar to a blank: Keeper -> Keeperstar, Boxstar -> Box
     ((and (isBoxStar (get-square s (+ x 1) y)) (isBlank (get-square s (+ x 2) y))) (set-square (set-square new_s (+ x 1) y keeperstar) (+ x 2) y box))
     ; Moving a boxstar to a star: Keeper -> Keeperstar, Boxstar -> Boxstar
     ((and (isBoxStar (get-square s (+ x 1) y)) (isStar (get-square s (+ x 2) y))) (set-square (set-square new_s (+ x 1) y keeperstar) (+ x 2) y boxstar))
     ; Moving a box to a blank: Keeper -> Keeper, Box -> Box
     ((and (isBox (get-square s (+ x 1) y)) (isBlank (get-square s (+ x 2) y))) (set-square (set-square new_s (+ x 1) y keeper) (+ x 2) y box))
     ; Moving a box to a star: Keeper -> Keeper, Box -> Boxstar
     ((and (isBox (get-square s (+ x 1) y)) (isStar (get-square s (+ x 2) y))) (set-square (set-square new_s (+ x 1) y keeper) (+ x 2) y boxstar))
     ; Otherwise move is not valid
    (t nil)))
   ; Case 2: Try to move up
   ((eql d 1)
    (cond
    ((isStar (get-square s (- x 1) y)) (set-square new_s (- x 1) y keeperstar))
    ((isBlank (get-square s (- x 1) y)) (set-square new_s (- x 1) y keeper))
    ((and (isBoxStar (get-square s (- x 1) y)) (isBlank (get-square s (- x 2) y))) (set-square (set-square new_s (- x 1) y keeperstar) (- x 2) y box))
    ((and (isBoxStar (get-square s (- x 1) y)) (isStar (get-square s (- x 2) y))) (set-square (set-square new_s (- x 1) y keeperstar) (- x 2) y boxstar))
    ((and (isBox (get-square s (- x 1) y)) (isBlank (get-square s (- x 2) y))) (set-square (set-square new_s (- x 1) y keeper) (- x 2) y box))
    ((and (isBox (get-square s (- x 1) y)) (isStar (get-square s (- x 2) y))) (set-square (set-square new_s (- x 1) y keeper) (- x 2) y boxstar))
    (t nil)))
   ; Case 3: Try to move right
   ((eql d 2)
    (cond 
    ((isStar (get-square s x (+ y 1))) (set-square new_s x (+ y 1) keeperstar))
    ((isBlank (get-square s x (+ y 1))) (set-square new_s x (+ y 1) keeper))
    ((and (isBoxStar (get-square s x (+ y 1))) (isBlank (get-square s x (+ y 2)))) (set-square (set-square new_s x (+ y 1) keeperstar) x (+ y 2) box))
    ((and (isBoxStar (get-square s x (+ y 1))) (isStar (get-square s x (+ y 2)))) (set-square (set-square new_s x (+ y 1) keeperstar) x (+ y 2) boxstar))
    ((and (isBox (get-square s x (+ y 1))) (isBlank (get-square s x (+ y 2)))) (set-square (set-square new_s x (+ y 1) keeper) x (+ y 2) box))
    ((and (isBox (get-square s x (+ y 1))) (isStar (get-square s x (+ y 2)))) (set-square (set-square new_s x (+ y 1) keeper) x (+ y 2) boxstar))
    (t nil)))
   ; Case 4: Try to move left
   ((eql d 3)
    (cond
    ((isStar (get-square s x (- y 1))) (set-square new_s x (- y 1) keeperstar))
    ((isBlank (get-square s x (- y 1))) (set-square new_s x (- y 1) keeper))
    ((and (isBoxStar (get-square s x (- y 1))) (isBlank (get-square s x (- y 2)))) (set-square (set-square new_s x (- y 1) keeperstar) x (- y 2) box))
    ((and (isBoxStar (get-square s x (- y 1))) (isStar (get-square s x (- y 2)))) (set-square (set-square new_s x (- y 1) keeperstar) x (- y 2) boxstar))
    ((and (isBox (get-square s x (- y 1))) (isBlank (get-square s x (- y 2)))) (set-square (set-square new_s x (- y 1) keeper) x (- y 2) box))
    ((and (isBox (get-square s x (- y 1))) (isStar (get-square s x (- y 2)))) (set-square (set-square new_s x (- y 1) keeper) x (- y 2) boxstar))
    (t nil)))
    


   )

  )
  )


; This function takes in a state s and returns all possible successor states.
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s 0 y x) (try-move s 1 y x) (try-move s 2 y x) (try-move s 3 y x)))
	 )
    (cleanUpList result);end
   );end let
  )


;

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
; Trival function, just return 0.
(defun h0 (s)
  0
  )



; This function takes in an atom and returns 1 if it is a box or 0 otherwise. 
(defun count-box (a)
  (cond
   ((isBox a) 1)
   (t 0))
  )


; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.

; This function takes in a state s and returns the number of misplaced boxes.
(defun h1 (s)
  (cond
   ; If s is null, return 0.
   ((null s) 0)
   ; If s is an atom, then count it if it is a box.
   ((atom s) (count-box s))
   ; Otherwise, recursively call to check every element in the list.
   (t (+ (h1 (car s)) (h1 (cdr s))))
  )
 )



; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.

; This function takes in a row r, a position (x, y), a value v, and an output (which will be pairs of coordinates, starts initially empty) and returns the final output.
; The final output are pairs of coordinates (x, y) that match the value v in the list.
(defun get-positions (r x y v output)
  (cond
   ; When we reach the end, return the final output.
   ((null r) output)
   ; If the first element equals the value, then append the coordinates to our output. Recursively call the function on the rest of the list and increment our column.
   ((eql v (car r)) (get-positions (cdr r) x (+ y 1) v (append output (list (list x y)))))
   ; Otherwise the value is not equal.  Recursively call without appending to the output.
   (t (get-positions (cdr r) x (+ y 1) v output))
   )
  )

; This function takes in a state s, a value v, a row r and an output (which will be pairs of coordinates, initially empty) and returns the final output.
; The final output are pairs of coordinates (x, y) that match the value v in s.
(defun get-state-positions (s v r output)
  (cond
   ; When we reach the end, return the final output.
   ((null s) output)
   ; Recursively call function on each row in s, finding the coordinates using get-positions. Increment r each time so we know the row's coordinate value.
   (t (get-state-positions (cdr s) v (+ r 1) (append output (get-positions (car s) r 0 v '()))))
   )
  )

; This function takes in an integer x and returns the absolute value.
(defun abs-val (x)
  (cond
   ; x > 0 -> keep same x
   ((> x 0) x)
   ; x < 0 -> 0 - x = +x
   ((< x 0) (- 0 x))
   ; x = 0 -> 0
   (t 0)
   )
  )

; This function takes in two arguments a and b which are lists of of the form (r, c).
; It returns the manhattan distance between the two points, which is abs(a1 - b1) + abs(a2 - b2).
(defun dist (a b)
  (+ (abs-val (- (car a) (car b))) (abs-val (- (car (cdr a)) (car (cdr b)))))
  )


; This function takes in a position of a box (r, c), the list of all goal positions ((r1, c1), (r2, c2), ...), and the final output (initially 0).
; It returns the distance from the box to the closest goal.
(defun smallest-dist (pos goals output)
  (cond
   ; When we reach the end, return the final output.
  ((null goals) output)
  (t
   (cond
    ; If the distance is less than our current min, then recursively call on rest of goals and update the output to the new current min.
    ((< (dist pos (car goals)) output) (smallest-dist pos (cdr goals) (dist pos (car goals))))
    ; Otherwise, recursively call and keep the same output since we didn't find a smaller distance.
    (t (smallest-dist pos (cdr goals) output)))
   )
  )
  )

; This function takes in the location of all the boxes and all the goals, and a final output (initially 0).
; It returns the final manhattan distance, which is the sum of each boxes closest distance to a goal.
(defun manhattan-dist (boxes goals output)
  (cond
   ; If we reach the end of the boxes or goals, return the final output.
   ((null boxes) output)
   ((null goals) output)
   ; Recursively call the function on each box, finding their smallest distance to a goal.
   ; Note of (dist (car boxes) (car goals)): We need something to intitially compare to, so just use the first distance.
   (t (manhattan-dist (cdr boxes) goals (+ output (smallest-dist (car boxes) (cdr goals) (dist (car boxes) (car goals))))))
  
   )
  )

; This function takes in a state s and returns the sum of each boxes closest distance to a goal.
; It is computed by the function manhattan-dist, which is commented above.
(defun h705512370 (s)
  (manhattan-dist (get-state-positions s box 0 '()) (get-state-positions s star 0 '()) 0)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun

(load-a-star)
(printstates (a* p14 #'goal-test #'next-states #'h705512370) 0.5)
(print 0)
