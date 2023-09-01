; Tyler Stovsky
; For DFSRL, the idea is that we should recursively call the function on the rest of the list, and then on the first element.
; This way, we can read get the order in right-to-left. We do this whenever we come across a list. When we see an atom, we
; want to turn it into a list so we can then append it to our final answer. If there is nothing, then we can just return nil
; so that it won't be appended to the final result.

; For FINAL-STATE, we are just checking if the current state is equal to the goal state.

; For NEXT-STATE, we are using the problem statement in order to ensure that a move we make is valid. If it is valid, then return that next state.
; Otherwise, return nil so we know we can't make that move.

; For SUCC-FN, we are just using NEXT-STATE to find which of the 4 moves are possible for a given state. We are returning these moves in a list.

; For ON-PATH, all we are doing is checking if the current state is contained in the stack. We do this by shrinking the stack each recursive call, checking
; if the first element is the current state every time.

; For MULT-DFS, we are just going through and running DFS at each state and checking if it actually ends up taking us to our goal state. If it does, then we
; have successfully found our answer. Otherwise, we just return nil.

; For DFS, first we check if we've reached our goal state. If we have, then we are done. If not, then we make sure we haven't seen the state yet,
; and then we run MULT-DFS on every possible successor of the current state.

;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; This function takes in a single argument FRINGE which represents a list of search trees, and returns a top-level
; list of lead nodes, in the order they are visited by right-to-left depth-first search. The initial call
; passes a FRINGE list with a single element: the root of the search tree.
(defun DFSRL (FRINGE)
  (cond
   ; If the current node is null, then we simply return nil.
   ((null FRINGE) ())
   ; If we are at a list, then we need to recursively call DFSRL on the cdr and car of FRINGE, and then append them together.
   ; Since we want right-to-left depth-first search, we want to append the cdr and then the car, as opposed to left-to-right which would be the opposite.
   ((listp FRINGE) (append (DFSRL (cdr FRINGE)) (DFSRL (car FRINGE))))
   ; Otherwise, we are at an atom. So, we should return this atom as a list.
   (t (list FRINGE))

   )
  )

;(write (DFSRL '(ROOT)))
;(terpri)
;(write (DFSRL '((((L E) F) T))))
;(terpri)
;(write (DFSRL '((R (I (G (H T)))))))
;(terpri)
;(write (DFSRL '(((A (B)) (D) C))))
;(terpri)
;(write (DFSRL '((T (H R E) E))))
;(terpri)
;(write (DFSRL '((A ((C ((E) D)) B)))))
;(terpri)

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
  (cond
   ; If our input state S is equal to the goal state (T T T T), then we return t.      
   ((equal '(t t t t) S) t)
   ; Otherwise, we return nil.
   (t ()))
  )


; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
  ; h = homer, b = baby, d = dog, p = poison. Defined variables because remembering which item is in which position in the list is confusing.
  (let ((h (first S))
	(b (second S))
	(d (third S))
	(p (fourth S)))
  (cond
   ; If we are trying to move homer:  
   ((equal A 'h)
    (cond
     ; If the baby and dog are together, and homer is accompanying the baby, then moving homer is invalid.
     ; Note: if the baby and dog are together, and homer is not with them, then moving homer is technically valid although the baby and dog
     ; should never be alone in the first place.
     ((and (equal h b) (equal b d)) ())
     ; If the baby and poison are together, and homer is accompanying the baby, then moving homer is invalid.
     ((and (equal h b) (equal b p)) ())
     ; Otherwise, we can safely move homer to the other side.
     (t (list (list (not h) b d p)))))
   ; If we are trying to move homer and the baby:
   ((equal A 'b)
    (cond
     ; If homer and the baby aren't together, then this is invalid.
     ((not (equal h b)) ())
     ; Otherwise, we can safely move homer and the baby to the other side.
     (t (list (list (not h) (not b) d p)))))
   ; If we are trying to move homer and the dog:
   ((equal A 'd)
    (cond
     ; If the baby and poison are together, then this is invalid since we can't move homer away.
     ((equal b p) ())
     ; If homer and the dog aren't together, then this is invalid.
     ((not (equal h d)) ())
     ; Otherwise, we can safely move homer and the dog to the other side.
     (t (list (list (not h) b (not d) p)))))
   ((equal A 'p)
    (cond
     ; If the baby and dog are together, then this is invalid since we can't move homer away.
     ((equal b d) ())
     ; If homer and the poison aren't together, then this is invalid.
     ((not (equal h p)) ())
     ; Otherwise, we can safely move homer and the poison to the other side.
     (t (list (list (not h) b d (not p))))))
   ; If A is not h, b, d, or p, then we can just return nil.
   (t ())
    )
   )
  )


; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
  ; NEXT-STATE is already created in such a way that if an input is invalid, then we just return nil.
  ; So, we can just run NEXT-STATE on all 4 options and append them to get the legal next states.
  (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p))
)


; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
  ; curr = first element in STATES, next = rest of elements in STATES
  (let ((curr (car STATES))
	(next (cdr STATES)))
  (cond
   ; If we reach the end, just return nil.  
   ((null STATES) ())
   ; We are simply checking if STATES contains S. If the first element in STATES is S, then we return t.
   ((equal S curr) t)
   ; Otherwise, we just cut off the first element and recursively call the function on the rest of the stack.
   (t (ON-PATH S next))
  )
 )
)


; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
  ; curr = first element in STATES, next = rest of elements in STATES
  (let ((curr (car STATES))
	(next (cdr STATES)))
    (cond
     ; If we reach the end, then we didn't find the complete path so return nil.
     ((null STATES) ())
     ; If the DFS returns nil, then we just cut off the first element and recursively call the function on the rest of STATES.
     ((null (DFS curr PATH)) (MULT-DFS next PATH))
     ; Otherwise, DFS actually found a path! Return it.
     (t (DFS curr PATH))
     )
    )
  )


; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
  ; new_path = path with our state appended
  (let ((new_path (append PATH (list S))))
  (cond
   ; If S is our goal state, then return our final path.
   ((FINAL-STATE S) new_path)
   ; If we aren't running into a cycle, then we want to run DFS on every possible successor state .
   ((not (ON-PATH S PATH)) (MULT-DFS (SUCC-FN S) new_path))
   ; Otherwise, we are in a cycle, so just return nil.
   (t ())
   )
  )
)

;(write (DFS '(NIL NIL NIL NIL) NIL))


