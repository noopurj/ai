;;; Compile and load the code
;;; Q1. The result for q1 exhaustive search is already ouputted
;;;
;;; Q2. To solve for other N, type (n-queen N) int the interpreter where N is the number of queens to be placed
;;;
;;; Q3. To solve by minimum conflict, type (min-conf N) in the interpreter where N is the number of queens
;;;
;;; Aim : Placing N queens on a NxN chess board such that no queen can attack another queen 
;;; Here queen number is equal to the index of the row


;; Assuming that we won't place a queen in the same row as another queen, there are total 4^4 posibilities
;; declaring an array *position* to store position of th queens, index number corresponds to the row number
(setf *position* (make-array 4 :initial-element -1))  ;initial position -1 i.e no queen has been placed
(setf position (make-array 4))

(defun solve (queen-no &optional (n 4))         ;default number of queens is 4
	"function to place queens on the board on safe poitions, parameter queenNo corresponds to which queen is being placed"
	(if (eq queen-no n)                                                           ; this means all queens have been placed
			(progn
				(dotimes (i n)
			 	  (setf (aref position i) (+ (aref *position* i) 1)))
				(print position))			 	          ; printing the solution
			(dotimes (i n)                                ; else, for this queen, find a safe position on that row
				(if (no-conflict queen-no i)              ; check if that position is safe
					(progn
					(setf (aref *position* queen-no) i)   ; placing the queen
					(solve (+ queen-no 1) n))))))           ; calling solve again for the next queen

(defun no-conflict (queen-no pos)
	"function to check if, given the current positions, placing another queen on position pos is safe"
	(dotimes (i queen-no t)                                      ; checking for all queens placed before, default return value of loop is true
		(if (or(eq (aref *position* i) pos)                      ; checking if its in the same column
			   (eq (aref *position* i) (- pos (- queen-no i)))   ; checking for same diagonal
			   (eq (aref *position* i) (+ pos (- queen-no i))))  ; checking for same diagonal
		(return-from no-conflict nil))))                         ; if any of the above is true return nil
		

(solve 0)  ; calling solve by first placing queen number 0 in the first row, solves for 4 queens

;;modification for N Queens
;; type the command (n-queen n) where n can be any number from 2,3,4...50 to output all possible solutions
(defun n-queen(n)
	(setf *position* (make-array n :initial-element -1))  ;initial position -1 i.e no queen has been placed
    (setf position (make-array n))
    (solve 0 n))

;; Solving it using the minimum coflict strategy
;; for n queens we first place them randomly over the board, each in a different row
;; each queen is right or left, to a pplace where the number of conflicts are minimum
;; This is done till the max number of steps(50) are over regardless of whether a complete solution is found or not
;; The solution found after 50 steps is outputted
(defun min-conf(n)
	"to find the best solution given a random arrangement of queens"
	(setf *position* (make-array n))
	(setf conflict (make-array n :initial-element 0))
	(setf position (make-array n))
	(dotimes (i n) 
		(setf (aref *position* i) (random n)))  ;randomly placing queens
	(format t "~%Random placement of queens : ~%")
	(princ *position*)
	(dotimes (i 50)                             ; only 50 steps allowed
		(dotimes (j n)
			(dotimes (pos n)                    ; if a better position is found, move queen there
				(if (< (conflicts j pos n *position*) (conflicts j (aref *position* j) n *position*))
					(setf (aref *position* j) pos)))))
	(dotimes (i n)
		(setf (aref position i) (+ (aref *position* i) 1)))
	(format t "~%Best Solution Found : ~%")
	(princ position)) 

(defun conflicts(queen pos n *position*)
	"to count the number of conflicts for that position given the current state"
	(setf conflicts 0)
	(dotimes (i n)
		(if (not (eq i queen))
			(if (or (eq (aref *position* i) pos)
				   (eq (aref *position* i) (- pos (- queen i)))
				   (eq (aref *position* i) (+ pos (- queen i))))
			(setf conflicts (+ conflicts 1)))))
	conflicts)


