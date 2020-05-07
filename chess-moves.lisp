;;  Copyright (C) 2020  Alan Tseng

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <https://www.gnu.org/licenses/>.

(defun in-range (x)
  (and (<= 0 x) (< x 8)))

(defmacro with-position (x body)
  `(let ((col (car ,x))
	 (row (cdr ,x)))
     ,body))

(defun in-range-pos (x)
  (with-position x
    (and (in-range col)
	 (in-range row))))

(defun to-index (x)
  (with-position x
    (+ (* 8 col) row)))

;; Returns a closure that returns position relative to starting position
(defun position-adder (delta-col delta-row)
  (lambda (x)
    (with-position x
      (cons (+ col delta-col)
	    (+ row delta-row)))))
;; (funcall (position-adder -1 1) (cons 0 0))

;; Returns coordinates of squares along a given ray from initial-pos
(defun ray (delta-col delta-row)
  (lambda (initial-pos)
    (let* ((adder (position-adder delta-col delta-row))
	   (pos (funcall adder initial-pos)))
      (loop while (in-range-pos pos)
	 collect pos
	 do (setf pos (funcall adder pos))))))
;; (funcall (ray 1 1) (cons 0 0))

;; Collects positions along multiple rays or displacements from given position
(defun displace (ray-funcs pos)
  (mapcar (lambda (func)
	    (funcall func pos))
	  ray-funcs))

;; Places where each chess piece can move starting from given position
(defun rook-moves (pos)
  (displace
   (list (ray -1 0)
	 (ray 1 0)
	 (ray 0 -1)
	 (ray 0 1))
   pos))
;; (rook-moves (cons 2 4))

(defun bishop-moves (pos)
  (displace
   (list (ray -1 -1)
	 (ray -1 1)
	 (ray 1 -1)
	 (ray 1 1))
   pos))
;; (bishop-moves (cons 3 2))

(defun queen-moves (pos)
  (append (rook-moves pos)
	  (bishop-moves pos)))
;; (queen-moves (cons 3 3))

;; Returns a function that returns valid places a piece can move to
(defun valid-displacements (displacements)
  (lambda (pos)
    (remove-if-not
     #'in-range-pos
     (displace
      (mapcar (lambda (delta)
		(position-adder (car delta)
				(cdr delta)))
	      displacements)
      pos))))

(defun knight-moves (pos)
  (mapcar #'list
	  (funcall
	   (valid-displacements
	    '((-1 . -2) (-1 . 2)
	      (1 . -2) (1 . 2)
	      (-2 . -1) (-2 . 1)
	      (2 . -1) (2 . 1)))
	   pos)))
;; (knight-moves (cons 1 0))

(defun king-moves (pos)
  (mapcar #'list
	  (funcall
	   (valid-displacements
	    '((-1 . -1) (-1 . 0) (-1 . 1)
	      (0 . -1) (0 . 1)
	      (1 . -1) (1 . 0) (1 . 1)))
	   pos)))
;; (king-moves (cons 3 3))

(defun white-pawn-moves (pos)
  (list
   (with-position pos
     (cond ((equal row 1) ;; Pawn in this row can move 1 or 2 spaces
	    (list (cons col (+ row 1))
		  (cons col (+ row 2))))
	   (t (funcall
	       (valid-displacements
		'((0 . 1)))
	       pos))))))

(defun black-pawn-moves (pos)
  (list
   (with-position pos
     (cond ((equal row 6) ;; Pawn in this row can move 1 or 2 spaces
	    (list (cons col (- row 1))
		  (cons col (- row 2))))
	   (t (funcall
	       (valid-displacements
		'((0 . -1)))
	       pos))))))

;; Pawns capture diagonally
(defun white-pawn-capture-moves (pos)
  (mapcar #'list
	  (funcall
	   (valid-displacements
	    '((-1 . 1) (1 . 1)))
	   pos)))
;; (white-pawn-capture-moves '(3 . 2))

(defun black-pawn-capture-moves (pos)
  (mapcar #'list
	  (funcall
	   (valid-displacements
	    '((-1 . -1) (1 . -1)))
	   pos)))
;; (black-pawn-capture-moves '(4 . 5))

;; Not included:
;;   - castling
;;   - en passant
;;   - pawn promotion

