;;; -*- lexical-binding: t -*-

(random t)

(defun sciss-get-random-person ()
  (interactive)
  (let ((persons ["per" "danne" "urban" "mike"]))
    (let ((person (elt persons (random (length persons)))))
      (if (called-interactively-p)
          (message person)
        person))))


(defun double-indentation-line ()
  "Double the number of spaces at the start of a line"
  (interactive)
  (save-excursion
    (beginning-of-line)
      (dotimes (i (skip-chars-forward " "))
        (insert " "))))


(defun multi-union (seta setb)
  "union of a multiset
not sure if is correctly implemented"
  (flet ((symbol< (a b) (string< (symbol-name a) (symbol-name b)))
         (setsort (set)
                  (sort set 'symbol<)))
    (let ((sorteda (setsort seta))
          (sortedb (setsort setb)))
      (flet ((recur (seta setb result)
                    (if seta
                        (if setb
                            (if (symbol< (car seta) (car setb))
                                (recur (cdr seta) setb (cons (car seta) result))
                              (if (symbol< (car setb) (car seta))
                                  (recur seta (cdr setb) (cons (car setb) result))
                                (recur (cdr seta) (cdr setb) result)))
                          (append seta result))
                      (append setb result))))
        (recur sorteda sortedb '())))))


(defun multi-intersection (seta setb)
  "intersection of a multiset
not sure if it is correctly implemented"
  (flet ((symbol< (a b) (string< (symbol-name a) (symbol-name b)))
         (setsort (set)
                  (sort set 'symbol<)))
    (let ((sorteda (setsort seta))
          (sortedb (setsort setb)))
      (flet ((recur (seta setb result)
                    (if (and seta setb)
                        (if (symbol< (car seta) (car setb))
                            (recur (cdr seta) setb result)
                          (if (symbol< (car setb) (car seta))
                              (recur seta (cdr setb) result)
                            (recur (cdr seta) (cdr setb) (cons (car seta) result))))
                      result)))
        (recur sorteda sortedb '())))))

(defun guid ()
  "return a guid
variant: standard
algorithm: v4
"
  (let ((hexmap ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"]))
    (cl-labels ((random-hex () (elt hexmap (random 16)))
                (hexstring (length) (let ((output "")) (dotimes (a length) (setq output (concat output (random-hex)))) output)))
      (concat (hexstring 8) "-" (hexstring 4) "-" "4" (hexstring 3) "-" (elt hexmap (logior #x8 (random 4))) (hexstring 3) "-" (hexstring 12)))))

(defun insert-guid ()
  (interactive)
  (insert (guid)))
