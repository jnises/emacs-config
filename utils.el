;;; -*- lexical-binding: t -*-

(random t)

(defun sciss-get-random-person ()
  (interactive)
  (let ((persons ["per" "danne" "jawad" "urban" "marcus"]))
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

  
