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


;; (defun convert-to-dos ()
;;   (interactive)
  
