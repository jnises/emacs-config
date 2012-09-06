;;; -*- lexical-binding: t -*-

(require 'cl)

(defun uniview-filter-file-list (files)
  (reduce (lambda (a b)
            (if (some (lambda (value) (equal b value)) '("." ".."))
                a
              ;; else
              (append a (list b))))
          files :initial-value '()))

(defvar uniview-sciss-profile-dir (concat (getenv "userprofile") "/sciss") "where the uniview user date is located")

(defun uniview-get-versions ()
  (uniview-filter-file-list (directory-files uniview-sciss-profile-dir)))

(defun uniview-load-latest-log (version)
  "open a buffer for the latest uniview log for version (dev, master, ...)"
  (interactive (list (ido-completing-read "version? " (uniview-get-versions))))
  (let* ((logdir (concat uniview-sciss-profile-dir "/" version "/Log"))
         (logfiles (mapcar 'car 
                           (sort
                            (directory-files-and-attributes logdir nil "^UniviewTheater\\.")
                            (lambda (a b)
                              (let ((adate (nth 5 (cdr a)))
                                    (bdate (nth 5 (cdr b))))
                                (or (> (car adate) (car bdate))
                                    (and (= (car adate) (car bdate))
                                         (> (cadr adate) (cadr bdate))))))))))
    (if logfiles
        (progn
          (find-file (concat logdir "/" (car logfiles)))
          (end-of-buffer)
          (auto-revert-tail-mode t))
      (error (concat "no logfiles found in " logdir)))))

(global-set-key (kbd "C-c u l") 'uniview-load-latest-log)

(provide 'uniview-tools)
