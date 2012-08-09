;;; -*- lexical-binding: t -*-

(defun uniview-filter-file-list (files)
  (reduce (lambda (a b)
            (if (some (lambda (value) (equal b value)) '("." ".."))
                a
              ;; else
              (append a (list b))))
          files :initial-value '()))

(setq uniview-sciss-profile-dir (concat (getenv "userprofile") "/sciss"))

(defun uniview-get-versions ()
  (uniview-filter-file-list (directory-files uniview-sciss-profile-dir)))

(mapcar 'car 
        (sort
         (directory-files-and-attributes "c:/users/joel/sciss/dev/log" nil "^UniviewTheater\\.")
         (lambda (a b)
           (let ((adate (nth 5 (cdr a)))
                 (bdate (nth 5 (cdr b))))
             (or (> (car adate) (car bdate))
                 (and (= (car adate) (car bdate))
                      (> (cadr adate) (cadr bdate))))))))

(nth 5 (file-attributes "uniview-tools.el"))

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
