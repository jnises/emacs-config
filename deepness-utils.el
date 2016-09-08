;;; -*- lexical-binding: t -*-

(random t)

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

(when (fboundp 'magit-git-lines)
  (defun magit-find-file-ido ()
    "Uses a completing read to open a file from git ls-files"
    (interactive)
    (let ((default-directory (magit-get-top-dir)))
      (if default-directory
          (find-file
           (ido-completing-read "File? "
                                (magit-git-lines "ls-files" "--exclude-standard" "-co")))
        (error "Not a git repository.")))))

(defun get-file-coding-system (filename)
  (interactive "f")
  (with-current-buffer
      (find-file-noselect filename)
    buffer-file-coding-system))

(defun set-env-from-bash-profile (varname)
  (let ((vardata (shell-command-to-string (format ". ~/.bash_profile; echo -n $%s" varname))))
    (when (not (= 0 (length vardata)))
      (setenv varname vardata))))

(defun load-overtone-stuff ()
  (interactive)
  (global-set-key (kbd "C-c o s") (lambda () (interactive) (nrepl-send-string "(stop)" (lambda (ignored))))))

(defun download-file-if-not-exist (url path &optional sha1)
  (unless (file-exists-p path)
    (url-retrieve url
                  (lambda (status) (unwind-protect
                                       (progn
                                         (goto-char (point-min))
                                         (unless (looking-at "HTTP/1.1 200 OK")
                                           (error (concat "Error downloading " url)))
                                         ;; strip the headers
                                         (search-forward "\n\n")
                                         (delete-region 1 (point))
                                         (unless (or (not sha1) (string-equal (secure-hash 'sha1 (current-buffer)) sha1))
                                           (error (concat "Error: " url " does not have the expected hash")))
                                         (write-file path))
                                     (kill-buffer))))))

(provide 'deepness-utils)
