(defun ediff-merge-files-with-ancestor-command ()
  (let (file-a file-b file-anc file-out)
    ;; check for a -a flag, for filemerge compatibility
    (if (string= (car command-line-args-left) "-a")
	;; arguments are "-a ancestor file-a file-b file-out"
	(progn
	  (setq file-a (nth 2 command-line-args-left))
	  (setq file-b (nth 3 command-line-args-left))
	  (setq file-anc (nth 1 command-line-args-left))
	  (setq file-out (nth 4 command-line-args-left))
	  (setq command-line-args-left (nthcdr 5 command-line-args-left)))
        ;; arguments are "file-a file-b ancestor file-out"
        (setq file-a (nth 0 command-line-args-left))
        (setq file-b (nth 1 command-line-args-left))
        (setq file-anc (nth 2 command-line-args-left))
        (setq file-out (nth 3 command-line-args-left))
        (setq command-line-args-left (nthcdr 4 command-line-args-left)))
    (ediff-merge-files-with-ancestor
     file-a file-b file-anc nil file-out)))
