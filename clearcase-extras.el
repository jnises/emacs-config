;; clearcase stuff

(require 'clearcase)

(defun remove-cc-revision (filename)
  "remove the clearcase revision from a filename"
  (if (string-match "@@.*$" filename)
      (substring filename 0 (match-beginning 0))
    ;; else
    filename))

(defun find-other-cc-revision (filename)
  "a find-file with the current clearcase revision filled in by default"
  (interactive
   (let ((clean-filename (remove-cc-revision (buffer-file-name))))
     (list (read-file-name "Find file: " (get-directory-from-path clean-filename) nil nil (concat "/" (get-filename-from-path clean-filename) "@@" (clearcase-fprop-version (buffer-file-name)))))))
  (find-file filename))


(defun clearcase-extras-unres-co (file comment)
  "Check out a file unreserved. Defaults to the file visited by the current buffer.
This function doesn't print error messages from clearcase properly"
  (interactive (list buffer-file-name (read-from-minibuffer "comment: " )))
  (let ((buffer (get-file-buffer file)))
    (when buffer
        (when (buffer-modified-p)
          (error "can't check out a file visited by a buffer that is modified")))
    (let ((buffername "*clearcase-extras-unres-co*"))
      (with-output-to-temp-buffer buffername
        (shell-command (concat "cleartool co -unres -c \"" comment "\" " file) buffername)))
    (when buffer
      (with-current-buffer buffer
        (revert-buffer)))))
        

(provide 'clearcase-extras)

