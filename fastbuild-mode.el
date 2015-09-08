(require 'js)
(require 'noflet)

;; indentation (stolen from js.el

(defun fb-indent-line ()
  "Indent the current line as Fastbuild"
  (interactive)
  ;; override some js indentation rules
  (let ((js--indent-operator-re "[-+*/%<>&^|?:]\\([^-+*/]\\|$\\)\\|!?=\\|")
        (jsindentation 'js--proper-indentation))
    (noflet ((js--proper-indentation (parse-status)
                                     (if (= 0 (line-number-at-pos))
                                         0
                                       (funcall jsindentation parse-status)))))
    (js-indent-line)))

(define-derived-mode fastbuild-mode js-mode "FB"
  (setq-local indent-line-function 'fb-indent-line)
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (modify-syntax-entry ?\; "<" fastbuild-mode-syntax-table)
  (modify-syntax-entry ?/ "<12" fastbuild-mode-syntax-table)
  (modify-syntax-entry ?\n ">" fastbuild-mode-syntax-table)
  (modify-syntax-entry ?^ "\\" fastbuild-mode-syntax-table)
  (modify-syntax-entry ?\\ "." fastbuild-mode-syntax-table)
  (modify-syntax-entry ?' "\"" fastbuild-mode-syntax-table)
  (c-update-modeline))

(provide 'fastbuild-mode)
