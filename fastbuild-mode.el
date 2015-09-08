;;; -*- lexical-binding: t -*-

(define-derived-mode fastbuild-mode c-mode "FB"
  (setq comment-start ";")
  (setq comment-end "")
  (modify-syntax-entry ?\; "<" fastbuild-mode-syntax-table)
  (modify-syntax-entry ?/ "<12" fastbuild-mode-syntax-table)
  (modify-syntax-entry ?\n ">" fastbuild-mode-syntax-table)
  (modify-syntax-entry ?^ "\\" fastbuild-mode-syntax-table)
  (modify-syntax-entry ?\\ "." fastbuild-mode-syntax-table)
  (modify-syntax-entry ?' "\"" fastbuild-mode-syntax-table)
  (c-update-modeline))

(provide 'fastbuild-mode)
