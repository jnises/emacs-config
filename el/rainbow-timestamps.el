;;; -*- lexical-binding: t -*-

;; ISO 8601
;; TODO handle more types of timestamps?
(setq rainbow-timestamps--regexp 
      "\\([0-9]\\{4\\}-[01][0-9]-[0-3][0-9]T[0-2][0-9]:[0-5][0-9]:[0-5][0-9]\\.[0-9]+\\([+-][0-2][0-9]:[0-5][0-9]\\|Z\\)\\)\\|\\([0-9]\\{4\\}-[01][0-9]-[0-3][0-9]T[0-2][0-9]:[0-5][0-9]:[0-5][0-9]\\([+-][0-2][0-9]:[0-5][0-9]\\|Z\\)\\)\\|\\([0-9]\\{4\\}-[01][0-9]-[0-3][0-9]T[0-2][0-9]:[0-5][0-9]\\([+-][0-2][0-9]:[0-5][0-9]\\|Z\\)\\)")


(defcustom rainbow-timestamps--period 60
  "time in seconds")

(defcustom rainbow-timestamps--saturation 60
  "from 0 to 100")

(defcustom rainbow-timestamps--lightness 60
  "from 0 to 100")

(defun rainbow-timestamps--time-to-color (time)
  (let ((theta (* 2 pi (/ time rainbow-timestamps--period))))
    (color-lab-to-srgb
     rainbow-timestamps--lightness
     (* rainbow-timestamps--saturation (cos theta))
     (* rainbow-timestamps--saturation (sin theta)))))

(defun rainbow-timestamps--propfn (end)
  (while (re-search-forward rainbow-timestamps--regexp end t)
    (let* ((start (match-beginning 0))
           (end (point))
           (time (time-to-seconds (encode-time (parse-time-string (buffer-substring-no-properties start end)))))
           (color (rainbow-timestamps--time-to-color time)))
      (add-face-text-property start end (list :foreground
                                              (apply 'format "#%02x%02x%02x"
                                                     (mapcar (lambda (x)
                                                               (floor (* (min 1 (max 0 x)) 255))) color))))))
  nil)

(defconst rainbow-timestamps--font-lock-keywords
  '(rainbow-timestamps--propfn))

;;;###autoload
(define-minor-mode rainbow-timestamps-mode
  "Color timestamps"
  :init-value nil
  :keymap nil
  (font-lock-remove-keywords nil rainbow-timestamps--font-lock-keywords)
  (when rainbow-timestamps-mode
    (font-lock-add-keywords nil rainbow-timestamps--font-lock-keywords 'append))
  ;; TODO do we need to ensure?
  (font-lock-flush)  
  (font-lock-ensure))


(defun rainbow-timestemps-set-period (period)
  (interactive "nperiod (seconds): ")
  (cl-assert (numberp period))
  (setq rainbow-timestamps--period period)
  ;;(font-lock-flush)
  (font-lock-ensure))


(provide 'rainbow-timestamps)
