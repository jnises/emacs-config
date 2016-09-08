;;; -*- lexical-binding: t -*-

;; put something like
;; (setq home-el-path "~/.emacs.d/el")
;; (load (concat home-el-path "/config.el"))
;; in your .emacs

;; some customize shit. doesn't seem to be possible to set default tab-width without it
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;;'(safe-local-variable-values (quote ((c-indentation-style . stroustrup))))
 '(tab-width 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background "Gray12" :foreground nil)))))

(unless (boundp 'el-path)
  (setq el-path (concat (getenv "HOME") "/.emacs.d/el")))
(setq external-el-path (concat el-path "/external"))
(setq downloaded-el-path (concat el-path "/downloaded"))
(setq downloaded-tmp-path (concat el-path "/tmp"))
(unless (file-directory-p downloaded-el-path)
  (make-directory downloaded-el-path))

(add-to-list 'load-path el-path)
(add-to-list 'load-path external-el-path)
(add-to-list 'load-path downloaded-el-path)

;; better package repo
(when (require 'package nil t)
  ;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
  (package-initialize))

(if (not (package-installed-p 'use-package))
    (package-refresh-contents)
    (package-install 'use-package))

(use-package paredit
  :ensure t
  :commands paredit-mode)

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :config
  (use-package color)
  (defun labhsl-to-rgb (h s l)
    "
hsl to rgb by way of lab
l is lab l, so the range is 0 to 100
"
    (let ((a (* (cos h) s))
          (b (* (sin h) s)))
      (mapcar (lambda (x) (max (min x 1) 0)) (color-lab-to-srgb l a b))))

  ;; better colors for rainbow delimiters
  (dotimes (n 9)
    (let ((rainbowfaces '(rainbow-delimiters-depth-1-face
                          rainbow-delimiters-depth-2-face
                          rainbow-delimiters-depth-3-face
                          rainbow-delimiters-depth-4-face
                          rainbow-delimiters-depth-5-face
                          rainbow-delimiters-depth-6-face
                          rainbow-delimiters-depth-7-face
                          rainbow-delimiters-depth-8-face
                          rainbow-delimiters-depth-9-face))
          (shuffledn (nth n '(3 7 1 4 6 0 5 8 2))))
      (set-face-foreground (nth n rainbowfaces)
                           (apply 'format "#%02x%02x%02x" (mapcar (lambda (x) (floor (* x 255))) (labhsl-to-rgb (* (/ shuffledn 9.0) pi 2) 130 80)))))))

(use-package deepness-utils
  :load-path el-path)

(setq default-packages '(clojure-mode
                         lua-mode
                         highlight-indentation
                         yascroll
                         undo-tree
                         multi-web-mode
                         ;;smex
                         projectile
                         js2-mode
                         magit
                         helm
                         helm-projectile
                         helm-ls-git
                         helm-gtags
                         ggtags
                         use-package))

(when (string-equal system-type "darwin")
  (add-to-list 'default-packages 'exec-path-from-shell))

(defun install-default-packages ()
  " install useful packages, call this method when starting emacs on a new machine "
  (interactive)
  (package-refresh-contents)
  (mapc (lambda (package)
          (if (not (package-installed-p package)) 
              (package-install package)))
        default-packages))

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

(defun initial-setup ()
  (interactive)
  (install-default-packages)
  ;; system specific setup
  (cond ((string-equal system-type "darwin")
         ;; download gud from apple that supports lldb
         (download-file-if-not-exist "http://www.opensource.apple.com/source/lldb/lldb-69/utils/emacs/gud.el?txt" (concat downloaded-el-path "/gud.el") "108a76a8d5d8ffa6aca950a103294a012bb606f9"))))

;; faster startup
(modify-frame-parameters nil '((wait-for-wm . nil)))

(setq inhibit-splash-screen t)

;; disable the toolbar
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))

;; show matching parens
(show-paren-mode t)

;; enable syntax highlighting
(global-font-lock-mode t)

;; show marked region
(transient-mark-mode t)

;; no tabs!!!
(setq-default indent-tabs-mode nil)

;; but if there is, set the default tab width (determines how a tab is displayed)
(setq default-tab-width 4)

;; fix grep stuff
(require 'grep)
(when (fboundp 'grep-apply-setting)
  (grep-apply-setting 'grep-find-command '("find . -type f ! -path \"*.git*\" -exec grep -nH -e  {} +" . 51)))

;; change color theme
(if (window-system)
  (load-theme 'tsdh-dark))

;; highlight line
(global-hl-line-mode 1)
(hl-line-mode 1)

;; nicer ediff
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; display line and column
(line-number-mode t)
(column-number-mode t)

;; TODO use ivy instead of helm/ido?

;; enable ido mode
(defun start-ido-mode ()
  (interactive)
  (ido-mode t)
  (setq ido-enable-flex-matching t)
  ;; disable auto searching for files unless called explicitly with C-c C-s
  (setq ido-auto-merge-delay-time 99999)
  (define-key ido-file-dir-completion-map (kbd "C-c C-s")
    (lambda()
      (interactive)
      (ido-initiate-auto-merge (current-buffer))))
  (when (fboundp 'magit-find-file-ido)
    (global-set-key (kbd "C-c f") 'magit-find-file-ido)))

(start-ido-mode)
(if (and (require 'helm nil t) (require 'helm-config nil t))
    (progn
      (setq ;;helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
       ;; helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
       ;;helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
       ;;helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
       ;;helm-ff-file-name-history-use-recentf t
       helm-mode-fuzzy-match t
       helm-completion-in-region-fuzzy-match t
       helm-M-x-fuzzy-match t
       helm-buffers-fuzzy-matching t
       ;;helm-recentf-fuzzy-match t
       )
      ;(helm-mode 1)
      ;(global-set-key (kbd "C-x C-f") 'helm-find-files)
      (global-set-key (kbd "M-x") 'helm-M-x)
      (global-set-key (kbd "C-x b") 'helm-mini)
      (global-set-key (kbd "M-y") 'helm-show-kill-ring))
  ;; fall back to ido-mode if helm is not available
  ;;(start-ido-mode)
  )

;; disable directory caching on windows
(when (equal system-type 'windows-nt)
  (setq ido-max-dir-file-cache 0)) 

;; font stuff
(when (and (or (> emacs-major-version 24) (and (>= emacs-major-version 24) (>= emacs-minor-version 4))) window-system)
  (cl-flet ((set-face-font-if-it-exists (target fontname)
                                        (when (x-list-fonts fontname)
                                          (set-face-font target fontname))))
    (set-face-font-if-it-exists 'default "-unknown-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")))

;; enable improved window switching, disable if you don't want it to clobber
;; shift-<arrowkeys> for selection
(windmove-default-keybindings)

;; nicer buffer names
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'forward))

;; load cedet
;; (when (require 'cedet nil t)
;;   (global-ede-mode t)
;;   (require 'semantic/sb)
;;   (semantic-mode t)
;;   (setq semantic-idle-work-update-headers-flag t)
;;   ;;(global-semantic-mru-bookmark-mode 1)
;;   (global-semanticdb-minor-mode t)

;;   ;; use shorter symref tag listing
;;   (setq semantic-symref-results-summary-function 'semantic-format-tag-canonical-name)

;;   ;; enable sticky function globaly
;;   (global-semantic-stickyfunc-mode 1)

;;   ;; enable goto symbol
;;   (add-to-list 'load-path (concat el-path "/ido-goto-buffer-tag/"))
;;   (when (require 'ido-goto-buffer-tag nil t) (global-set-key (kbd "C-c i") 'ido-goto-buffer-tag)))

;; windows only stuff
(when (string-equal system-type "windows-nt")
  ;; set cygwin path for w32 emac
  ;; TODO only use git bash unix utils?
  ;;(setq w32shell-cygwin-bin "C:/cygwin/bin")
  ;;(setq gnuwin-path "c:/local/gnuwin32/bin")
  (setq cygwin-root-directory (if (file-exists-p "c:/cygwin64") "c:/cygwin64" "c:/cygwin"))
  (setq cygwin-bin-path (concat cygwin-root-directory "/bin"))
  (setenv "PATH"
          (mapconcat 'identity
                     (mapcar (lambda (path) (replace-regexp-in-string "/" "\\\\" path)) (list ;;gnuwin-path
                                 ;;w32shell-cygwin-bin
                                 cygwin-bin-path
                                 "C:/Program Files/Git/bin"
                                 (getenv "PATH")))
                     ";"))

  (add-to-list 'exec-path cygwin-bin-path)
  (add-to-list 'exec-path "C:/Program Files/Git/bin")
  ;; (set-variable 'find-program "find.exe")
  ;; (set-variable 'grep-program "grep.exe")
  (and (require 'cygwin-mount nil t) (require 'setup-cygwin nil t)))

(when (string-equal system-type "darwin")
  ;; use apple gud that supports lldb
  (let ((path (concat downloaded-el-path "/gud")))
    (if (file-exists-p path)
        (load path))))

;; enable indentation highlighting for modes that benefit from them (python)
(if (window-system)
    (when (require 'highlight-indentation nil t)
      (add-hook 'python-mode-hook (lambda ()
                                    (highlight-indentation-mode t)))))

;; python stuff
(add-hook 'python-mode-hook (lambda ()
                              ;; tab width is a mess, so force python to use the correct one
                              (setq tab-width 4)
                              (setq python-indent 4)
                              ;(semantic-mode 1)
                              (let ((pycommand (if (string-equal system-type "windows-nt")
                                                   "py -3"
                                                 "ipython")))
                                (setq python-shell-interpreter pycommand)
                                (setq py-python-command pycommand))
                              ))

;; disable electric indent everywhere since it doesn't seem possible to disable it for python only
(electric-indent-mode -1)

;(setq gud-pdb-command-name "python -i -m pdb")

(when (or (< emacs-major-version 24) (and (= emacs-major-version 24) (< emacs-minor-version 4)))
  (set-default-coding-systems 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))

(require 'git nil t)

;; better eshell behaviour
(setq eshell-cmpl-cycle-completions nil)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun c-style-hook-function ()
  (interactive)
  (c-set-style "linux")
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'inline-open 0)
  ;;(semantic-mode t)
  (setq show-trailing-whitespace t))
(add-hook 'c-mode-common-hook 'c-style-hook-function)

(load (concat el-path "/fulpdb.el"))

;; use rainbow delimiters and paredit mode for some lisp files
(dolist (hook '(clojure-mode-hook
                emacs-lisp-mode-hook
                scheme-mode-hook)) (add-hook hook (lambda ()
                                                    (rainbow-delimiters-mode t)
                                                    (paredit-mode t))))

;; next/prev error shortcuts
(global-set-key (kbd "<f5>") 'previous-error)
(global-set-key (kbd "<f6>") 'next-error)

(global-set-key (kbd "C-c c c") 'comment-region)
(global-set-key (kbd "C-c c u") 'uncomment-region)

(if (window-system)
    (when (require 'yascroll nil t)
      (scroll-bar-mode -1)
      (global-yascroll-bar-mode t)
      (set-face-background 'yascroll:thumb-text-area "Gray80")
      (set-face-background 'yascroll:thumb-fringe "Gray80")
      (set-face-foreground 'yascroll:thumb-fringe "Gray80")))

(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

(defun load-overtone-stuff ()
  (interactive)
  (global-set-key (kbd "C-c o s") (lambda () (interactive) (nrepl-send-string "(stop)" (lambda (ignored))))))
  
;; change magit settings
(eval-after-load 'magit
  '(progn
     (when (string-equal system-type "windows-nt")
       (let ((gitpath "c:/Program Files/Git/bin/git.exe"))
         (if (file-exists-p gitpath)
             (setq magit-git-executable gitpath))))))

;; web stuff
(eval-after-load 'multi-web-mode
  '(progn (setq mweb-default-major-mode 'html-mode)
          (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                            (js-mode "<script *\\(type=\"text/javascript\"\\|language=\"javascript\"\\)?[^>]*>" "</script>")
                            (css-mode "<style *\\(type=\"text/css\"\\)?[^>]*>" "</style>")))
          (global-set-key (kbd "C-c w") (lambda ()
                                          (interactive)
                                          (multi-web-mode t)))))

(global-set-key (kbd "C-c m") 'compile)

(when (require 'glsl-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
  ;; hack to stop glsl-mode from indenting layout modifiers as knr argument declarations
  (add-hook 'glsl-mode-hook
            (lambda ()
              (c-set-offset 'knr-argdecl [0]))))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun start-smex-mode ()
  (when (require 'smex nil t)
    (global-set-key (kbd "M-x") (lambda ()
                                  (interactive)
                                  (or (boundp 'smex-cache) (smex-initialize))
                                  (global-set-key (kbd "M-x") 'smex)
                                  (smex)))))

;; try to disable modula-2 mode
(setq auto-mode-alist (remove (rassoc 'modula-2-mode auto-mode-alist) auto-mode-alist))
;; remove modula mode from the auto mode list
(delete '("\\.mod\\'" . m2-mode) auto-mode-alist)

(define-derived-mode glog-mode fundamental-mode
  (setq font-lock-defaults '((("^E.*$" . font-lock-warning-face)
                              ("^W.*$" . font-lock-function-name-face)
                              ("^I.*$" . font-lock-comment-face))))
  (setq mode-name "glog"))


;; ignore non-safe file local variables
(setq enable-local-variables :safe)

;; some projectile stuff
(when (require 'projectile nil t)
  (setq projectile-enable-caching t)
  (when (string-equal system-type "windows-nt")
    (when (ignore-errors (call-process "ls"))
      (setq projectile-indexing-method 'alien)))
  (projectile-global-mode))

(eval-after-load 'cider
  '(progn (when (string-equal system-type "windows-nt")
            (setq cider-lein-command "lein.bat"))
          (when (not (boundp 'clojure--prettify-symbols-alist)) (setq clojure--prettify-symbols-alist nil))))

(when (string-equal system-type "darwin")
  (when (require 'exec-path-from-shell nil t)
    (exec-path-from-shell-initialize)))

;; no backup files
(setq make-backup-files nil)

;; to make org mode not clobber windmove keys
(setq org-replace-disputed-keys t)

(when (require 'js2-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

;; use python 3 by default
;(setq py-python-command "py -3")
;(setq python-python-command "py -3")

;; i don't want no autocomplete
(setq company-auto-complete nil)
(setq company-auto-complete-chars nil)
(setq company-idle-delay 100000000)

(eval-after-load 'elpy
  '(add-hook 'elpy-mode-hook (lambda ()
                               ;; stop elpy from messing with company mode settings
                               (set (make-local-variable 'company-idle-delay) 10000000))))

(eval-after-load 'omnisharp
  '(add-hook 'omnisharp-mode-hook (lambda ()
                                    (company-mode t)
                                    (define-key omnisharp-mode-map (kbd "C-M-i") 'company-omnisharp))))

(eval-after-load 'company
  '(global-set-key (kbd "C-c TAB") 'company-complete))

;; no suspend when not in terminal
(when (window-system)
  (global-unset-key [(control z)]))


(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; for racer to work
(set-env-from-bash-profile "RUST_SRC_PATH")

(server-start)
