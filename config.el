;;; -*- lexical-binding: t -*-

;; put something like
;; (setq download-packages t) ; or not if you don't want to download external things
;; (setq home-el-path "~/.emacs.d/el")
;; (load (concat home-el-path "/config.el"))
;; in your .emacs

;; on windows create a symlink from %userprofile%/.gitconfig to %appdata%/.gitconfig

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

;;;;;;;;;;;;;;;;;;;;;;
;; startup config
;;;;;;;;;;;;;;;;;;;;;;

;; util functions
(eval-when-compile
  (require 'subr-x))

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
(setq tab-width 4)

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

;; disable electric indent everywhere since it doesn't seem possible to disable it for python only
(electric-indent-mode -1)

(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; encoding stuff for old emacs versions
(when (or (< emacs-major-version 24) (and (= emacs-major-version 24) (< emacs-minor-version 4)))
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))

;; better eshell behaviour
(setq eshell-cmpl-cycle-completions nil)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; next/prev error shortcuts
(global-set-key (kbd "<f5>") 'previous-error)
(global-set-key (kbd "<f6>") 'next-error)

(global-set-key (kbd "C-c c c") 'comment-region)
(global-set-key (kbd "C-c c u") 'uncomment-region)

(global-set-key (kbd "C-c m") 'compile)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; try to disable modula-2 mode
(setq auto-mode-alist (remove (rassoc 'modula-2-mode auto-mode-alist) auto-mode-alist))
;; remove modula mode from the auto mode list
(delete '("\\.mod\\'" . m2-mode) auto-mode-alist)

;; ignore non-safe file local variables
(setq enable-local-variables :safe)

;; no backup files
(setq make-backup-files nil)

;; no suspend when not in terminal
(when (window-system)
  (global-unset-key [(control z)]))

(defun find-windows-git-root ()
  (if-let ((output (shell-command-to-string "reg.exe query HKEY_LOCAL_MACHINE\\Software\\GitForWindows /v InstallPath"))
           (regpos (string-match "REG_SZ" output))
           (pathstart (string-match "[^[:blank:]]" output (+ regpos (length "REG_SZ"))))
           (pathend (string-match "\n" output pathstart)))
      (replace-regexp-in-string "\\\\" "/" (substring output pathstart pathend))
    nil))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages
;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (setq download-packages (if (boundp 'download-packages) download-packages nil)))

(unless (boundp 'package-archives)
  (setq package-archives '()))

;; better package repo
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(when (and download-packages (require 'package nil t))
  (package-initialize))

;; make sure use-package is loaded
(add-to-list 'load-path (concat external-el-path "/use-package"))
(eval-when-compile
  (require 'use-package))

;; a package with some random local utils
(use-package deepness-utils
  :load-path el-path)

(use-package grep
  :config
  ;; fix grep stuff
  (when (fboundp 'grep-apply-setting)
    (grep-apply-setting 'grep-find-command '("find . -type f ! -path \"*.git*\" -exec grep -nH -e  {} +" . 51))))

(use-package paredit
  :load-path (lambda () (concat external-el-path "/paredit"))
  :commands paredit-mode
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  scheme-mode-hook)) (add-hook hook (lambda ()
                                                      (paredit-mode t)))))

(use-package undo-tree
  :load-path (lambda () (concat external-el-path "/undo-tree"))
  :config
  (global-undo-tree-mode))


;;;;;;;;;;;;;;;;;;;;
;; ensured packages

(use-package rainbow-delimiters
  :if download-packages
  :ensure t
  :commands rainbow-delimiters-mode
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  scheme-mode-hook)) (add-hook hook (lambda ()
                                                      (rainbow-delimiters-mode t))))
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

(use-package highlight-indentation
  :ensure t
  :if (and download-packages window-system)
  :commands highlight-indentation-mode
  :init
  ;; enable indentation highlighting for modes that benefit from them (python)
  (add-hook 'python-mode-hook #'highlight-indentation-mode))

(use-package yascroll
  :ensure t
  :if (and download-packages window-system)
  :config
  (scroll-bar-mode -1)
  (global-yascroll-bar-mode t)
                                        ;(set-face-background 'yascroll:thumb-text-area "Gray80")
                                        ;(set-face-background 'yascroll:thumb-fringe "Gray80")
                                        ;(set-face-foreground 'yascroll:thumb-fringe "Gray80")
  )

(use-package multi-web-mode
  :ensure t
  :if download-packages
  :commands multi-web-mode
  :config
  (setq mweb-default-major-mode 'html-mode)
  (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                    (js-mode "<script *\\(type=\"text/javascript\"\\|language=\"javascript\"\\)?[^>]*>" "</script>")
                    (css-mode "<style *\\(type=\"text/css\"\\)?[^>]*>" "</style>")))
  :bind ("C-c w" . multi-web-mode))

(use-package projectile
  :ensure t
  :if download-packages
  :config
  (setq projectile-enable-caching t)
  (when (string-equal system-type "windows-nt")
    (when (ignore-errors (call-process "ls"))
      (setq projectile-indexing-method 'alien)))
  :init
  (projectile-global-mode))

(use-package js2-mode
  :ensure t
  :if download-packages
  :mode "\\.js\\'"
  :commands js2-mode
  :config
  (setq js2-strict-missing-semi-warning nil))

(use-package magit
  :ensure t
  :if download-packages
  :commands (magit-status magit-find-file-ido)
  :config
  (when (string-equal system-type "windows-nt")
    (let ((gitpath (concat (or (find-windows-git-root) "") "/bin/git.exe")))
      (if (file-exists-p gitpath)
          (setq magit-git-executable gitpath))))
  (setq magit-auto-revert-mode nil))

(use-package exec-path-from-shell
  :ensure t
  :if (and download-packages (string-equal system-type "darwin"))
  :config
  (exec-path-from-shell-initialize))

(use-package smex
  :ensure t
  :if download-packages
  :bind (("M-x" . smex)))

(use-package rust-mode
  :ensure t
  :if download-packages
  :mode "\\.rs\\'"
  :commands rust-mode)

;; for racer to work
(use-package racer
  :ensure t
  :if download-packages
  :commands racer-mode
  :init
  (set-env-from-bash-profile "RUST_SRC_PATH")
  ;(call-process "rustc" nil nil nil "--print" "sysroot")
  ;; TODO check first if rustc returns properly
  (setenv "RUST_SRC_PATH" (concat (string-trim (shell-command-to-string "rustc --print sysroot")) "/lib/rustlib/src/rust/src"))
  (add-hook 'rust-mode-hook 'racer-mode))

(use-package elpy
  :ensure t
  :if download-packages
  :commands
  elpy-mode
  elpy-enable
  :init
  (add-hook 'python-mode-hook #'elpy-mode)
  :config
  (add-hook 'elpy-mode-hook #'(lambda ()
                                ;; stop elpy from messing with company mode settings
                                (set (make-local-variable 'company-idle-delay) 10000000))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package-dependent config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun add-to-path (paths)
  "paths: a path to add to path, or a list of paths"
  (let ((paths (if (listp paths) paths (list paths))))
    (setenv "PATH"
            (mapconcat 'identity
                       (mapcar (lambda (path) (replace-regexp-in-string "/" "\\\\" path)) (append paths (list (getenv "PATH"))))
                       ";"))))

;; windows only stuff
(when (string-equal system-type "windows-nt")
  (if-let ((gitroot (find-windows-git-root)))
      (dolist (path (mapcar 
                     (lambda (suffix) (concat gitroot suffix))
                     '("/bin" "/usr/bin")))
        (add-to-path path)
        (add-to-list 'exec-path path))

    ;; try cygwin instead

    ;; set cygwin path for w32 emac
    ;; TODO only use git bash unix utils?
    ;;(setq w32shell-cygwin-bin "C:/cygwin/bin")
    ;;(setq gnuwin-path "c:/local/gnuwin32/bin")
    (setq cygwin-root-directory (if (file-exists-p "c:/cygwin64") "c:/cygwin64" "c:/cygwin"))
    (setq cygwin-bin-path (concat cygwin-root-directory "/bin"))
    (setq cygwin-usr-bin-path (concat cygwin-root-directory "/usr/bin"))
    (add-to-path '(cygwin-bin-path cygwin-usr-bin-path))
    (add-to-list 'exec-path cygwin-bin-path)
    (add-to-list 'exec-path cygwin-usr-bin-path)
    ;; (set-variable 'find-program "find.exe")
    ;; (set-variable 'grep-program "grep.exe")
    (and (require 'cygwin-mount nil t) (require 'setup-cygwin nil t))))

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

(defun c-style-hook-function ()
  (interactive)
  (c-set-style "linux")
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'inline-open 0)
  ;;(semantic-mode t)
  (setq show-trailing-whitespace t))
(add-hook 'c-mode-common-hook 'c-style-hook-function)

(define-derived-mode glog-mode fundamental-mode
  (setq font-lock-defaults '((("^E.*$" . font-lock-warning-face)
                              ("^W.*$" . font-lock-function-name-face)
                              ("^I.*$" . font-lock-comment-face))))
  (setq mode-name "glog"))


;; to make org mode not clobber windmove keys
(setq org-replace-disputed-keys t)

;; use python 3 by default
;(setq py-python-command "py -3")
;(setq python-python-command "py -3")

;; i don't want no autocomplete
(setq company-auto-complete nil)
(setq company-auto-complete-chars nil)
(setq company-idle-delay 100000000)


(eval-after-load 'company
  '(global-set-key (kbd "C-c TAB") 'company-complete))


(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(defun hightlight-nonascii ()
  (interactive)
  (highlight-regexp "[^[:ascii:]]"))


(defun highlight-log ()
  (interactive)
  (highlight-regexp "error" 'hi-pink)
  (highlight-regexp "warning" 'hi-yellow))

(server-start)
