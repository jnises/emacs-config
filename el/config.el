;;; -*- lexical-binding: t -*-

;; TODO with frozen straight.el deps we don't need the download-packages option?
;; TODO use straight-vc-git-default-clone-depth ?

;; load early init in case it wasn't
(unless (boundp 'deepness-early-inited)
  (load (concat (file-name-directory load-file-name) ".." "early-init")
        nil t))

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

(eval-and-compile
  (setq download-packages (if (boundp 'download-packages) download-packages nil)))

(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; util functions
(eval-when-compile
  (require 'subr-x))

(setq inhibit-splash-screen t
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      ;; Resizing the Emacs frame can be a terribly expensive part of changing the
      ;; font. By inhibiting this, we halve startup times, particularly when we use
      ;; fonts that are larger than the system default (which would resize the frame).
      frame-inhibit-implied-resize t)

;; disable the toolbar
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))

;; no menu on windows
(if (and (fboundp 'menu-bar-mode) IS-WINDOWS)
    (menu-bar-mode 0))

;; show matching parens
(show-paren-mode t)

;; enable syntax highlighting
(global-font-lock-mode t)

;; show marked region
(transient-mark-mode t)

;; no tabs!!!
(setq-default indent-tabs-mode nil
              ;; but if there is, set the default tab width (determines how a tab is displayed)
              tab-width 4)

;; change color theme (used if don't want to download the theme)
;; (if (and (window-system) (not (and download-packages (require 'doom-themes nil 'noerror))))
;;     (load-theme 'tsdh-dark))

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

;; enable improved window switching, disable if you don't want it to clobber
;; shift-<arrowkeys> for selection
(windmove-default-keybindings)

;; nicer buffer names
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'forward))

;; disable electric indent everywhere since it doesn't seem possible to disable it for python only
;;(electric-indent-mode -1)

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

;; mostly editing c++ code these days
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; ignore non-safe file local variables
(setq enable-local-variables :safe)

;; no backup files
(setq create-lockfiles nil
      make-backup-files nil)

;; no suspend when not in terminal
(when (window-system)
  (global-unset-key [(control z)]))

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq compilation-scroll-output t)

;; shell-command-to-string doesn't seem to work in some versions of windows 10
(defun better-shell-command-to-string (command &rest args)
  (with-output-to-string
    (with-current-buffer standard-output
      (apply 'call-process command nil t nil args))))

(defun find-windows-git-root ()
  (if-let ((output (better-shell-command-to-string "reg.exe" "query" "HKEY_LOCAL_MACHINE\\Software\\GitForWindows" "-v" "InstallPath"))
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
    ;; (setq cygwin-root-directory (if (file-exists-p "c:/cygwin64") "c:/cygwin64" "c:/cygwin"))
    ;; (setq cygwin-bin-path (concat cygwin-root-directory "/bin"))
    ;; (setq cygwin-usr-bin-path (concat cygwin-root-directory "/usr/bin"))
    ;; (add-to-path (list cygwin-bin-path cygwin-usr-bin-path))
    ;; (add-to-list 'exec-path cygwin-bin-path)
    ;; (add-to-list 'exec-path cygwin-usr-bin-path)
    ;; (set-variable 'find-program "find.exe")
    ;; (set-variable 'grep-program "grep.exe")
    ;; (and (require 'cygwin-mount nil t) (require 'setup-cygwin nil t))
    )

  ;; msys
  (let ((msysroot "c:/msys64"))
    (when (file-exists-p msysroot)
      (let ((msysusrbin (concat msysroot "/usr/bin")))
        (add-to-path msysusrbin)
        (add-to-list 'exec-path msysusrbin)))))

;; python stuff
(add-hook 'python-mode-hook (lambda ()
                              ;; tab width is a mess, so force python to use the correct one
                              (setq tab-width 4)
                              (setq python-indent 4)
                                        ;(semantic-mode 1)
                              (let ((pycommand (if (string-equal system-type "windows-nt")
                                                   "py -3"
                                                 "python3")))
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

;; google glog file mode
(define-derived-mode glog-mode fundamental-mode
  (setq font-lock-defaults '((("^E.*$" . font-lock-warning-face)
                              ("^W.*$" . font-lock-function-name-face)
                              ("^I.*$" . font-lock-comment-face))))
  (setq mode-name "glog"))


;; to make org mode not clobber windmove keys
(setq org-replace-disputed-keys t)

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


;; prefer horizontal splits
(with-eval-after-load "window"
  (fmakunbound #'split-window-sensibly)
  (defun split-window-sensibly (&optional window)
	"Have a look at window.el for documentation"
	(let ((window (or window (selected-window))))
      (or
	   (and (window-splittable-p window t)
			;; Split window horizontally.
			(with-selected-window window
			  (split-window-right)))
	   ;;;; in fact don't split vertically at all
	   ;; (and (window-splittable-p window)
	   ;; 		;; Split window vertically.
	   ;; 		(with-selected-window window
	   ;; 		  (split-window-below)))
	   (and
        ;; If WINDOW is the only usable window on its frame (it is
        ;; the only one or, not being the only one, all the other
        ;; ones are dedicated) and is not the minibuffer window, try
        ;; to split it vertically disregarding the value of
        ;; `split-height-threshold'.
        (let ((frame (window-frame window)))
          (or
           (eq window (frame-root-window frame))
           (catch 'done
             (walk-window-tree (lambda (w)
                                 (unless (or (eq w window)
                                             (window-dedicated-p w))
                                   (throw 'done nil)))
                               frame)
             t)))
		(not (window-minibuffer-p window))
		(let ((split-height-threshold 0))
		  (when (window-splittable-p window)
			(with-selected-window window
			  (split-window-below)))))))))

(when (string-equal system-type "darwin")
  ;; no beeps!
  (setq ring-bell-function 'ignore)
  ;; macos ls doesn't support --dired
  (setq dired-use-ls-dired nil))

;; from doom

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(when (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-pipe-read-delay 0              ; faster IPC
        w32-pipe-buffer-size (* 64 1024))  ; read more at a time (was 4K)

  ;; The clipboard on Windows could be in another encoding (likely utf-16), so
  ;; let Emacs/the OS decide what to use there.
  (setq selection-coding-system 'utf-8))

;; Emacs is essentially one huge security vulnerability, what with all the
;; dependencies it pulls in from all corners of the globe. Let's try to be at
;; least a little more discerning.
(setq gnutls-verify-error (and (fboundp 'gnutls-available-p)
                               (gnutls-available-p)
                               (not (getenv-internal "INSECURE")))
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (not IS-WINDOWS)
                         (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2"))
      ;; `gnutls-min-prime-bits' is set based on recommendations from
      ;; https://www.keylength.com/en/4/
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
      ;; Emacs is built with `gnutls' by default, so `tls-program' would not be
      ;; used in that case. Otherwise, people have reasons to not go with
      ;; `gnutls', we use `openssl' instead. For more details, see
      ;; https://redd.it/8sykl1
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    ;; compatibility fallbacks
                    "gnutls-cli -p %p %h"))

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)


;; TODO this should use the current region if present
(defun strip-ansi-color-in-buffer ()
  "remove any ansi escape codes in buffer"
  (interactive)
  (save-excursion
   (mark-whole-buffer)
   (ansi-color-filter-region (region-beginning) (region-end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO get this using submodule instead?
(when download-packages
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;; make sure use-package is loaded
(eval-when-compile
  (add-to-list 'load-path (concat external-el-path "/use-package"))
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


;;;;;;;;;;;;;;;;;;;;
;; downloaded packages

(when download-packages
  (when (window-system)
    (use-package doom-themes
	  :straight t
	  :config
	  (load-theme 'doom-dark+ t)))

  (use-package undo-tree
    :straight t
    :config
    ; don't make ~undo-tree~ droppings on disk
    (setq undo-tree-auto-save-history nil)
    (global-undo-tree-mode))
  
  (use-package rainbow-delimiters
    :straight t
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
                             (apply 'format "#%02x%02x%02x" (mapcar (lambda (x) (floor (* x 255))) (labhsl-to-rgb (* (/ shuffledn 9.0) pi 2) 60 60)))))))

  (use-package highlight-indentation
    :straight t
    :if (and download-packages window-system)
    :commands highlight-indentation-mode
    :init
    ;; enable indentation highlighting for modes that benefit from them (python)
    (add-hook 'python-mode-hook #'highlight-indentation-mode))

  (use-package yascroll
    :straight t
    :if (and download-packages window-system)
    :config
    (scroll-bar-mode -1)
    (global-yascroll-bar-mode t)
                                        ;(set-face-background 'yascroll:thumb-text-area "Gray80")
                                        ;(set-face-background 'yascroll:thumb-fringe "Gray80")
                                        ;(set-face-foreground 'yascroll:thumb-fringe "Gray80")
    )

  (use-package projectile
    :straight t
    :config
    (setq projectile-enable-caching t)
    ;; (when (string-equal system-type "windows-nt")
    ;;   (when (ignore-errors (call-process "ls"))
    ;;     (setq projectile-indexing-method 'alien)))
    :init
    (projectile-global-mode)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

  (use-package js2-mode
    :straight t
    :mode "\\.js\\'"
    :commands js2-mode
    :config
    (setq js2-strict-missing-semi-warning nil))

  (use-package magit
    :straight t
    :commands (magit-status magit-find-file-ido)
    :config
    (when (string-equal system-type "windows-nt")
      (let ((gitpath (concat (or (find-windows-git-root) "") "/bin/git.exe")))
        (if (file-exists-p gitpath)
            (setq magit-git-executable gitpath))))
    (setq magit-auto-revert-mode nil))

  (use-package exec-path-from-shell
    :straight t
    :if (and download-packages (string-equal system-type "darwin"))
    :config
    (exec-path-from-shell-initialize))

  (use-package smex
    :straight t
    :bind (("M-x" . smex)))

  ;; rustic is an extension to rust-mode
  (use-package rustic
    :straight t
    :config
    (defun set-rustic-build-directory (dir)
      (interactive "D")
      (setq rustic-compile-directory-method (lambda () dir)))
    (defun reset-rustic-build-directory ()
      (interactive)
      (setq rustic-compile-directory-method rustic-buffer-crate)))

  ;; language server support
  (use-package lsp-mode
    :straight t
    :commands lsp
    :hook ((c-mode c++-mode rust-mode) . 'lsp)
    :init
    (setq lsp-keymap-prefix "C-c l")
    (setq lsp-enable-snippet nil)
    (setq lsp-eldoc-enable-hover nil)
    (setq lsp-eldoc-enable-signature-help nil)
    (setq lsp-eldoc-prefer-signature-help nil)
    (setq lsp-signature-render-all nil)
    (setq lsp-enable-symbol-highlighting nil)
    (setq lsp-headerline-breadcrumb-enable nil)
    ;; don't show docs in minibuffer
    (setq lsp-signature-auto-activate nil)
    ;; no flycheck noise
    ;; (setq lsp-diagnostics-provider :none)
    (when (string-equal system-type "windows-nt")
	  (setq lsp-pyls-server-command "py -3 -m pyls")))

  (use-package dap-mode
    :straight t
    :config
    (dap-ui-mode)
    (dap-ui-controls-mode 1)

    (require 'dap-lldb)
    (require 'dap-gdb-lldb)
    ;; installs .extension/vscode
    (dap-gdb-lldb-setup)
    (dap-register-debug-template
     "Rust::LLDB Run Configuration"
     (list :type "lldb"
           :request "launch"
           :name "LLDB::Run"
	       :gdbpath "rust-lldb"
           :target nil
           :cwd nil)))
  
  (use-package editorconfig
    :straight t
    :config
    (editorconfig-mode 1))

  (use-package company
    :straight t
    :init
    ;; i don't want no autocomplete
    (setq company-auto-complete nil)
    (setq company-auto-complete-chars nil)
    (setq company-idle-delay 100000000)
    :config
    (global-set-key (kbd "C-c TAB") 'company-complete))

  (use-package flycheck
    :straight t)
  (use-package rg
    :straight t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package-dependent config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;(server-start)

