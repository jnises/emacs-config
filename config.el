;; put something like
;; (setq home-el-path "~/.emacs.d/el")
;; (load (concat home-el-path "/config.el"))
;; in your .emacs


(setq el-path (concat (getenv "HOME") "/.emacs.d/el"))
(setq external-el-path (concat el-path "/external"))

(add-to-list 'load-path external-el-path)

(defun install-default-packages ()
  " install useful packages, call this method when starting emacs on a new machine "
  (interactive)
  (package-refresh-contents)
  (mapc (lambda (package) (package-install package))
        '(rainbow-delimiters
          clojure-mode
          slime
          slime-repl
          swank-clojure
          php-mode)))


;; faster startup
(modify-frame-parameters nil '((wait-for-wm . nil)))

(setq inhibit-splash-screen t)

;; disable the toolbar
(tool-bar-mode 0)

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

;; change color theme
(if (< emacs-major-version 24)
    (when (and window-system (load (concat external-el-path "/color-theme")))
      (color-theme-charcoal-black)
      ;; highlight current line
      (set-face-background 'highlight "Gray12")
      (set-face-foreground 'highlight nil)
      ;; force color
      (add-hook 'hl-line-mode-hook (lambda () (set-face-foreground 'highlight nil)))
      (add-hook 'global-hl-line-mode-hook (lambda () (set-face-foreground 'highlight nil)))
      (global-hl-line-mode 1)
      (hl-line-mode 1)
      (set-face-background 'menu "Black"))
  (load-theme 'tango-dark))

;; tag updating helpers
;; (when (file-exists-p "/vobs/stn/dev/linux")
;;   (when (load (concat el-path "/patchwork-tags"))
;;     (patchwork-tags-set-keybindings)
;;     (patchwork-tags-protected-stn-update)))


;; nicer ediff
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; compile keybind
(global-set-key (kbd "C-c m") 'compile)

;; use nicer indentation style for c-like languages
(setq c-default-style "linux"
      c-basic-offset 4)

;; display column
(column-number-mode t)

(load (concat external-el-path "/php-mode.el"))

;; enable ido mode
(ido-mode t)
(setq ido-enable-flex-matching t)

;; font stuff
(when window-system 
  (defun set-face-font-if-it-exists (target fontname)
    (when (x-list-fonts fontname)
      (set-face-font target fontname)))
  (set-face-font-if-it-exists 'default "-unknown-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"))

;; keybindings to go to the previous/next error in compilation/grep
(global-set-key (kbd "C-'") 'next-error)
(global-set-key (kbd "C-ä") 'previous-error)

;; enable improved window switching, disable if you don't want it to clobber
;; shift-<arrowkeys> for selection
(windmove-default-keybindings)

;; breadcrumb bookmarks, (as in visual studio)
(when (load (concat external-el-path "/breadcrumb"))
  (global-set-key [(control f2)]          'bc-set)
  (global-set-key [(f2)]                  'bc-previous)
  (global-set-key [(shift f2)]            'bc-next)
  (global-set-key [(meta f2)]             'bc-list))

;; nicer buffer names
(when (load "uniquify")
  (setq uniquify-buffer-name-style 'forward))

;; load cedet
;;(when (load (concat external-el-path "/cedet/common/cedet.el") t)
(when (load "cedet")
  (global-ede-mode t)
  (require 'semantic/sb)
  (semantic-mode t)
  (setq semantic-idle-work-update-headers-flag t)
  (global-semantic-mru-bookmark-mode 1)
  (global-semanticdb-minor-mode t)

  
  ;;(require 'semantic-ia)
  ;;(require 'semantic-gcc)

  ;; use shorter symref tag listing
  (setq semantic-symref-results-summary-function 'semantic-format-tag-canonical-name)

  ;; some useful keybindings for semantic
  (global-set-key (kbd "M-ö") 'semantic-ia-fast-jump)
  (global-set-key (kbd "C-ö") 'semantic-ia-complete-tip)

  ;; enable sticky function globaly
  (global-semantic-stickyfunc-mode 1)

  ;; enable goto symbol
  (add-to-list 'load-path (concat el-path "/ido-goto-buffer-tag/"))
  (when (require 'ido-goto-buffer-tag nil t) (global-set-key (kbd "C-c i") 'ido-goto-buffer-tag))

;;  (when (load (concat el-path "/goto-symbol") t t)
;;    (global-set-key (kbd "C-c i") 'ido-goto-buffer-tag))

  
  ;; (local-set-key (kbd "<C-return>") 'semantic-ia-complete-symbol)
  ;; (local-set-key (kbd "C-c ?") 'semantic-ia-complete-symbol-menu)
  ;; (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  ;; (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))

  ;; set up some ede projects
  (when (file-exists-p "/vobs/stn/dev/linux")
    (flet ((add-proj (dir &optional name)
                     (when (not (file-exists-p dir))
                       (make-directory dir))
                     (when (null name)
                       (setq name (substring dir (+ 1 (string-match "/[^/]+$" dir)))))
                     (let ((filething (concat dir "/." (user-login-name) "_ede_anchor")))
                       (shell-command (concat "touch " filething))
                       (ede-cpp-root-project name :file filething))))
      ;; nothing is done here for now
      ))
  )

;; set cygwin path for w32 emac
(setq w32shell-cygwin-bin "C:/cygwin/bin")

(setenv "PATH"
        (concat
         "c:/gnuwin32/bin;"
         ";c:/cygwin/bin"
         ";c:/cygwin/usr/bin"
         ";C:/Program Files (x86)/Git/bin"
         ";c:/program files (x86)/putty"
         ";"(getenv "PATH")))

(add-to-list 'exec-path "C:/Program Files (x86)/Git/bin")
(add-to-list 'exec-path "C:/Program Files (x86)/putty")

;; TODO add smex


;; load ropemacs when in python mode
(eval-after-load "pymacs" '(progn
                             (pymacs-load "ropemacs" "rope-")
                             (ropemacs-mode)))

(add-hook 'python-mode-hook (lambda ()
                              (require 'pymacs)))

(setq gud-pdb-command-name "python -i -m pdb")

(eval-after-load "package"
  '(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")))

(when (require 'rainbow-delimiters nil t)
  (global-rainbow-delimiters-mode))

(setq tramp-default-method "plink")

;; remove -ssh arg from plink command line
(require 'tramp)
(let ((args (assoc 'tramp-login-args (assoc "plink" tramp-methods))))
  (setcdr args (list (remove '("-ssh") (cadr args)))))

(set-default-coding-systems 'utf-8)

