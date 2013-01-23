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


;; better package repo
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defun install-default-packages ()
  " install useful packages, call this method when starting emacs on a new machine "
  (interactive)
  (package-refresh-contents)
  (mapc (lambda (package)
          (if (not (package-installed-p package)) 
              (package-install package)))
        '(rainbow-delimiters
          clojure-mode
          php-mode
          lua-mode
          highlight-indentation
          yascroll
          undo-tree
          paredit
          multi-web-mode)))

(unless (boundp 'el-path)
  (setq el-path (concat (getenv "HOME") "/.emacs.d/el")))
(setq external-el-path (concat el-path "/external"))

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

(add-to-list 'load-path el-path)
(add-to-list 'load-path external-el-path)

;; fix grep stuff
(require 'grep)
(grep-apply-setting 'grep-find-command '("find . -type f ! -path \"*.git*\" -exec grep -nH -e  {} +" . 51))

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
  ;; emacs 24
  (load-theme 'tango-dark)
  ;; ugly hack to make ediff behave
  (when (string-equal system-type "windows-nt")
    (load "c:/local/emacs/etc/themes/tango-dark-theme.el" t)))

;; highlight line
(global-hl-line-mode 1)
(hl-line-mode 1)

;; nicer ediff
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; display column
(column-number-mode t)

;; enable ido mode
(ido-mode t)
(setq ido-enable-flex-matching t)

;; font stuff
(when window-system 
  (defun set-face-font-if-it-exists (target fontname)
    (when (x-list-fonts fontname)
      (set-face-font target fontname)))
  (set-face-font-if-it-exists 'default "-unknown-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"))

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
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'forward))

;; load cedet
(when (require 'cedet nil t)
  (global-ede-mode t)
  (require 'semantic/sb)
  (semantic-mode t)
  (setq semantic-idle-work-update-headers-flag t)
  (global-semantic-mru-bookmark-mode 1)
  (global-semanticdb-minor-mode t)

  ;; use shorter symref tag listing
  (setq semantic-symref-results-summary-function 'semantic-format-tag-canonical-name)

  ;; enable sticky function globaly
  (global-semantic-stickyfunc-mode 1)

  ;; enable goto symbol
  (add-to-list 'load-path (concat el-path "/ido-goto-buffer-tag/"))
  (when (require 'ido-goto-buffer-tag nil t) (global-set-key (kbd "C-c i") 'ido-goto-buffer-tag))

  ;; set up some ede projects
  ;; (flet ((add-proj (dir &optional name)
  ;;                  (when (not (file-exists-p dir))
  ;;                    (make-directory dir))
  ;;                  (when (null name)
  ;;                    (setq name (substring dir (+ 1 (string-match "/[^/]+$" dir)))))
  ;;                  (let ((filething (concat dir "/." (user-login-name) "_ede_anchor")))
  ;;                    (shell-command (concat "touch " filething))
  ;;                    (ede-cpp-root-project name :file filething))))
  ;;   ;; nothing is done here for now
  ;;   )
  )

;; windows only stuff
(when (string-equal system-type "windows-nt")
  ;; set cygwin path for w32 emac
  ;;(setq w32shell-cygwin-bin "C:/cygwin/bin")
  ;;(setq gnuwin-path "c:/local/gnuwin32/bin")
  (setq cygwin-bin-path "c:/cygwin/bin")
  (setenv "PATH"
          (mapconcat 'identity
                     (list ;;gnuwin-path
                           ;;w32shell-cygwin-bin
                           cygwin-bin-path
                           "C:/Program Files (x86)/Git/bin"
                           "c:/program files (x86)/putty"
                           "C:/Program Files/Java/jre6/bin"
                           (getenv "PATH"))
                     ";"))

  (add-to-list 'exec-path cygwin-bin-path)
  (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin")
  (add-to-list 'exec-path "C:/Program Files (x86)/putty")
  ;; (set-variable 'find-program "find.exe")
  ;; (set-variable 'grep-program "grep.exe")
  (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
  (setq ispell-program-name "aspell")
  ;;(setq ange-ftp-ftp-program-name "c:/windows/system32/ftp.exe")
  ;;(setq tramp-default-method "ftp")
  (and (require 'cygwin-mount nil t) (require 'setup-cygwin nil t))
)

;; enable indentation highlighting for modes that benefit from them (python)
(when (require 'highlight-indentation nil t)
  (add-hook 'python-mode-hook (lambda ()
                                (highlight-indentation-mode t))))

;; python stuff
(add-hook 'python-mode-hook (lambda ()
                              ;; tab width is a mess, so force python to use the correct one
                              (setq tab-width 4)
                              (setq python-indent 4)
                              (semantic-mode 1)))


(setq gud-pdb-command-name "python -i -m pdb")

(set-default-coding-systems 'utf-8)

(require 'git nil t)

;; to shut svn up
(setenv "LC_ALL" "C")

;; better eshell behaviour
(setq eshell-cmpl-cycle-completions nil)

(put 'narrow-to-region 'disabled nil)

(defun c-style-hook-function ()
  (interactive)
  (c-set-style "linux")
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'inline-open 0))
(add-hook 'c-mode-common-hook 'c-style-hook-function)
(add-hook 'c-mode-common-hook (lambda () (semantic-mode t)))
;;(add-hook 'c++-mode-hook 'c-style-hook-function)

(load (concat el-path "/fulpdb.el"))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(load (concat el-path "/utils"))

(when (require 'rainbow-delimiters nil t)
  (require 'color)
  (dolist (hook
           '(clojure-mode-hook))
    (add-hook hook (lambda () (rainbow-delimiters-mode t))))
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
                           (apply 'format "#%02x%02x%02x" (mapcar (lambda (x) (floor (* x 255))) (labhsl-to-rgb (* (/ shuffledn 9.0) pi 2) 130 80))))))
  )

;; use rainbow delimiters and paredit mode for some lisp files
(dolist (hook '(clojure-mode-hook emacs-lisp-mode-hook)) (add-hook hook (lambda ()
                                                                          (when (require 'rainbow-delimiters nil t)
                                                                            (rainbow-delimiters-mode t))
                                                                          (when (require 'paredit nil t)
                                                                            (paredit-mode t)))))

;; some uniview stuff for work
(when (equal (system-name) "OVERTOWN")
  (require 'uniview-connection)
  (require 'uniview-tools))

;; next/prev error shortcuts
(global-set-key (kbd "<f5>") 'previous-error)
(global-set-key (kbd "<f6>") 'next-error)

(global-set-key (kbd "C-c c c") 'comment-region)
(global-set-key (kbd "C-c c u") 'uncomment-region)

(when (string-equal system-type "windows-nt")
  (when (require 'pyflakes nil t)
    (set-variable 'pyflakes-command "python c:/python27/scripts/pyflakes")))
  
(when (require 'yascroll nil t)
  (scroll-bar-mode -1)
  (global-yascroll-bar-mode t)
  (set-face-background 'yascroll:thumb-text-area "Gray80")
  (set-face-background 'yascroll:thumb-fringe "Gray80")
  (set-face-foreground 'yascroll:thumb-fringe "Gray80"))

(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

(defun load-overtone-stuff ()
  (interactive)
  (global-set-key (kbd "C-c o s") (lambda () (interactive) (nrepl-send-string "(stop)" (lambda (ignored))))))
  
;; change magit diff colors
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

;; web stuff
(when (require 'multi-web-mode nil t)
  (setq mweb-default-major-mode 'html-mode)
  (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                      (js-mode "<script *\\(type=\"text/javascript\"\\|language=\"javascript\"\\)?[^>]*>" "</script>")
                      (css-mode "<style *\\(type=\"text/css\"\\)?[^>]*>" "</style>")))
  (global-set-key (kbd "C-c w") (lambda ()
                                  (interactive)
                                  (multi-web-mode t))))

(global-set-key (kbd "C-c m") 'compile)

(add-to-list 'load-path (concat external-el-path "/glsl-mode"))
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq ange-ftp-ftp-program-name "c:/local/bin/ftp.exe")

(server-start)
