; ---------------------------------------------------------------------------
;; `init.el': Emacs configration file
;;
;; (c) 2016-2018 Peisen Wang
;;
(setq user-full-name "Peisen Wang")
(setq user-mail-address "wangps@mail.ustc.edu.cn")
;; ---------------------------------------------------------------------------


;;(add-to-list 'load-path (expand-file-name "scripts" user-emacs-directory))


;;; Package Management
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPkgs
  '(
    elpy
    ;;color-theme-sanityinc-tomorrow
    solarized-theme
    ;;leuven-theme
    ;;rainbow-mode
    ws-butler
    linum-off))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPkgs)


;;; Appearance
;;(set-fringe-mode '(1. 0))
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Maximize window size at start:
;;(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; color theme:
(defun org-theme () (interactive) (load-theme 'leuven t))
(defun light-theme () (interactive) (load-theme 'solarized-light t))
(defun dark-theme () (interactive) (load-theme 'solarized-dark t))
(dark-theme)

;; Emacs frame title:
(setq frame-title-format '(""))

;; Show line number:
(global-linum-mode t)
(setq linum-format "%d ")
;; Don't show line number in certain modes, like `ansi-term':
;; Adding hooks like `term-mode-hook' does not work, maybe a bug, so use
;; `linum-off' package.
(require 'linum-off)
(add-to-list 'linum-disabled-modes-list 'ansi-term)
;; Don't display line number at bottom:
(line-number-mode -1)
;; Display column number at bottom: 
(setq column-number-mode t)

;; Line limit
(setq-default
 whitespace-line-column 80
 whitespace-style       '(face lines-tail))
(add-hook 'prog-mode-hook #'whitespace-mode)

;; Increse default font size and window size on mac:
(if (eq system-type 'darwin)
    (set-default-font "Monaco 13"))

;; no startup message
(setq inhibit-startup-message t
      initial-scratch-message nil
      initial-major-mode 'eshell)

;; bref anwser to yes or no:
(defalias 'yes-or-no-p 'y-or-n-p)

;; Instant feedback completion
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; Save minibuffer history:
(savehist-mode 1)

(setq enable-recursive-minibuffers t)

;; Paste from x clipboard:
(setq x-select-enable-clipboard t)

(setq mouse-yank-at-point t)

;; Better display for files with the same name:
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Smooth scrolling:
(setq scroll-step 1)
;;(setq scroll-conservatively 5)

;; Scroll up and down commands (`C-v' and `M-v') scroll to the end:
(setq scroll-error-top-bottom 'true)

(setq apropos-do-all t)

;; Auto load changed files:
(global-auto-revert-mode t)

;; Backup and auto-save settings
;; `TODO:' Uniform the position and create if non-exist.
(setq
    backup-by-copying t ; automatic backup
    backup-directory-alist '(("." . "~/.emacs.d/saves/")) ; place to save
    delete-old-versions t 
    kept-new-versions 6)
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Save tramp at local
(setq tramp-auto-save-directory temporary-file-directory)
(setq tramp-backup-directory-alist backup-directory-alist)

;; Enable tramp mode password caching
(setq password-cache-expiry nil)


;;; Editing
;; Do not input tabs:
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

;; Show brackets:
(show-paren-mode t)
(setq show-paren-style 'parentheses)

(setq require-final-newline t)

(setq kill-ring-max 200)

;; Trim whitespaces at end of the lines.
(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)

;;; key bindings
;; Set `COMMAND' as `META' and `OPTION' as `SUPER' for mac:
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; previously binded to `delete-blank-lines'
(global-set-key (kbd "C-x C-o") 'other-window)

;; previously binded to `open-line' and `electric-newline-and-maybe-indent'
(global-set-key (kbd "C-o") (kbd "C-e C-m"))
(global-set-key (kbd "C-j") (kbd "C-p C-e C-m"))

;; previously binded to `kill-buffer'
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

;; exchange keys binded to `list-buffers' and `switch-to-buffer'
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "C-x b") 'list-buffers)

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;; Enable type return when holding control:
(global-set-key (kbd "<C-return>") (kbd "RET"))

;; Replaced with more powerful equivalents
(global-set-key (kbd "M-/") 'hippie-expand)


;;; Tools
;; Set python path
(setq elpy-rpc-python-command "python3")
(setq python-shell-interpreter "python3")
;; enable elpy:
(elpy-enable)
;; Prevent elpy from highlighting indents. 
(delete 'elpy-module-highlight-indentation elpy-modules)

;; MATLAB mode
;; Treat `.m' files as MATLAB files by default:
(setq auto-mode-alist
      (cons
       '("\\.m$" . octave-mode)
       auto-mode-alist))

;; term / ansi-term mode
;; Add key binding for starting term:
;(global-set-key (kbd "C-x C-t") 'ansi-term)
;; Pervent ansi-term from asking what shell to use every time:
(defvar term-shell-name 'shell-file-name)
(defadvice ansi-term (before force-bash)
  (interactive (list shell-file-name)))
(ad-activate 'ansi-term)
;; Close buffer after logging out from terminal:
;; This also closes eshell's visual commands.
(defadvice term-handle-exit
  (after term-kill-buffer-on-exit activate)
  (kill-buffer))
;; Paste into term
(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

;; eshell
;; Initialization  
(defun eshell-new ()
  (interactive)
  (eshell t))
(global-set-key (kbd "C-x C-t") 'eshell-new)
;; List of visual commands 
(setq additional-eshell-visual-commands
      '("python" "python3" "ipython" "ipython3" "ssh" "bash"))
(defun m-eshell-hook ()
  ;; Use C-p and C-n to switch input lines.
  (define-key eshell-mode-map [(control p)]
    'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map [(control n)]
    'eshell-next-matching-input-from-input)
  (define-key eshell-mode-map [up] 'previous-line)
  (define-key eshell-mode-map [down] 'next-line)
  ;; Default path
  (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env))
  ;; Visual commands
  (setq eshell-visual-commands
        (append eshell-visual-commands additional-eshell-visual-commands))
  ;; Enable tramp
  (add-to-list 'eshell-modules-list 'eshell-tramp)
)
(add-hook 'eshell-mode-hook 'm-eshell-hook)
;; Complete like bash
(setq eshell-cmpl-cycle-completions nil)
;; Jump to prompt on keypress
(setq eshell-scroll-to-bottom-on-input t)

;; Server addresses 
;;;(load-file (expand-file-name "servers.el" user-emacs-directory))

;; org mode
;; Key bindings:
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-log-done t)
;; Agenda files:
;;(setq org-agenda-files (list "~/Dropbox/notes/lab.org"
;;                             "~/Dropbox/notes/others.org"
;;                             "~/Dropbox/notes/life.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default)))
 '(package-selected-packages
   (quote
    (material-theme ws-butler gnu-elpa-keyring-update solarized-theme linum-off elpy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
