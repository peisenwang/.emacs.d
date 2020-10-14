;;; Emacs configuration
;;  ===================
;; Copyright 2016 - 2020 Peisen Wang

;; User info
(setq user-full-name "Peisen Wang"
      user-mail-address "wpeisen@gmail.com")


;;;; Startup and gui
;;   ===============
(setq inhibit-startup-message t
      initial-scratch-message nil)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
;;(fringe-mode 0)

;; Set title
(setq frame-title-format '("%f"))

;; Expand to full height
(setq default-frame-alist '((fullscreen . fullheight)))


;;;; Packages general setting
;;   ========================
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(defvar package-list
  '(use-package
     ws-butler
     linum-off
     solarized-theme
     monokai-theme
     elpy
     ) "A list of packages.")

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;;;; Theme
;;   =====
(setq monokai-background "#222E32")
(load-theme 'monokai t)


;;;; Backup and autosave
;;   ===================
;; Backup ([FILE]~) settings
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Auto save (#[FILE]#) settings
(defvar auto-save-file-directory
  (concat temporary-file-directory "emacs-auto-saves"))
(make-directory auto-save-file-directory t)
(setq auto-save-file-name-transforms
      `((".*" ,auto-save-file-directory t)))

;; Tramp setting
(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-auto-save-directory auto-save-file-directory)

;; Save history for M-x etc.
(savehist-mode t)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))


;;;; General settings
;;   ================
;; Auto load changed files
(global-auto-revert-mode t)

;; recentf
(recentf-mode t)

;; No type full yes
(defalias 'yes-or-no-p 'y-or-n-p)

;; Fast show prefix keys in echo area
(setq echo-keystrokes 0.1)

;; Scrolling
(setq scroll-margin 3
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      ;; C-v and M-v scroll to the end
      scroll-error-top-bottom t)

;; environment path
(setenv "PATH" (concat "~/.local/bin" (getenv "PATH")))


;;;; Editing
;;   =======
;; Tab
(setq tab-width 4
      indent-tabs-model nil)

;; Show parentheses
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; auto parenthese pair
(add-hook 'prog-mode-hook #'electric-pair-mode)

;; Larger kill ring size
(setq kill-ring-max 500)

;; EOF newline
(setq require-final-newline t)

;; Show column number
(setq column-number-mode t)

;; Do not show line number at bottom
(setq line-number-mode nil)

;; Better display for files with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;;;; Key-bindings
;;   ============
;; Use "C-x C-o" instead of "C-x o" to switch windows
;; (originally `delete-blank-lines')
(global-set-key (kbd "C-x C-o") 'other-window)

;; Switch "C-x b" and "C-x C-b" for buffer switching
(global-set-key (kbd "C-x b") 'list-buffers)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

;; Kill current buffer with "C-x C-k" (originally `kill-buffer')
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

;; Set key-binding for `replace-string'
(global-set-key (kbd "C-x s") 'replace-string)

;; Create nextline for "C-o" (originally `open-line')
(global-set-key (kbd "C-o") (kbd "C-e C-M"))
;; Create a previous line for "C-j" (originally `eval-print-last-sexp')
(global-set-key (kbd "C-j") (kbd "C-a C-M C-p"))

;; Enable type return when holding control
(global-set-key (kbd "<C-return>") (kbd "RET"))

;; Comment region
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;; Use "C-q" to switch window like what I do for tmux (originally `quoted-insert')
(global-unset-key (kbd "C-q"))
(global-set-key (kbd "C-q C-p") 'windmove-up)
(global-set-key (kbd "C-q C-n") 'windmove-down)
(global-set-key (kbd "C-q C-f") 'windmove-right)
(global-set-key (kbd "C-q C-b") 'windmove-left)
(global-set-key (kbd "C-q C-o") 'other-window)
(global-set-key (kbd "C-q -") 'split-window-below)
(global-set-key (kbd "C-q |") 'split-window-right)

;; Easy toggle text size
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Use command as meta and option as super for mac
(setq mac-option-modifier 'super
      mac-command-modifier 'meta)


;;;; Specific packages setting
;;   =========================
;; ido
(use-package ido
  :config
  (setq ido-everywhere t
	ido-enable-flex-matching t
	ido-use-filename-at-point 'guess)
  (ido-mode t))

;; Auto delete trailing whitespaces.
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;; Set line numbers with certain modes as exceptions
(use-package linum-off
  :init
  (global-linum-mode t)
  :config
  (add-to-list 'linum-disabled-modes-list 'ansi-term))

;; elpy for python
(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
  (setq elpy-rpc-python-command "python3"))

;; Visual indication for replace-regexp
(use-package visual-regexp
  :bind
  ("C-x M-s" . vr/replace))


;;;; Customize
;;   =========
(setq custom-file (concat user-emacs-directory "custom.el"))
(load-file custom-file)
