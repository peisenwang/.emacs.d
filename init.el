;;; Emacs configuration
;; Copyright 2016 - 2020 Peisen Wang

;; User info

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq user-full-name "Peisen Wang"
      user-mail-address "wpeisen@gmail.com")


;;;; Startup and gui
;;   ===============
(setq inhibit-startup-message t
      initial-scratch-message nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)


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
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-saves") t)))


;;;; General settings
;;   ================
;; No type full yes
;;(defalias 'yes-or-no-p 'y-or-n-p)

;; Fast show prefix keys in echo area
(setq echo-keystrokes 0.1)

;; recentf
(recentf-mode t)


;;;; Editing
;;   =======
;; Tab
(setq tab-width 4
      indent-tabs-model nil)

;; Show parentheses
(show-paren-mode t)

;; auto parenthese pair
(add-hook 'prog-mode-hook #'electric-pair-mode)

;; Show column number
(setq column-number-mode t)


;;;; Key-bindings
;;   ============
;; Use "C-x C-o" instead of "C-x o" to switch windows
(global-set-key (kbd "C-x C-o") 'other-window)

;; Switch "C-x b" and "C-x C-b" for buffer switching
(global-set-key (kbd "C-x b") 'list-buffers)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

;; Kill current buffer with "C-x C-k"
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

;; Create nextline for "C-o" (originally `open-line')
(global-set-key (kbd "C-o") (kbd "C-e C-M"))
;; Create a previous line for "C-j" (originally `eval-print-last-sexp')
(global-set-key (kbd "C-j") (kbd "C-a C-M C-p"))

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


;;;; Packages
;;   ========
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(defvar custom-installed-packages
  '(use-package
     ws-butler
     linum-off) "A list of packages.")

(require 'cl-lib)


;; Auto delete trailing whitespaces.
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))


;;;; Customize
;;   =========
(setq custom-file (concat user-emacs-directory "custom.el"))
(load-file custom-file)
