;;; Emacs configuration
;;  ===================
;; Copyright 2016 - 2022 Peisen Wang
;;
;; Use "grep -E '^;{3,}' init.el" to show the outline of this file.

;; User info
(setq user-full-name "Peisen Wang"
      user-mail-address "wpeisen@gmail.com")


;;;; Startup and gui
;;   ===============
;; This part is moved to 'early-init.el' to speed up loading.

;; Split window after gui init
(split-window-horizontally)


;;;; Packages general setting
;;   ========================
(package-initialize)
(require 'package)
(setq package-archives
      '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; install the missing packages
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))


;;;; Theme
;;   =====
(defvar cus-bg-color "#222E32")

;; (use-package monokai-theme
;;   :config
;;   (setq monokai-background cus-bg-color)
;;   (load-theme 'monokai t))

(use-package modus-themes
  :config
  (add-to-list 'modus-themes-vivendi-colors `(bg-main . ,cus-bg-color))
  (load-theme 'modus-vivendi t))

;; Make line warp arrows darker
(defface fringe-dark
  '((t (:foreground "#525252")))
  "Face for fringe bitmaps."
  :group 'basic-faces)

(set-fringe-bitmap-face 'left-curly-arrow 'fringe-dark)
(set-fringe-bitmap-face 'right-curly-arrow 'fringe-dark)

;; Extra theme for tramp to show address in mode line, this "shall be loaded
;; always as the last custom theme, because it inherits existing settings."
(use-package tramp-theme
  :config
  (load-theme 'tramp t))


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

;; Tramp backup settings
;; (add-to-list 'backup-directory-alist
;;              (cons tramp-file-name-regexp nil))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-auto-save-directory auto-save-file-directory)

;; Save history for M-x etc
(savehist-mode t)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

;; Do not create lock files
(setq create-lockfiles nil)

;; recentf
(defun recentf-open-files-without-indent (&optional files buffer-name)
  "Similar to `recentf-open-files' without indent before items or insturctions"
  (interactive)
  (unless (or files recentf-list)
    (error "There is no recent file to open"))
  (recentf-dialog (or buffer-name (format "*%s*" recentf-menu-title))
    ;; Use a L&F that looks like the recentf menu.
    (tree-widget-set-theme "folder")
    (apply 'widget-create
           `(group
             :format "\n%v\n"
             ,@(recentf-open-files-items (or files recentf-list))))
    (recentf-dialog-goto-first 'link)))

(use-package recentf
  :hook (after-init . recentf-mode)
  ;; :init
  ;; Recentf for some reason will clean opened tramp files when the system is
  ;; offline. Disable auto cleanup temporarily to prevent that.
  ;; commented as "Setting this variable directly does not take effect"
  ;; (setq recentf-auto-cleanup 'never)
  :config
  (setq recentf-max-saved-items 50
	recentf-max-menu-items 50)
  ;; Don't show leading numbers as I never use them
  (setq recentf-show-file-shortcuts-flag nil)
  (add-to-list
   'recentf-exclude (format "%s/\\.emacs\\.d/elpa/.*" (getenv "HOME")))
  (setq recentf-keep '(recentf-keep-default-predicate file-remote-p))
  ;; From tramp user manual
  (remove-hook
   'tramp-cleanup-connection-hook
   #'tramp-recentf-cleanup)
  (remove-hook
   'tramp-cleanup-all-connections-hook
   #'tramp-recentf-cleanup-all)
  :bind
  ;; Previously binded to `set-fill-column'
  ("C-x f" . recentf-open-files-without-indent))


;;;; General settings
;;   ================
;; Use command as meta and option as super for mac
(setq mac-option-modifier 'super
      mac-command-modifier 'meta)


;;;; Window & buffer management
;;   ==========================
;; Use "C-x C-o" instead of "C-x o" to switch windows
;; (originally `delete-blank-lines')
(global-set-key (kbd "C-x C-o") 'other-window)

;; Use "C-q" to switch window like what I do in tmux (originally `quoted-insert')
(global-unset-key (kbd "C-q"))
(global-set-key (kbd "C-q C-p") 'windmove-up)
(global-set-key (kbd "C-q C-n") 'windmove-down)
(global-set-key (kbd "C-q C-f") 'windmove-right)
(global-set-key (kbd "C-q C-b") 'windmove-left)
(global-set-key (kbd "C-q C-o") 'other-window)
(global-set-key (kbd "C-q -") 'split-window-below)
(global-set-key (kbd "C-q |") 'split-window-right)

;; Swap content between windows
(defun swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa
copied from https://stackoverflow.com/a/1774949"
  (interactive)
  (let* ((this (selected-window))
     (other (next-window))
     (this-buffer (window-buffer this))
     (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)))

(global-set-key (kbd "C-x C-M-o") 'swap-buffers-in-windows)

;; Swap "C-x b" and "C-x C-b" for buffer switching
(global-set-key (kbd "C-x b") 'list-buffers)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

;; Kill current buffer with "C-x C-k" (originally `kill-buffer')
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)


;;;; Editing interface
;;   =================

;;;;; Opening files
;; Auto load changed files
(global-auto-revert-mode t)

;; Rename opened file
(defun rename-current-file ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;; ido
(use-package ido
  :demand
  :config
  (setq ido-everywhere t
	ido-enable-flex-matching t
	ido-use-filename-at-point 'guess)
  (ido-mode t)
  :bind
  ;; Previously binded to `set-goal-coloum' and will always pop up.
  ("C-x C-n" . ido-switch-buffer-other-window))

;; Better display for files with the same name
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

;;;;; Line/column number
;; Do not show line number at bottom
(setq line-number-mode nil)

;; Show column number
(setq column-number-mode t)

;; Set line numbers with certain modes as exceptions
(use-package linum-off
  :init
  (global-linum-mode t)
  :config
  (add-to-list 'linum-disabled-modes-list 'ansi-term))

;;;;; Mode line & echo area
;; No type full yes
(defalias 'yes-or-no-p 'y-or-n-p)

;; Fast show prefix keys in echo area
(setq echo-keystrokes 0.1)

;;;;; Text area
;; ;; Easy toggle text size
;; (global-set-key (kbd "C-=") 'text-scale-increase)
;; (global-set-key (kbd "C--") 'text-scale-decrease)


;;;; Text
;;   ====

;;;;; Parentheses
;; Show parentheses
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; auto parenthese pair
(add-hook 'prog-mode-hook #'electric-pair-mode)
(setq-default electric-pair-inhibit-predicate
	      'electric-pair-conservative-inhibit)

;;;;; Empty texts
;; Tab size
(setq tab-width 4
      indent-tabs-model nil)

;; EOF newline
(setq require-final-newline t)

;; Auto delete trailing whitespaces.
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode)
  :config
  (setq ws-butler-keep-whitespace-before-point nil))

;;;;; Width control
;; Set fill-column
(setq-default fill-column 78)

(defun fill-paragraph-scroll-right ()
  "This function calls `fill-paragraph' with an extra
`scroll-right' to force align the left of the text to the edge of
the window, as sometimes `fill-paragraph' leaves the text
under-scrolled."
  (interactive)
  (fill-paragraph)
  (if (< (current-column) (window-body-width))
      (let ((offset
	     (- (current-column)
		(/ (- (car (window-absolute-pixel-position))
		      (car (window-absolute-body-pixel-edges)))
		   (frame-char-width)))))
	(if offset (scroll-right offset)))))

(global-set-key (kbd "M-q") 'fill-paragraph-scroll-right)

;; Unfill paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; Originally the same as (originally) M-q
(global-set-key (kbd "M-Q") 'unfill-paragraph)

;; Use visual-line-mode for org mode
;; (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;; (add-hook 'org-mode-hook #'visual-line-mode)

;;;;; Code folding
(use-package hideshow
  :init
  (add-hook #'prog-mode-hook #'hs-minor-mode)
  :diminish hs-minor-mode
  :bind
  (("C--" . hs-hide-all)
   ("C-=" . hs-show-all)
   ("<C-tab>" . hs-toggle-hiding)))


;;;; Editing control
;;   ===============

;;;;; General editing key-bindings
;; Create nextline for "C-o" (originally `open-line')
(global-set-key (kbd "C-o") (kbd "C-e C-M"))
;; Create a previous line for "C-j" (originally `eval-print-last-sexp')
(global-set-key (kbd "C-j") (kbd "C-a C-M C-p"))

;; Enable type return when holding control
(global-set-key (kbd "<C-return>") (kbd "RET"))

;; Originally `mark-page', unbind for easy mis-triggering
(global-unset-key (kbd "C-x C-p"))

;;;;; Navigating
;; Scrolling
(setq scroll-margin 3
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      ;; C-v and M-v scroll to the end
      scroll-error-top-bottom t)

;; Scroll half of window instead of one window.
(defun scroll-half-page-down ()
  "scroll down half the page"
  (interactive)
  (scroll-down-command (/ (window-body-height) 2)))

(defun scroll-half-page-up ()
  "scroll up half the page"
  (interactive)
  (scroll-up-command (/ (window-body-height) 2)))

;; Originally `scroll-up-command' and `scroll-down-command'.
(global-set-key (kbd "C-v") 'scroll-half-page-up)
(global-set-key (kbd "M-v") 'scroll-half-page-down)

;;;;; Selecting
;; Replace selected text
(delete-selection-mode t)

;; Cut region or line in one command
(defun slick-cut (beg end)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(advice-add 'kill-region :before #'slick-cut)

;; Copy region or line in one command
(defun slick-copy (beg end)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(advice-add 'kill-ring-save :before #'slick-copy)

;; Comment region or line in one command
(defun comment-or-uncomment-region-or-line ()
  "`comment-or-uncomment-region' or comment the current line if
there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)

;;;;; Deleting
;; Larger kill ring size
(setq kill-ring-max 500)

;; Delete word and line without saving to kill ring.
(defun delete-word (arg)
  "Delete word without saving to kill ring, modified from `kill-word'."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun delete-line (&optional arg)
  "Delete line without saving to kill ring, modified from `kill-line'."
  (interactive "P")
  (delete-region
   (point)
   (progn
     (if arg
	 (forward-visible-line (prefix-numeric-value arg))
       (if (eobp)
	   (signal 'end-of-buffer nil))
       (let ((end
	      (save-excursion
		(end-of-visible-line) (point))))
	 (if (or (save-excursion
		   ;; If trailing whitespace is visible,
		   ;; don't treat it as nothing.
		   (unless show-trailing-whitespace
		     (skip-chars-forward " \t" end))
		   (= (point) end))
		 (and kill-whole-line (bolp)))
	     (forward-visible-line 1)
	   (goto-char end))))
     (point))))

;; Originally `kill-word' and `kill-line'.
(global-set-key (kbd "M-d") 'delete-word)
(global-set-key (kbd "C-k") 'delete-line)

;; Kill whole line when using C-k at the start of the line.
(setq kill-whole-line t)

;;;;; Replacing
;; Set key-binding for replacing, originally binded by ido
(global-set-key (kbd "C-x C-r") 'replace-string)

;; Visual indication for replace-regexp
(use-package visual-regexp
  :bind
  ("C-x M-r" . vr/replace))


;;;; Version control
;;   ===============
;; I only use git currently
(setq vc-handled-backends '(Git))

;; Show edited lines at the side
(use-package diff-hl
  :config
  (custom-set-faces
  `(diff-hl-change ((t (:background "#3A4232" :foreground ,cus-bg-color))))
  `(diff-hl-insert ((t (:background "#223E32" :foreground ,cus-bg-color))))
  `(diff-hl-delete ((t (:background "#4E2E32" :foreground ,cus-bg-color)))))
  (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-pos)
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))


;;;; Org mode
;;   ========
(use-package org
  :config
  (setq org-startup-folded nil
	org-hide-leading-stars t)
  (setq org-todo-keywords
	'((sequence "TODO" "PEND" "|" "DONE")))
  (setq org-todo-keyword-faces
	'(("TODO" . "firebrick1")
	  ("PEND" . "brown")
          ("DONE" . "dark gray"))))


;;;; Content-specific settings
;;   =========================
;; elpy for python
(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
  (setq elpy-rpc-python-command "python3")
  (setq python-fill-docstring-style 'django)
  :bind
  (:map elpy-mode-map
	;; Overwrites `elpy-refactor-extract-function'
	("C-c C-r f" . elpy-format-code))
  (:map python-mode-map
	("C-c C-," . python-indent-shift-left)
	;; C-. might be used by ibus, can be changed in ibus-setup
	("C-c C-." . python-indent-shift-right)))

;; yasnippet
(use-package yasnippet
  :defer t)

;; markdown
(use-package markdown-mode
  :commands(markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; js
(use-package js-mode
  :defer t
  :mode (("\\.js" . js-jsx-mode))
  :init
  (setq js-indent-level 2))


;;;; Load work-specific configs
;; (file-exists-p (concat user-emacs-directory "work"))
(load (concat user-emacs-directory "work/config.el"))


;;;; Customize
;;   =========
(setq custom-file (concat user-emacs-directory "custom.el"))
(load-file custom-file)
