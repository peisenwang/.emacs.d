;;; Emacs configuration
;;  ===================
;; Copyright 2016 - 2025 Peisen Wang
;;
;; Use "grep -E '^;{3,}' init.el" to show the outline of this file.

;; User info
(setq user-full-name "Peisen Wang"
      user-mail-address "wpeisen@gmail.com")


;;;; Startup and gui
;;   ===============
;; This part is moved to early-init.el to speed up loading.

;; Split window after gui init
(split-window-horizontally)


;;;; Customize
;;   =========
;; Put before most other config like themes
(setq custom-file (concat user-emacs-directory "custom.el"))
(if (file-exists-p custom-file)
    (load-file custom-file))


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
(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-common-palette-overrides
	'(;; (bg-main bg-dim)
	  ;; Set fringe color same as bg
	  (fringe bg-main)
	  ;; Set line number area same as bg
	  (bg-line-number-active bg-main)
	  (bg-line-number-inactive bg-main)
	  ;; No visual borderline
	  (border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)))

  (setq modus-vivendi-palette-overrides
	'(
	  ;; Use fainter color for org mode headings
	  ;; Generate the colors: utils/generate_colors.py
	  (fainter-0 "#e0c1b6")
	  (fainter-1 "#d2e0b6")
	  (fainter-2 "#b6e0c7")
	  (fainter-3 "#b6cce0")
	  (fainter-4 "#ccb6e0")
	  (fainter-5 "#e0b6c7")
	  (fainter-6 "#e0d2b6")
	  (fg-heading-2 fainter-3)
	  (fg-heading-3 fainter-2)
	  (fg-heading-4 fainter-6)
	  (fg-heading-5 fainter-4)
	  (fg-heading-6 fainter-1)
	  (fg-heading-7 fainter-5)
	  (prose-done fg-dim)))
  (load-theme 'modus-vivendi :no-confirm)
  ;; Get background color
  (setq cus-bg-color (nth 1 (assoc 'bg-main modus-vivendi-palette))))

;; Make line wrap arrows darker
(defface fringe-dark
  '((t (:foreground "#525252")))
  "Face for fringe bitmaps."
  :group 'basic-faces)

(set-fringe-bitmap-face 'left-curly-arrow 'fringe-dark)
(set-fringe-bitmap-face 'right-curly-arrow 'fringe-dark)


;;;; Backup and autosave
;;   ===================
;; Backup ([FILE]~, [FILE].~1~) settings
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Stop backing up remote files to speedup tramp
(setq backup-enable-predicate '(lambda (f) (not (file-remote-p f))))

;; Auto save (#[FILE]#) settings
(defvar auto-save-file-directory
  (concat temporary-file-directory "emacs-auto-saves"))
(make-directory auto-save-file-directory t)
(setq auto-save-file-name-transforms
      `((".*" ,auto-save-file-directory t)))

;; Tramp
;; See https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./ for
;; some further improvement insights
(use-package tramp
  :after recentf
  :config
  (setq tramp-auto-save-directory auto-save-file-directory)
  ;; Prevent tramp from asking to verify "Autosave file on local temporary 
  ;; directory, do you want to continue?" when logging into remote container 
  ;; as root.
  (setq tramp-allow-unsafe-temporary-files t)
  ;; From manual: You can prevent the creation of remote lock files by setting 
  ;; the variable `remote-file-name-inhibit-locks' to t.
  (setq remote-file-name-inhibit-locks t)
  ;; Use a larger maximum file size for base64 inline copying
  (setq tramp-copy-size-limit (* 1024 1024)) ;; 1Mb
  ;; From tramp user manual
  (remove-hook
   'tramp-cleanup-connection-hook
   #'tramp-recentf-cleanup)
  (remove-hook
   'tramp-cleanup-all-connections-hook
   #'tramp-recentf-cleanup-all))

;; Save history for M-x etc
(savehist-mode t)
(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring))

;; Do not create lock files
(setq create-lockfiles nil)

;; recentf
(defun recentf-open-files-without-indent (&optional files buffer-name)
  "Similar to `recentf-open-files' without instructions or indent
before items"
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
  :init
  ;; Recentf for some reason will clean opened tramp files when the system is
  ;; offline. Disable auto cleanup temporarily to prevent that.
  (custom-set-variables '(recentf-auto-cleanup 'never))
  :config
  (setq recentf-max-saved-items 500
	recentf-max-menu-items 50)
  ;; Don't show leading numbers as I never use them
  (setq recentf-show-file-shortcuts-flag nil)
  ;; Sometimes package update results in visiting internal files, use the
  ;; following to exclude package files.
  ;; (add-to-list
  ;;  'recentf-exclude (format "%s/\\.emacs\\.d/elpa/.*" (getenv "HOME")))
  (setq recentf-keep '(recentf-keep-default-predicate file-remote-p))
  :bind
  ("C-x O" . recentf-open-files-without-indent))


;;;; General settings
;;   ================
;; Use command as meta and option as super for mac
(setq mac-option-modifier 'super
      mac-command-modifier 'meta)


;;;; Window & buffer management
;;   ==========================

;; Switch window with simpler key binding
;; Previous binded to `open-line'
(global-set-key (kbd "C-o") 'other-window)

;; Use "C-q" to switch window like what I do in tmux
;; (originally `quoted-insert')
(global-unset-key (kbd "C-q"))
(global-set-key (kbd "C-q C-p") 'windmove-up)
(global-set-key (kbd "C-q C-n") 'windmove-down)
(global-set-key (kbd "C-q C-f") 'windmove-right)
(global-set-key (kbd "C-q C-]") 'windmove-right)
(global-set-key (kbd "C-q C-b") 'windmove-left)
(global-set-key (kbd "C-q C-[") 'windmove-left)
(global-set-key (kbd "C-q C-o") 'other-window)
(global-set-key (kbd "C-q -") 'split-window-below)
(global-set-key (kbd "C-q |") 'split-window-right)
(global-set-key (kbd "C-q 0") 'delete-window)
;; Use "C-q C-q" for `quoted-insert' instead
(global-set-key (kbd "C-q C-q") 'quoted-insert)

;; Even simpler moving between windows
(global-set-key (kbd "M-]") 'windmove-right)
(global-set-key (kbd "M-[") 'windmove-left)

;; Wrap around when moving around windows
(setq windmove-wrap-around t)

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

;; Use "C-x C-b" instead of "C-x b"  for buffer switching, previously binded to `list-buffers'
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
;; Use `buffer-menu' instead of `list-buffers' to open list in current 
;; window. Previously binded to `switch-to-buffer'
(global-set-key (kbd "C-x b") 'buffer-menu)

;; Kill current buffer with "C-x C-k" (originally `kill-buffer')
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
;; ;; Kill current buffer with "M-k" (originally `kill-sentence')
;; (global-set-key (kbd "M-k") 'kill-this-buffer)

;; Reload buffer manually
(global-set-key (kbd "<f5>") 'revert-buffer)


;;;; Editing interface
;;   =================

;;;;; Opening files
;; Auto load changed files
(global-auto-revert-mode t)

(defun find-file-recentf ()
  "Open files from recentf."
  (interactive)
  (find-file (completing-read "Recent Files: " recentf-list nil t)))

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

;; ;; ido
;; (use-package ido
;;   :demand
;;   :config
;;   (setq ido-everywhere t
;; 	ido-enable-flex-matching t
;; 	ido-use-filename-at-point 'guess
;; 	;; Stop ido from looking at other places when creating a file.
;; 	ido-auto-merge-work-directories-length -1)
;;   (ido-mode t)
;;   :bind
;;   ;; Previously binded to `set-goal-coloum' and will always pop up.
;;   ("C-x C-n" . ido-switch-buffer-other-window))

;; fido-vertical-mode
;; Note: Use "M-j" instead of enter to open file without auto completion.
(use-package icomplete
  :demand
  :after (recentf)
  :config
  (fido-vertical-mode)
  :bind
  ;; Previous binded to `other-window'
  ("C-x C-o" . find-file-recentf)
  (:map icomplete-fido-mode-map
	;; Originally bind to `icomplete-fido-kill', change to custom function
	;; preventing saving to kill ring
	("C-k" . delete-line)))

;; Better display for files with the same name
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

;;;;; Line/column number
;; Do not show line number at bottom
(setq line-number-mode nil)

;; Show column number
(setq column-number-mode t)

;; Display line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)


;;;;; Mode line
;; Show hostname in buffer name when accessing remote files (as "tramp-theme"
;; does)
;; Modified from "How to get host indication in the mode line?" section in
;; https://gnu.huihoo.org/emacs/manual/tramp/Frequently-Asked-Questions.html
(defconst mode-line-buffer-identification-with-remote
  (list
   '(:eval
     (let ((host-name (file-remote-p default-directory 'host)))
       (concat
	(if host-name (concat host-name ": ")
	  ""))))
   "%12b"))

(setq-default
 mode-line-buffer-identification
 mode-line-buffer-identification-with-remote)

(add-hook
 'dired-mode-hook
 (lambda ()
   (setq
    mode-line-buffer-identification
    mode-line-buffer-identification-with-remote)))

;;;;; Echo area
;; No type full yes
(setq use-short-answers t)

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
(setq-default tab-width 4
	      indent-tabs-mode nil)

;; EOF newline
(setq require-final-newline t)

;; Auto delete trailing whitespaces.
(use-package ws-butler
  :ensure t
  :diminish
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
      (scroll-right offset)))

(global-set-key (kbd "M-q") 'fill-paragraph-scroll-right)

;; Unfill paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; Originally the same as (originally) "M-q"
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

;;;;; Highlighting
(use-package symbol-overlay
  :hook
  ((prog-mode . symbol-overlay-mode))
  :bind
  (("M-i" . 'symbol-overlay-put)
   ("M-n" . 'symbol-overlay-jump-next)
   ("M-p" . 'symbol-overlay-jump-prev)
   ("M-N" . 'symbol-overlay-switch-forward)
   ("M-P" . 'symbol-overlay-switch-backward)
   ("M-I" . 'symbol-overlay-remove-all)))

;;;; Editing control
;;   ===============

;;;;; General editing key-bindings
;; Create new line at next line and move to it (originally
;; `electric-newline-and-maybe-indent')
(global-set-key (kbd "C-j") (kbd "C-e C-M"))
;; Create new line at previous line and move to it (originally
;; `default-indent-newline' binded to both)
(global-set-key (kbd "M-j") (kbd "C-a C-M C-p"))
(global-set-key (kbd "C-M-j") (kbd "C-a C-M C-p"))

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
     (set-marker (mark-marker) (point))
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


;;;; Completion
;;   ==========
(use-package corfu
  :custom
  (corfu-auto t)  ;; Auto completion
  (corfu-cycle t)  ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect 'prompt)  ;; Always preselect the prompt
  ;; :hook ((prog-mode . corfu-mode)
  :init
  (global-corfu-mode))

;; Enable indentation+completion using the TAB key.
(setq tab-always-indent 'complete)


;;;; Tree-sitter
;;   ===========
;; Use `treesit-install-language-grammar' to install lauguage grammers
(use-package treesit
  :config
  (setq treesit-language-source-alist
         '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
           (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
           (cmake      . ("https://github.com/uyha/tree-sitter-cmake"))
           (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
           (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
           (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
           (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
           (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
           (yaml       . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml")))))


;;;; Version control
;;   ===============
;; I only use git currently
(setq vc-handled-backends '(Git))

;; Disable vc for remote files as tramp sometimes breaks with "Tramp: Checking 
;; 'vc-registered' for [path]...failed"
;; From tramp FAQ
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; Show edited lines at the side
(use-package diff-hl
  :ensure t
  :after (modus-themes)
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
  ;; Originally `org-return-and-maybe-indent' in `org-mode-map'.
  (unbind-key "C-j" org-mode-map)
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
;; python
;; Requirement: "pip install python-lsp-server" into the venv
;; Also: python-lsp-ruff, pylsp-mypy, pylsp-rope
;; Possible setup: https://www.adventuresinwhy.com/post/eglot/
(use-package eglot
  ;; :custom
  ;; (eglot-events-buffer-size 0) ;; No event buffers (Lsp server logs)
  ;; (eglot-report-progress nil) ;; Disable lsp server logs
  ;; (eglot-autoshutdown t);; Shutdown unused servers.
  :hook
  ((python-ts-mode . eglot-ensure)))

;; Have to specifically use `python' to change the `python-ts-mode-map' keymap
(use-package python
  :init
  (add-to-list 'major-mode-remap-alist
	       '(python-mode . python-ts-mode))
  :bind
  (:map python-ts-mode-map
	("C-c C-," . python-indent-shift-left)
	;; C-. might be used by ibus, can be changed in ibus-setup
	("C-c C-." . python-indent-shift-right)))

(use-package pyvenv
  :after python
  :config
  (pyvenv-mode t)
  ;; Default venv to use
  (pyvenv-activate "~/.venv/m"))

;; yaml
(use-package yaml-ts-mode
  :mode "\\.ya?ml\\'")

;; cmake
(use-package cmake-ts-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; tex
;; Have to specifically use `latex' to change the `LaTeX-mode-map' keymap
(use-package latex
  :bind
  (:map LaTeX-mode-map
	;; Originally `reindent-then-newline-and-indent' in `LaTeX-mode-map'
	("C-j" . nil)))

;; yasnippet
(use-package yasnippet
  :config
  (yas-global-mode))

;; markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; js
(use-package js-mode
  :mode (("\\.js\\'" . js-jsx-mode))
  :init
  (setq js-indent-level 2))

;; epub
(use-package nov
  :mode ("\\.epub\\'" . nov-mode))


;;;; Load work-specific configs
;;   ==========================
(if (file-exists-p (concat user-emacs-directory "work"))
    (load (concat user-emacs-directory "work/config.el")))


;;;; Finish
;;   ======
;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
