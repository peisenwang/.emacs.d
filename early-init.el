;;; Early emacs configurations before loading GUI
;;
;; Copyright 2022-2024 Peisen Wang

;; Make startup faster by reducing the frequency of garbage collection. This
;; will be set back when startup finishes.
;; Change the value from 800k (default) to 50M
(setq gc-cons-threshold (* 50 1000 1000))

;; GUI
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; Startup message
(setq inhibit-startup-message t
      initial-scratch-message nil)

;; Setup window
(setq default-frame-alist '((fullscreen . fullheight) (width . 173)))

;; Set title
(setq frame-title-format '("%f"))

;;; Native compilation
(setq native-comp-async-report-warnings-errors 'silent)

;; Set *scratch* buffer mode to plain text
(setq initial-major-mode 'text-mode)
