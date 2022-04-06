;;; Early emacs configurations before loading GUI
;;
;; Copyright 2022 Peisen Wang

;; GUI
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; Startup message
(setq inhibit-startup-message t
      initial-scratch-message nil)

;; Setup window
(setq default-frame-alist '((fullscreen . fullheight) (width . 170)))

;; Set title
(setq frame-title-format '("%f"))

;;; Native compilation
(setq native-comp-async-report-warnings-errors 'silent)
