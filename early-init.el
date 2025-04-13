;; -*- lexical-binding: t; -*-

;; Disable unnecessary UI elements *before* they load
(setq inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)
;; set the frame size
(setq default-frame-alist '((width . 160) (height . 50) (pixel-width . 1200) (pixel-height . 800)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)
(which-key-mode t)

;; Set up basic optimizations
(setq gc-cons-threshold (* 50 1000 1000))  ;; Increase garbage collection threshold
(setq read-process-output-max (* 1024 1024)) ;; Better performance for LSP

;; Open URLs in firefox
(setq browse-url-browser-function 'browse-url-firefox)
(setq browse-url-firefox-new-window-is-tab t)

;; Prevent Emacs from loading outdated bytecode
(setq load-prefer-newer t)

;; Improve startup performance by delaying package initialization
(setq package-enable-at-startup nil)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      vc-handled-backends '(Git))

;; Hack to avoid being flashbanged
(defun my-emacs/avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs."
  (setq mode-line-format nil)
  ;; These colors should match your selected theme for maximum effect
  ;; Note that for catppuccin whenever we create a new frame or open it on terminal
  ;; it is necessary to reload the theme.
  (set-face-attribute 'default nil :background "#161a1f" :foreground "#cdcdcd"))

(my-emacs/avoid-initial-flash-of-light)


;; Better Window Management handling
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("Emacs"))

(setq inhibit-compacting-font-caches t)

;; Disables unused UI Elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(provide 'early-init)
;;; early-init.el ends here
