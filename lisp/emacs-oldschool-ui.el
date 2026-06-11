;; emacs-oldschool-ui.el --- Retro Emacs UI setup

;; Use bitmap font if available
;; (set-frame-font "fixed-12" t t)

;; Disable UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Set background and foreground colors (minimalist)
(set-foreground-color "black")
(set-background-color "gray90")
(set-cursor-color "black")

;; Minimal syntax highlighting
(setq font-lock-maximum-decoration 1)
(global-font-lock-mode 1)

;; Default face tweaks (optional)
(set-face-attribute 'default nil :weight 'normal :height 150)

;; No fringe/border padding (optional)
(set-fringe-mode 0)

(provide 'emacs-oldschool-ui)
