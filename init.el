;; -*- lexical-binding: t; -*-

;; ------------------------
;; Package Management Setup
;; ------------------------
(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ;; Always download missing packages

;; ------------------------
;; UI Tweaks and Visual Enhancements
;; UI in Early init
;; AND KEY BINDS
;; ------------------------
(setq auto-save-default t)        ;; enable auto-save
(setq auto-save-timeout 20)       ;; save after 20 seconds of idle
(setq auto-save-interval 200)     ;; or after 200 input events

(setq backup-directory-alist `(("." . "~/.emacs.d/backups"))) ;; put backups in one place
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-saves/" t)))
(make-directory "~/.emacs.d/auto-saves/" t)
(setq auto-save-list-file-prefix "~/.emacs.d/auto-save-list/.saves-") ;; promp recovery


;;(setq display-line-numbers 'relative)
(global-display-line-numbers-mode 1)  ;; Show line numbers
(setq confirm-kill-emacs #'y-or-n-p)  ;; Ask before quitting
(setq confirm-kill-processes nil)        ;; Don't ask about background processes
(global-visual-line-mode 1)  ;; Enables word wrap everywhere
(electric-pair-mode 1)  ;; Enable auto-pairing for (), {}, []
(setq electric-pair-preserve-balance t)  ;; Keep pairs balanced
(setq use-dialog-box nil)


(global-set-key (kbd "C-x k") (lambda () (interactive) (kill-buffer (current-buffer))))

;; open file at last edited position
(setq save-place-mode t)

;; recent files list in minbuffer
(recentf-mode 1)
(setq recentf-max-menu-items 15)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;(set-face-attribute 'default nil :family "Anonymous Pro" :height 130)



(set-face-attribute 'default nil :family "GeistMono Nerd Font" :height 150)
(set-face-attribute 'variable-pitch nil :family "Alegreya":height 140)
;; (set-face-attribute 'default nil :family "Iosevka" :height 155)
;; (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")
;;(set-face-attribute 'org-modern-symbol nil :family "Iosevka")

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; PROJECT MANAGMENT WITH PROJECTILE
(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


;;; ELEC_PAIR
(use-package elec-pair
  :ensure nil
  :defer t
  :hook (after-init . electric-pair-mode))

;;;;;;;;;;;;;;;;;;
;; undo redo
;;;;;;;;;;;;;;;;;
(use-package undo-fu
  :ensure t)
(global-set-key (kbd "C-z") #'undo-fu-only-undo)
(global-set-key (kbd "C-S-z") #'undo-fu-only-redo)
(use-package undo-fu-session
  :after undo-fu
  :config
  (undo-fu-session-global-mode))

;;;;;;;;;;;;;;;;;;;
;; SPELL CHECKER ;;
;;;;;;;;;;;;;;;;;;;
(setq ispell-program-name "hunspell")
(setq ispell-dictionary "en_US") 

;; testing save place, now
;; ------------------------
;; Startup Dashboard
;; ------------------------


(defun my-dashboard ()
  "Create a simple startup dashboard and center it in the buffer."
  (switch-to-buffer "*dashboard*")
  (read-only-mode 0)   ;; Allow modification
  (erase-buffer)
  (display-line-numbers-mode -1)

  ;; Get window width for centering
  (let* ((win-width (window-width))
	 (win-height (window-height))
         (text-width 40)  ;; Adjust this based on content width
	 (text-height 20)

         (left-padding (max 0 (/ (- win-width text-width) 2)))
	 (top-padding (max 0 (/ (- win-height text-height)2))))
    (dotimes (_ top-padding) (insert "\n"))

    ;; Define a helper function to insert centered text
    (defun insert-centered (text &optional face)
      (let ((line (concat (make-string left-padding ?\s) text "\n")))
	(insert (propertize line 'face face))))

    (insert "\n\n\n\n")
    (insert-centered "███████╗███╗   ███╗ █████╗  ██████╗███████╗" '(:foreground "#716C9C"))
    (insert-centered "██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝" '(:foreground "#716C9C"))
    (insert-centered "█████╗  ██╔████╔██║███████║██║     ███████╗" '(:foreground "#716C9C"))
    (insert-centered "██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║" '(:foreground "#716C9C"))
    (insert-centered "███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║" '(:foreground "#716C9C"))
    (insert-centered "╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝" '(:foreground "#716C9C"))
    (insert "\n\n")
    (insert-centered (format "\t  Welcome to Emacs, %s!\n" user-login-name) '(:foreground "#716C9C"))
    (insert-centered (format "\t  Loading time : %s" (emacs-init-time)) '(:foreground "#716C9C"))
    (insert-centered (format "\t  Packages     : %s" (length package-activated-list)) '(:foreground "#716C9C"))
    (insert "\n\n")


    (insert-centered (format "\t  Emacs version: %s" emacs-version) '(:foreground "#716C9C"))
    (insert "\n")


    (insert "\n\n"))

  (read-only-mode 1)   ;; Make buffer read-only again
  (goto-char (point-min)))  

(defun my-dashboard-update-on-resize (_frame)
  (when (string= (buffer-name) "*dashboard*")
    (my-dashboard)))

;;(add-hook 'window-size-change-functions #'my-dashboard-update-on-resize)
;;(add-hook 'emacs-startup-hook #'my-dashboard)



(setq initial-buffer-choice t)

(defun my-show-dashboard ()
  (when (display-graphic-p)
    (my-dashboard)))

;;(add-hook 'server-after-make-frame-hook #'my-show-dashboard)

;; ------------------------
;; Buffer Management (Ibuffer)
;; ------------------------
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; -----------------------
;; File position save
;; ----------------------
(require 'saveplace)
(setq save-place-file (expand-file-name ".emacs-places" user-emacs-directory))
(add-hook 'find-file-hook #'save-place-find-file-hook)
(setq-default save-place t)  ;; Enable for all buffers

;; -----------------------
;; MAGIT version control
;; -----------------------
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

;; -----------------------
;; diff-hl tracking changes
;; -----------------------
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode)) 
(setq diff-hl-margin-mode t)


;; ------------------------
;; Org Mode Configuration
;; ------------------------

(use-package org
  :config
  (autoload 'org-tempo "org-tempo" nil t)
  (setq org-directory "~/org"
	org-agenda-files "~/org/agenda.org"
	org-startup-indented t
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-startup-with-inline-images nil
	org-startup-folded t
        org-export-with-sub-superscripts '{}
        org-highlight-latex-and-related '(native script entities))
  (add-hook 'org-mode-hook 'org-indent-mode))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)  ;; Enable org-bullets in org-mode
  :config
  (setq org-bullets-bullet-list '("▶" "◉" "○" "◆" "•")))

;; Better editing with pretty symbols
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t
        org-appear-autoemphasis t
        org-appear-autosubmarkers t))
;; (use-package corfu
;;   :ensure t
;;   :init
;;   (global-corfu-mode)  ;; Enable Corfu globally
;;   :custom
;;   (corfu-auto t)        ;; Enable auto completion
;;   (corfu-auto-delay 0.1)
;;   (corfu-cycle t)       ;; Cycle through candidates
;;   :hook
;;   ((org-mode . corfu-mode-disable)))  ;; Disable corfu-mode in org-mode

;; (set-face-attribute 'corfu-default nil 
;;                     :background (face-attribute 'mode-line :background)
;;                     :foreground (face-attribute 'mode-line :foreground))

;; (set-face-attribute 'corfu-current nil
;;                     :background (face-attribute 'mode-line :foreground)
;;                     :foreground (face-attribute 'mode-line :background))

(defun corfu-mode-disable ()
  "Disable corfu mode in Org mode."
  (corfu-mode -1))


(use-package org-download
  :after org
  :config
  
  ;; Save images in a subdirectory relative to the Org file
  (setq org-download-method 'directory)
  (setq org-download-image-dir "./images") ;; Store images in "images/" relative to the Org file
  ;; Fix screenshot method for Wayland
    (setq org-download-screenshot-method "grim -g \"$(slurp)\" %s")
;;  (setq org-download-screenshot-method "wl-paste --type image/png > %s")
  ;; Automatically create the directory if it doesn't exist
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local org-download-image-dir
                          (concat (file-name-directory (buffer-file-name)) "images/"))))

  ;; Enable org-download in Dired mode
  (add-hook 'dired-mode-hook 'org-download-enable)
  (define-key org-mode-map (kbd "M-p") 'org-download-screenshot))
;;  (global-set-key (kbd "M-p")  #'org-download-screenshot)

  




(defun my-add-electric-pairs ()
  "Enable pairing for additional characters."
  (setq-local electric-pair-inhibit-predicate
              (lambda (c) (if (char-equal c ?$) nil (electric-pair-default-inhibit c))))
  (setq-local electric-pair-pairs (append electric-pair-pairs '((?$ . ?$)))))

(add-hook 'org-mode-hook #'my-add-electric-pairs)
(add-hook 'latex-mode-hook #'my-add-electric-pairs)

;;;;;;;;;;;;;;;;;;;;;;
;; ORG MODERN ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;
;; (package-install 'org-modern)
;; (use-package org-modern
;;   :ensure t
;;   :hook (org . org-modern))
;; (with-eval-after-load 'org (global-org-modern-mode))

;; Set the bibliography file
;;(setq org-cite-global-bibliography '("~/org/research/references.bib"))

;; Use citar for citation completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG MODE CUSTOM FORMATTING ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; ADD CUSTOM FACES FOR STUFF
(with-eval-after-load 'org
  (require 'org-tempo)  ;; Ensure org-tempo is loaded
  (font-lock-add-keywords 'org-mode
                          '(("^\\s-*#\\+begin_definition" . font-lock-keyword-face)
                            ("^\\s-*#\\+begin_theorem" . font-lock-constant-face)
                            ("^\\s-*#\\+begin_proposition" . font-lock-constant-face)
                            ("^\\s-*#\\+begin_example" . font-lock-constant-face)
                            ("^\\s-*#\\+begin_remark" . font-lock-constant-face))))

;; GETTING TEMPO TO USE IT
(defun my/org-add-templates ()
  (dolist (template '(("d" . "definition")
                      ("t" . "theorem")
                      ("p" . "proposition")
                      ("e" . "example")
                      ("rem" . "remark")))
    (unless (assoc (car template) org-structure-template-alist)
      (add-to-list 'org-structure-template-alist template))))
(add-hook 'org-mode-hook #'my/org-add-templates)

;; html exports
(with-eval-after-load 'ox-html
  (defun my-org-html-special-block (special-block contents info)
    "Export #+begin_definition and #+begin_theorem blocks with special styling."
    (let* ((type (org-element-property :type special-block))
           (class (cond ((string= type "definition") "definition")
                        ((string= type "theorem") "theorem")
                        ((string= type "proposition") "proposition")
                        ((string= type "example") "example")
                        ((string= type "remark") "remark")
                        (t type))))
      (format "<div class=\"%s\">\n%s\n</div>" class contents)))

  (advice-add 'org-html-special-block :override #'my-org-html-special-block))

;; ------------------------
;; Dired with Minibuffer and Fuzzy Search
;; ------------------------
(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode))

(use-package vertico
  :init (vertico-mode))

(use-package orderless
  :config (setq completion-styles '(orderless)))

(use-package dired
  :ensure nil
  :bind ("C-x d" . dired))


;; ------------------------
;; Lightweight Syntax Highlighting & Auto-completion
;; ------------------------

(add-to-list 'package-selected-packages 'dash)


(use-package eglot
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (scheme-mode . eglot-ensure))
  :config
  ;; Configure how Eglot displays documentation
  (setq eglot-ignored-server-capabilities '(:hoverProvider)) ;; Disable if unwanted
  (setq eglot-send-changes-idle-time 0.5)  ;; Reduce delay in sending changes
  (setq eglot-autoshutdown t)  ;; Automatically shutdown LSP servers when not needed

  ;; Set up keybindings similar to lsp-mode
  (define-key eglot-mode-map (kbd "M-.") #'xref-find-definitions)  ;; Jump to definition
  (define-key eglot-mode-map (kbd "M-,") #'xref-pop-marker-stack) ;; Jump back
  (define-key eglot-mode-map (kbd "C-c h") #'eldoc-doc-buffer) ;; Show documentation
  (define-key eglot-mode-map (kbd "C-c r") #'eglot-rename) ;; Rename symbol
  (define-key eglot-mode-map (kbd "C-c f") #'eglot-format-buffer) ;; Format buffer
  (define-key eglot-mode-map (kbd "C-c a") #'eglot-code-actions)) ;; Code actions

(use-package yasnippet
  :config (yas-global-mode 1))

;; Optional: Company mode for completion (Eglot integrates with it)
(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (global-company-mode))

;; Treesitter
;; (use-package tree-sitter
;;   :ensure t
;;   :hook ((prog-mode . tree-sitter-mode)
;;          (prog-mode . tree-sitter-hl-mode))
;;   :config
;;   (global-tree-sitter-mode))
;; (use-package tree-sitter-langs
;;   :ensure t
;;   :after tree-sitter  ;; Ensure tree-sitter is loaded first
;;   :config
;;   (dolist (lang '(c c++ python go scheme))
;;     (add-to-list 'tree-sitter-major-mode-language-alist
;;                 (cons lang lang))))
;; Optional: Set up LSP (for enhanced completion and features) without Emacs Lisp

 


;; (use-package lsp-mode
;;   :ensure t
;;   :hook ((c-mode . lsp)
;;          (cpp-mode . lsp)
;;          (python-mode . lsp)
;;          (go-mode . lsp)
;;          (scheme-mode . lsp))
;;   :commands lsp)

;; (use-package lsp-ui
;;   :ensure t
;;   :after lsp-mode
;;   :config
;;   (setq lsp-ui-doc-enable t
;;         lsp-ui-doc-position 'at-point))
;; ------------------------
;;; THEMES
;; ------------------------
(setq custom-safe-themes t)
 

;; (use-package spaceway-theme
;;   :ensure nil
;;   :load-path "lisp/spaceway/"
;;   :config
;;   (load-theme 'spaceway t)
;;   (global-hl-line-mode t)
;;   (set-frame-parameter nil 'cursor-color "#dc322f")
;;   (add-to-list 'default-frame-alist '(cursor-color . "#dc322f"))
;;    ;; Set background and face colors
;;   (defun my/set-nofrils-bg ()
;;     ;;#959AA8
;;     ;;"#BAB5A1"
;;     (let ((bg "#262626")) ;; your custom background
;;       (set-face-background 'default bg)
;;       (set-face-background 'fringe bg)
;;       ;;(set-face-background 'linum bg)
;;       (set-face-background 'line-number bg)
;;       (set-face-background 'line-number-current-line bg)
;;       (set-face-background 'mode-line bg)
;;       (set-face-background 'mode-line-inactive bg)
;;       (add-to-list 'default-frame-alist `(background-color . ,bg))
;;       (set-background-color bg)))
;;   (my/set-nofrils-bg))

;; (use-package ef-themes
;;   :ensure t)
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-ayu-dark t))


;; (use-package almost-mono-themes
;;   :ensure t
;;   :config
;;   (load-theme 'almost-mono-cream))

;; (use-package nofrils-acme-theme
;;   :ensure t
;;   :config
;;   (load-theme 'nofrils-light t)
;;   (global-hl-line-mode t)



;;   (my/set-nofrils-bg))
;; (set-face-foreground 'font-lock-comment-face "#3D4459")
;; ;;(set-face-foreground 'font-lock-comment-face "#4A6A94")

;; (set-face-attribute 'mode-line nil :foreground "#000000" :background "#686868")
;;(load-theme 'spaceway t)
(setenv "SCHEME" "dark")


;; ----------------------
;;; EMACS-SOLO-MODE-LINE
;; ---------------------
;;  Customizations to the mode-line
;;
(use-package emacs-solo-mode-line
  :ensure nil
  :no-require t
  :defer t
  :init
  ;; Shorten big branches names
  (defun emacs-solo/shorten-vc-mode (vc)
    "Shorten VC string to at most 20 characters.qq
 Replacing `Git-' with a branch symbol."
    (let* ((vc (replace-regexp-in-string "^ Git[:-]" "  " vc))) ;; Options:   ᚠ ⎇
      (if (> (length vc) 20)
          (concat (substring vc 0 20) "…")
        vc)))

  ;; Formats Modeline
  (setq-default mode-line-format
                '("%e" "  "
                  ;; (:propertize " " display (raise +0.1)) ;; Top padding
                  ;; (:propertize " " display (raise -0.1)) ;; Bottom padding
                  (:propertize "λ  " face font-lock-keyword-face)

                  (:propertize
                   ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))

                  mode-line-frame-identification
                  mode-line-buffer-identification
                  "   "
                  mode-line-position
                  mode-line-format-right-align
                  "  "
                  (project-mode-line project-mode-line-format)
                  "  "
                  (vc-mode (:eval (emacs-solo/shorten-vc-mode vc-mode)))
                  "  "
                  ;;mode-line-modes
                  ;;mode-line-misc-info
                  "  ")
                project-mode-line t
                mode-line-buffer-identification '(" %b")
                mode-line-position-column-line-format '(" %l:%c"))

  ;; Provides the Diminish functionality
  (defvar emacs-solo-hidden-minor-modes
    '(abbrev-mode
      eldoc-mode
      flyspell-mode
      smooth-scroll-mode
      outline-minor-mode
      which-key-mode))

  (defun emacs-solo/purge-minor-modes ()
    (interactive)
    (dolist (x emacs-solo-hidden-minor-modes nil)
      (let ((trg (cdr (assoc x minor-mode-alist))))
        (when trg
          (setcar trg "")))))

  (add-hook 'after-change-major-mode-hook 'emacs-solo/purge-minor-modes))
;;(display-time-mode t)
;;(display-battery-mode t)
;; ---------- EMACS-SOLO-OLIVETTI
(use-package emacs-solo-olivetti
  :ensure nil
  :no-require t
  :defer t
  :init
  (defvar emacs-solo-center-document-desired-width 90
    "The desired width of a document centered in the window.")

  (defun emacs-solo/center-document--adjust-margins ()
    ;; Reset margins first before recalculating
    (set-window-parameter nil 'min-margins nil)
    (set-window-margins nil nil)

    ;; Adjust margins if the mode is on
    (when emacs-solo/center-document-mode
      (let ((margin-width (max 0
                               (truncate
                                (/ (- (window-width)
                                      emacs-solo-center-document-desired-width)
                                   2.0)))))
        (when (> margin-width 0)
          (set-window-parameter nil 'min-margins '(0 . 0))
          (set-window-margins nil margin-width margin-width)))))

  (define-minor-mode emacs-solo/center-document-mode
    "Toggle centered text layout in the current buffer."
    :lighter " Centered"
    :group 'editing
    (if emacs-solo/center-document-mode
        (add-hook 'window-configuration-change-hook #'emacs-solo/center-document--adjust-margins 'append 'local)
      (remove-hook 'window-configuration-change-hook #'emacs-solo/center-document--adjust-margins 'local))
    (emacs-solo/center-document--adjust-margins))


  (add-hook 'org-mode-hook #'emacs-solo/center-document-mode)
  (add-hook 'gnus-group-mode-hook #'emacs-solo/center-document-mode)
  (add-hook 'gnus-summary-mode-hook #'emacs-solo/center-document-mode)
  (add-hook 'gnus-article-mode-hook #'emacs-solo/center-document-mode)

  ;; (add-hook 'newsticker-treeview-list-mode-hook 'emacs-solo/timed-center-visual-fill-on)
  ;; (add-hook 'newsticker-treeview-item-mode-hook 'emacs-solo/timed-center-visual-fill-on)
  )
;; ------------------------
;; Final Performance Tweaks
;; ------------------------
(setq gc-cons-threshold (* 100 1000 1000))  ;; 100MB during startup
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 10 1000 1000))))  ;; Reset to 10MB

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d6b934330450d9de1112cbb7617eaf929244d192c4ffb1b9e6b63ad574784aad"
     "c5878086e65614424a84ad5c758b07e9edcf4c513e08a1c5b1533f313d1b17f1"
     "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e"
     "1ad12cda71588cc82e74f1cabeed99705c6a60d23ee1bb355c293ba9c000d4ac"
     "6f1f6a1a3cff62cc860ad6e787151b9b8599f4471d40ed746ea2819fcd184e1a"
     "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882"
     "d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c"
     "8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae"
     "cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2"
     "ffba0482d3548c9494e84c1324d527f73ea4e43fff8dfd0e48faa8fc6d5c2bc7"
     "ae20535e46a88faea5d65775ca5510c7385cbf334dfa7dde93c0cd22ed663ba0"
     "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290"
     "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36"
     "77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d"
     "7c28419e963b04bf7ad14f3d8f6655c078de75e4944843ef9522dbecfcd8717d"
     "c8c4baac2988652a760554e0e7ce11a0fe0f8468736be2b79355c9d9cc14b751"
     "3c08da65265d80a7c8fc99fe51df3697d0fa6786a58a477a1b22887b4f116f62"
     "30d174000ea9cbddecd6cc695943afb7dba66b302a14f9db5dd65074e70cc744"
     "f9fe320c8e4e759d4788c6d5aa30c65caae7153a844d90618565b8d9e49b16e0"
     "c21904759f8d6d73f8be4a03c71a81b9908e71276c490f664022bf997111b458"
     "6e8f43c0b76fc272cfa811b709c62fb4bf79855cf2ce0389a6539eda7d0ca4ca"
     "9b18d731d2660fd002e10582b206128c0b97eebe2e4a6570db15cb27613f2c86"
     "b1162ee87ca94024dbb677dc40c8d8e5ec02d3ccf505bed683f4aa11604468d0"
     "72ab3be5ff65bead7f537bf684a2369f8977d57b76c7781197e2c58e36a07254"
     "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d"
     "452068f2985179294c73c5964c730a10e62164deed004a8ab68a5d778a2581da"
     "350fef8767e45b0f81dd54c986ee6854857f27067bac88d2b1c2a6fa7fecb522"
     "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0"
     "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8"
     "b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738"
     "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f"
     "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19"
     "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350"
     "3061706fa92759264751c64950df09b285e3a2d3a9db771e99bcbb2f9b470037"
     "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554"
     "b9c002dc827fb75b825da3311935c9f505d48d7ee48f470f0aa7ac5d2a595ab2"
     "36c5acdaf85dda0dad1dd3ad643aacd478fb967960ee1f83981d160c52b3c8ac"
     "df39cc8ecf022613fc2515bccde55df40cb604d7568cb96cd7fe1eff806b863b"
     "e85a354f77ae6c2e47667370a8beddf02e8772a02e1f7edb7089e793f4762a45"
     "b3ba955a30f22fe444831d7bc89f6466b23db8ce87530076d1f1c30505a4c23b"
     "19b62f442479efd3ca4c1cef81c2311579a98bbc0f3684b49cdf9321bd5dfdbf"
     "a3a71b922fb6cbf9283884ac8a9109935e04550bcc5d2a05414a58c52a8ffc47"
     "00d7122017db83578ef6fba39c131efdcb59910f0fac0defbe726da8072a0729"
     "596e4a7606d6cc3d17e10ea3af5e88813244649d85c51c02a81517b61d19073a"
     "cee5c56dc8b95b345bfe1c88d82d48f89e0f23008b0c2154ef452b2ce348da37"
     "59c36051a521e3ea68dc530ded1c7be169cd19e8873b7994bfc02a216041bf3b"
     "fae5872ff90462502b3bedfe689c02d2fa281bc63d33cb007b94a199af6ccf24"
     default))
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((doom-dashboard :url
		     "https://github.com/emacs-dashboard/doom-dashboard.git"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 
