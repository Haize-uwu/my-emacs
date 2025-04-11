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
;; ------------------------
(setq auto-save-default t
      make-backup-files nil)
;;(setq display-line-numbers 'relative)
(global-display-line-numbers-mode 1)  ;; Show line numbers
(setq confirm-kill-emacs #'y-or-n-p)  ;; Ask before quitting
(setq confirm-kill-processes nil)        ;; Don't ask about background processes
(global-visual-line-mode 1)  ;; Enables word wrap everywhere
(electric-pair-mode 1)  ;; Enable auto-pairing for (), {}, []
(setq electric-pair-preserve-balance t)  ;; Keep pairs balanced

;; open file at last edited position
(save-place-mode 1)

;; recent files list in minbuffer
(recentf-mode 1)
(setq recentf-max-menu-items 15)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(set-face-attribute 'default nil :family "GeistMono Nerd Font" :height 130)

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
    (defun insert-centered (text)
      (insert (make-string  left-padding ?\s) text "\n"))

    (insert "\n\n\n\n")
    (insert-centered "███████╗███╗   ███╗ █████╗  ██████╗███████╗")
    (insert-centered "██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝")
    (insert-centered "█████╗  ██╔████╔██║███████║██║     ███████╗")
    (insert-centered "██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║")
    (insert-centered "███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║")
    (insert-centered "╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝")
    (insert "\n\n")
    (insert-centered (format "\tWelcome to Emacs, %s!\n" user-login-name))
    (insert-centered (format "\tLoading time : %s" (emacs-init-time)))
    (insert-centered (format "\tPackages     : %s" (length package-activated-list)))
    (insert "\n\n")


    (insert-centered (format "\tEmacs version: %s" emacs-version))
    (insert "\n")
    
    
    (insert "\n\n"))

  (read-only-mode 1)   ;; Make buffer read-only again
  (goto-char (point-min)))

(add-hook 'emacs-startup-hook #'my-dashboard)
;;(add-hook 'dashboard-mode-hook (lambda () (display-line-numbers-mode -1)))

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

  ;; Automatically create the directory if it doesn't exist
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local org-download-image-dir
                          (concat (file-name-directory (buffer-file-name)) "images/"))))

  ;; Enable org-download in Dired mode
  (add-hook 'dired-mode-hook 'org-download-enable))


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
;; (use-package modus-themes
;;   :ensure nil
;;   :defer t
;;   :custom
;;   (modus-themes-italic-constructs t)
;;   (modus-themes-bold-constructs t)
;;   (modus-themes-mixed-fonts nil)
;;   (modus-themes-prompts '(bold intense))
;;   (modus-themes-common-palette-overrides
;;    `((bg-main "#292D3E")
;;      (bg-active bg-main)
;;      (fg-main "#EEFFFF")
;;      (fg-active fg-main)
;;      (fg-mode-line-active "#A6Accd")
;;      (bg-mode-line-active "#232635")
;;      (fg-mode-line-inactive "#676E95")
;;      (bg-mode-line-inactive "#282c3d")
;;      ;; (border-mode-line-active "#676E95")
;;      ;; (border-mode-line-inactive bg-dim)
;;      (border-mode-line-active nil)
;;      (border-mode-line-inactive nil)
;;      (bg-tab-bar      "#242837")
;;      (bg-tab-current  bg-main)
;;      (bg-tab-other    "#242837")
;;      (fg-prompt "#c792ea")
;;      (bg-prompt unspecified)
;;      (bg-hover-secondary "#676E95")
;;      (bg-completion "#2f447f")
;;      (fg-completion white)
;;      (bg-region "#3C435E")
;;      (fg-region white)

;;      (fg-line-number-active fg-main)
;;      (fg-line-number-inactive "gray50")
;;      (bg-line-number-active unspecified)
;;      (bg-line-number-inactive "#292D3E")
;;      (fringe "#292D3E")

;;      (fg-heading-0 "#82aaff")
;;      (fg-heading-1 "#82aaff")
;;      (fg-heading-2 "#c792ea")
;;      (fg-heading-3 "#bb80b3")
;;      (fg-heading-4 "#a1bfff")

;;      (fg-prose-verbatim "#c3e88d")
;;      (bg-prose-block-contents "#232635")
;;      (fg-prose-block-delimiter "#676E95")
;;      (bg-prose-block-delimiter bg-prose-block-contents)

;;      (accent-1 "#79a8ff")

;;      (keyword "#89DDFF")
;;      (builtin "#82aaff")
;;      (comment "#676E95")
;;      (string "#c3e88d")
;;      (fnname "#82aaff")
;;      (type "#c792ea")
;;      (variable "#c792ea")
;;      (docstring "#8d92af")
;;      (constant "#f78c6c")))
;;   :config
;;   (modus-themes-with-colors
;;    (custom-set-faces
;;     `(tab-bar
;;       ((,c
;;         :background "#232635"
;;         :foreground "#A6Accd"
;;         ;; :box (:line-width 1 :color "#676E95")
;;         )))
;;     `(tab-bar-tab
;;       ((,c
;;         ;; :background "#232635"
;;         ;; :underline t
;;         ;; :box (:line-width 1 :color "#676E95")
;;         )))
;;     `(tab-bar-tab-inactive
;;       ((,c
;;         ;; :background "#232635"
;;         ;; :box (:line-width 1 :color "#676E95")
;;         )))))
;;   :init
;;   (load-theme 'modus-vivendi-tinted t))


;;spaceway themes
(use-package spaceway-theme
  :ensure nil
  :load-path "lisp/spaceway/"
  :config
  (global-hl-line-mode t)
  (set-frame-parameter nil 'cursor-color "#dc322f")
  (add-to-list 'default-frame-alist '(cursor-color . "#dc322f")))
  
(use-package ef-themes
  :ensure t)
(use-package doom-themes
  :ensure t)
(setq custom-safe-themes t)

(load-theme 'spaceway t)
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
    "Shorten VC string to at most 20 characters.
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
                  mode-line-modes
                  mode-line-misc-info
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
 '(custom-enabled-themes '(spaceway))
 '(custom-safe-themes
   '("72ab3be5ff65bead7f537bf684a2369f8977d57b76c7781197e2c58e36a07254"
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
 '(package-selected-packages
   '(citar color-themes corfu dash dash-functional doom-themes ef-themes
	   lsp-mode lsp-ui magit orderless org org-bullets
	   org-download org-modern projectile python-mode
	   rainbow-delimiters sweet-theme tree-sitter-langs vertico
	   yasnippet)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
