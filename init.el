;; -*- lexical-binding: t; -*-

;; EMACS SERVER START
(require 'server)
(unless (server-running-p)
  (server-start))
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
;;(global-display-line-numbers-mode 1)  ;; Show line numbers
(setq confirm-kill-emacs #'y-or-n-p)  ;; Ask before quitting
(setq confirm-kill-processes nil)        ;; Don't ask about background processes
(global-visual-line-mode 1)  ;; Enables word wrap everywhere
(electric-pair-mode 1)  ;; Enable auto-pairing for (), {}, []
(setq electric-pair-preserve-balance t)  ;; Keep pairs balanced
(setq use-dialog-box nil)
(setq doc-view-mode nil)

;; y/n shorthand
(fset 'yes-or-no-p 'y-or-n-p)

;; open file at last edited position
(save-place-mode 1) 

;; recent files list in minbuffer
(recentf-mode 1)
(setq recentf-max-menu-items 15)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; minibuffer memory
(setq history-length 25)
(savehist-mode 1)

(global-display-line-numbers-mode -1)
(global-hl-line-mode t)
(add-hook 'window-setup-hook (lambda () (window-divider-mode -1)))

;; Revert buffers when the file has changed
(global-auto-revert-mode 1)
;; and for dired on creation of files
(setq global-auto-revert-non-file-buffers t)

;; Custom  file location to clean up init.el
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(use-package doom-themes
  :ensure t
  :config

  (load-theme 'doom-feather-dark t))
  ;; (custom-theme-set-faces
  ;;  'doom-peacock
  ;;  '(default ((t (:background "#030118")))))
  ;; (enable-theme 'doom-peacock))

 
(setq doom-feather-brighter-comments t)


;; (set-face-attribute 'hl-line nil
;;                     :background "#4A4A4A"
;;                     :extend t)
(set-face-attribute 'region nil
                    :background "#957FB8"  
                    :distant-foreground nil)
;;(set-face-attribute 'cursor nil :background "#ff0000")
;;(set-face-attribute 'region nil :background "#555577" :extend t)
;;(set-face-background 'hl-line "#111111")

;; transparency
(set-frame-parameter nil 'alpha-background 80)
;;(set-face-attribute 'default nil  :background "#ECF0F1")
(add-to-list 'default-frame-alist '(alpha-background . 80))



(global-set-key (kbd "C-x k") (lambda () (interactive) (kill-buffer (current-buffer))))
(global-set-key (kbd "C-c b") #'previous-buffer)
(global-set-key (kbd "C-c n") #'next-buffer)
(global-set-key (kbd "C-c v") #'mode-line-other-buffer)
(global-set-key (kbd "C-c g") #'comment-line)
;; test comment

;;(set-face-attribute 'default nil :family "Go Mono" :height 90)
(set-face-attribute 'default nil :family "GeistMono Nerd Font" :height 100)
(set-face-attribute 'variable-pitch nil :family "GeistMono Nerd Font":height 140)
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

;; LISP PAIRING MADE EASY
(use-package paredit
  :ensure t
  :hook (after-init . paredit-mode))

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

;;doom modeline
(use-package doom-modeline
  :ensure t
  
  :config
  (doom-modeline-mode)
  (display-time-mode)
  (display-battery-mode)
  (setq doom-modeline-battery t)
  (setq doom-modeline-buffer-encoding -1)
  (setq doom-modeline-percent -1)
  (setq doom-modeline-buffer-file-name-style 'file-name))


(setq user-emacs-directory (expand-file-name "~/.my-emacs/"))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'my-dashboard)
;; go to custom dashboard buffer
(global-set-key (kbd "M-h") (lambda () (interactive) (haize/show-welcome-buffer))) 

;; theme toggle between light and dark
(require 'my-theme-toggle)

;;(add-hook 'window-size-change-functions #'my-dashboard-update-on-resize)
(add-hook 'emacs-startup-hook #'haize/show-welcome-buffer)



(setq initial-buffer-choice t)

(defun my-show-dashboard ()
  (when (display-graphic-p)
    (haize/show-welcome-buffer)))



;; ------------------------
;; Buffer Management (Ibuffer)
;; ------------------------
(global-set-key (kbd "C-x C-b") 'ibuffer)


(use-package ibuffer
  :ensure nil
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-display-summar nil)
  (setq ibuffer-use-other-window nil)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-title-face 'font-loc-doc-face)
  (setq ibuffer-use-header-line t)
  (setq ibuffer-default-shrink-to-minimum-size nil)
  (setq ibuffer-formats
      '((mark modified read-only " "
              (name 30 30 :left :elide)
              " "
              
              (mode 16 16 :left :elide)
	      " " filename-and-process)))
  (setq ibuffer-saved-filter-groups
	'(("Main"
	   ("Directories" (mode . direc-mode))
	   ("C++" ( or (mode .c++-mode)
		    (mode . c++-ts-mode)
		    (mode . c-mode)
		    (mode . j-mode)))
	   ("GLSL" (or
		    (name . "\.glsl$")
		    (name . "\.fs$")
		    (name . "\.vs$")))
	   ("E-Lisp" (mode . emacs-lisp-mode))
	   ("Org" (mode . org-mode))
	   ("Emacs" (or
		     (mode . "^\\*Help\\*$")
		     (mode . "^\\*Welcome\\*$")
		     (mode . "^\\*info\\*$")
		     (mode . "^\\*scratch\\*$")
		     (mode . "^\\*Backtrace\\*$")
		     (mode . "\\*Messages\\*$")))
	   ("Org Agenda" (mode . "^\\*Org Agenda\\*$")))))
  :hook
  (ibuffer-mode . (lambda () (ibuffer-switch-to-saved-filter-groups "Main"))))

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
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'org-modern-mode))

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


;; ----------------------
;; Org Roam Configuration
;; ----------------------


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
  (add-hook 'dired-mode-hook 'org-download-enable 'corfu-mode-disable))

;;  (global-set-key (kbd "M-p")  #'org-download-screenshot)

  




(defun my-add-electric-pairs ()
  "Enable pairing for additional characters."
  (setq-local electric-pair-inhibit-predicate
              (lambda (c) (if (char-equal c ?$) nil (electric-pair-default-inhibit c))))
  (setq-local electric-pair-pairs (append electric-pair-pairs '((?$ . ?$)))))

(add-hook 'org-mode-hook #'my-add-electric-pairs)
(add-hook 'latex-mode-hook #'my-add-electric-pairs)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INLINE ELISP OVERLAY ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package eros
  :config
  (eros-mode 1))

;;;;;;;;;;;;;;;;;;;;;;
;; ORG MODERN ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;
;; (package-install 'org-modern)
(use-package org-modern
  :ensure t
  :hook (org . org-modern))

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

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
  (setq nerd-icons-ibuffer-icon t)
  (setq nerd-icons-ibuffer-color-icon t))

(use-package vertico
  :init (vertico-mode))

(use-package orderless
  :config (setq completion-styles '(orderless)))

(use-package dired
  :ensure nil
  :bind ("C-x d" . dired))

(use-package dired-open
  :ensure t
  :config
  (setq dired-open-extensions `(("pdf" . "zathura"))))



;; ------------------------
;; Lightweight Syntax Highlighting & Auto-completion
;; ------------------------

(add-to-list 'package-selected-packages 'dash)

(use-package go-mode
  :mode "\\.go\\'")
  ;; TYPST MODE
(use-package typst-ts-mode
  :ensure t
  :custom
  (typst-ts-mode-enable-raw-blocks-highlight t)
  :config
  (keymap-set typst-ts-mode-map "C-c C-c" #'typst-ts-tmenu))


;; PYTHON STUFF
(use-package elpy
  :ensure t
  :after (company highlight-indentation pyvenv yasnippet s python)
  :init
  (elpy-enable))

(use-package eglot
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (scheme-mode . eglot-ensure)
		 (typst-ts-mode . eglot-ensure)
		 (js-mode . eglot-ensure))
  :config
  ;; Configure how Eglot displays documentation
  (setq eglot-ignored-server-capabilities '(:hoverProvider)) ;; Disable if unwanted
  (setq eglot-send-changes-idle-time 0.5)  ;; Reduce delay in sending changes
  (setq eglot-autoshutdown t)  ;; Automatically shutdown LSP servers when not needed
  ;; (setq eglot-workspace-configuration
  ;;     '((:python-lsp-server . ((commandPath . "/home/remoosa/disent_env/bin/pyls")))))


  (add-to-list 'eglot-server-programs
               '((c++-mode c-mode) . ("clangd" "--header-insertion=never")))
  (add-to-list 'eglot-server-programs
	       '(typst-ts-mode . ("tinymist")))
  (add-to-list 'eglot-server-programs
	       '((js-mode) . ("typescript-language-server" "--stdio")))
  (add-to-list
   'eglot-server-programs
   `(python-mode
     . ,(eglot-alternatives
         '("/home/haize/venv/bin/pylsp"
           "/ssh:mss:/home-mscluster/remoosa/ICLR/iclr/bin/pylsp"))))
  
  ;; Set up keybindings similar to lsp-mode
  (define-key eglot-mode-map (kbd "M-.") #'xref-find-definitions)  ;; Jump to definition
  (define-key eglot-mode-map (kbd "M-,") #'xref-pop-marker-stack) ;; Jump back
  (define-key eglot-mode-map (kbd "C-c h") #'eldoc-doc-buffer) ;; Show documentation
  (define-key eglot-mode-map (kbd "C-c r") #'eglot-rename) ;; Rename symbol
  (define-key eglot-mode-map (kbd "C-c f") #'eglot-format-buffer) ;; Format buffer
  (define-key eglot-mode-map (kbd "C-c a") #'eglot-code-actions) ;; Code actions

   (with-eval-after-load 'eglot
     (remove-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)))

(use-package yasnippet
  :config (yas-global-mode 1))

;; Optional: Company mode for completion (Eglot integrates with it)
(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (global-company-mode)
  ;;(setq company-backends '(company-capf))
  )

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
;;   (dolist (lang '(c c++ python go scheme js))
;;     (add-to-list 'tree-sitter-major-mode-language-alist
;;                 (cons lang lang))))
;; ;; Optional: Set up LSP (for enhanced completion and features) without Emacs Lisp

 

;; (use-package lsp-mode
;;    :ensure t
;;    :hook ((c-mode . lsp)
;;           (cpp-mode . lsp)
;;           (python-mode . lsp)
;;           (go-mode . lsp)
;;           (scheme-mode . lsp)
;;           (js-mode . lsp))
;;    :commands lsp)

;; (use-package lsp-ui
;;   :ensure t
;;   :after lsp-mode
;;   :config
;;   (setq lsp-ui-doc-enable t
;;         lsp-ui-doc-position 'at-point))


;;------------ VISUALS --------------
(use-package spacious-padding
  :ensure t
  :config
  (spacious-padding-mode 0))
;; ------------------------
;;; THEMES
;; ------------------------

;;Some themes I like :
;; snow-theme
;; snowish-theme
;;
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
;;   :ensure t
;;   :config 
;;   (load-theme 'doom-ayu-light t))

;; (use-package apropospriate-theme
;;   :ensure t
;;   :config 
;;   (load-theme 'apropospriate-light t))


;; last used feather dark

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
;; (use-package emacs-solo-mode-line
;;   :ensure nil
;;   :no-require t
;;   :defer t
;;   :init
;;   ;; Shorten big branches names
;;   (defun emacs-solo/shorten-vc-mode (vc)
;;     "Shorten VC string to at most 20 characters.qq
;;  Replacing `Git-' with a branch symbol."
;;     (let* ((vc (replace-regexp-in-string "^ Git[:-]" "  " vc))) ;; Options:   ᚠ ⎇
;;       (if (> (length vc) 20)
;;           (concat (substring vc 0 20) "…")
;;         vc)))

;;   ;; Formats Modeline
;;   (setq-default mode-line-format
;;                 '("%e" "  "
;;                   ;; (:propertize " " display (raise +0.1)) ;; Top padding
;;                   ;; (:propertize " " display (raise -0.1)) ;; Bottom padding
;;                   (:propertize "λ  " face font-lock-keyword-face)

;;                   (:propertize
;;                    ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))

;;                   mode-line-frame-identification
;;                   mode-line-buffer-identification
;;                   "   "
;;                   mode-line-position
;;                   mode-line-format-right-align
;; 		  (display-time-mode t)
;; 		  "  "
;; 		  (display-battery-mode t)
;;                   "  "
;;                   (project-mode-line project-mode-line-format)
;;                   "  "
;;                   (vc-mode (:eval (emacs-solo/shorten-vc-mode vc-mode)))
;;                   "  "
;;                   ;;mode-line-modes
;;                   ;;mode-line-misc-info
;;                   "  ")
;;                 project-mode-line t
;;                 mode-line-buffer-identification '(" %b")
;;                 mode-line-position-column-line-format '(" %l:%c"))

;;   ;; Provides the Diminish functionality
;;   (defvar emacs-solo-hidden-minor-modes
;;     '(abbrev-mode
;;       eldoc-mode
;;       flyspell-mode
;;       smooth-scroll-mode
;;       outline-minor-mode
;;       which-key-mode))

;;   (defun emacs-solo/purge-minor-modes ()
;;     (interactive)
;;     (dolist (x emacs-solo-hidden-minor-modes nil)
;;       (let ((trg (cdr (assoc x minor-mode-alist))))
;;         (when trg
;;           (setcar trg "")))))

;;   (add-hook 'after-change-major-mode-hook 'emacs-solo/purge-minor-modes))

;; ---------- EMACS-SOLO-OLIVETTI
(use-package emacs-solo-olivetti
  :ensure nil
  :no-require t
  :defer t
  :init
  (defvar emacs-solo-center-document-desired-width 120
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
