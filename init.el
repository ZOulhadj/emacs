;; Required software
;; rp (RipGrep) - string searching
;; mpv - music
;; mu, offlineimap (MailDir Utils) - Mail

(defun custom/open-emacs-config ()
  "Load my Emacs init.el configuration file"
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun custom/latex-word-count ()
  (interactive)
  (shell-command (concat "texcount "
                         (buffer-file-name))))

(global-set-key (kbd "C-c c") 'custom/open-emacs-config)
(global-set-key (kbd "C-c w") 'custom/latex-word-count)


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure nil)


;; built-in packages

(use-package cus-edit
  :init
  (setq
   custom-file (locate-user-emacs-file "custom.el")
   )
  :config
  (load custom-file 'noerror 'nomessage)
  )

(use-package startup
  :no-require t
  :init
  (setq
   inhibit-startup-message t
   initial-scratch-message nil
   user-mail-address "zakariyaoulhadj01@gmail.com" ; todo
   )
  )

(use-package window
  :init
  (setq
   split-height-threshold nil
   split-width-threshold 0
   )
  )

(use-package frame
  :config
  (blink-cursor-mode 0)
  )

(use-package scroll-bar
  :config
  (scroll-bar-mode -1)
  )

(use-package hl-line
  :config
  (global-hl-line-mode 0)
  )

(use-package files
  :init
  (setq
   make-backup-files nil
   confirm-kill-emacs 'y-or-n-p
   )
  )


(use-package savehist
  :init
  (savehist-mode))

;; (use-package subword
;;   :ensure nil
;;   :diminish
;;   :hook (prog-mode . 'subword-mode))
  

(use-package autorevert
  :init
  (setq
   global-auto-revert-non-file-buffers t
   )
  :config
  (global-auto-revert-mode t)
  )

(use-package delsel
  :config
  (delete-selection-mode t)
  )

(use-package vc-hooks
  :init
  (setq
   vc-follow-symlinks t
   )
  )

(use-package simple
  :init
  (setq
   column-number-mode t
   read-extended-command-predicate #'command-completion-default-include-p ; hide commands (M-x) that are not supported in the current mode
   )
  )

(use-package isearch
  :init
  (setq
   isearch-wrap-pause 'no ; automatically wrap search
   )
  )

(use-package paren
  :init
  (setq
   show-paren-delay 0.0
   )

  :config
  (show-paren-mode t)
  )

(use-package compile
  :init
  (setq
   compilation-scroll-output nil
   )
  )

(use-package dired
  :init
  (setq-default
   dired-listing-switches "-alh"
   )
  )

(use-package recentf
  :config
  (recentf-mode 1)
  )

(use-package saveplace
  :config
  (save-place-mode 1)
  )

(use-package cc-vars
  :init
  (setq
   c-default-style "k&r"
   c-basic-offset 4
   )
  )

(use-package display-fill-column-indicator
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  )

(use-package org
  :init
  (setq-default
   org-display-custom-times t
   )

  (setq
   org-time-stamp-custom-formats '("<%d/%m/%y %a>" . "<%d/%m/%y %a %H:%M>")
   )
  )

(use-package emacs
  :init
  (setq
   gc-cons-threshold 20000000 ;; 20mb
   read-process-output-max (* 1024 1024)
   create-lockfiles nil
   use-dialog-box nil
   enable-recursive-minibuffers t
   ring-bell-function 'ignore
   default-frame-alist nil
   fill-column 80
   )
  
  (setq-default
   indent-tabs-mode nil
   )
  
  :config
  (tool-bar-mode -1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (set-frame-font "Source Code Pro 14")
  (load-theme 'modus-operandi)
  
  :bind
  ("C-c c" . comment-line)
  ("C-x k" . kill-this-buffer)
  )

(global-unset-key [mouse-2])


;; external packages

(use-package diminish
  :ensure t)

(use-package magit
  :ensure t)

(use-package which-key
  :ensure t
  :diminish
  :init (setq
         which-key-show-early-on-C-h nil
         which-key-idle-delay 1.0
         which-key-idle-secondary-delay nil)
  :config (which-key-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq 
   vertico-cycle t
   vertico-resize nil
   vertico-count 10))

;; Enable vertico-multiform
(vertico-multiform-mode)

;; (use-package vertico-posframe
;;   :init
;;   (setq vertico-posframe-poshandler #'posframe-poshandler-frame-top-center)
;;   :config
;;   (vertico-posframe-mode 0))

(use-package marginalia
  :ensure t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("C-c g" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"
    ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

;; (use-package embark
;;   :ensure t
;;   :bind
;;   (("C-." . embark-act)         ;; pick some comfortable binding
;;    ("C-;" . embark-dwim)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

;;   :init

;;   ;; Optionally replace the key help with a completing-read interface
;;   (setq prefix-help-command #'embark-prefix-help-command)

;;   ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
;;   ;; strategy, if you want to see the documentation from multiple providers.
;;   (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
;;   ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

;;   :config

;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-projectile
  :ensure t)

(use-package projectile
  :ensure t
  :diminish
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)
              ("C-c C-f" . projectile-find-file)))

;; (use-package consult-lsp
;;   :ensure t)

(use-package lsp-mode
  :ensure t
  :init
  (setq
   lsp-keymap-prefix "C-c l"
   lsp-headerline-breadcrumb-enable nil
   lsp-enable-symbol-highlighting nil
   lsp-enable-links nil
   lsp-idle-delay 0.1
   )
  :hook (
         (prog-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  )


(use-package company
  :ensure t
  :diminish
  :config (setq
           company-global-modes '(not text-mode term-mode markdown-mode gfm-mode)
           company-selection-wrap-around t
           company-show-numbers nil
           company-tooltip-align-annotations t
           company-idle-delay 0.0
           company-require-match nil
           company-minimum-prefix-length 2)
  
  :bind (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("<tab>" . company-complete-selection))
  :hook (prog-mode . company-mode)

  )

(use-package emms
  :ensure t
  :config
  (emms-all)
  (emms-default-players)
  )

(use-package smartparens
  :ensure t
  :diminish
  :hook (prog-mode . smartparens-mode)
  )

(use-package flycheck
  :ensure t
  :diminish
  :init (global-flycheck-mode)
  )

(use-package mu4e
  :config
  (setq
   mu4e-maildir-shortcuts '(
                            ("gmail/INBOX" . ?i))
   mu4e-contexts `(
	           ,(make-mu4e-context
	             :name "Gmail Account"
	             :match-func (lambda (msg)
			           (when msg
			             (mu4e-message-contact-field-matches
			              msg '(:from :to :cc :bcc) user-mail-address)))
	             :vars '(
		             (mu4e-trash-folder . "/gmail/[Gmail].Trash")
		             (mu4e-refile-folder . "/gmail/[Gmail].Archive")
		             (mu4e-drafts-folder . "/gmail/[Gmail].Drafts")
		             (mu4e-sent-folder . "/gmail/[Gmail].Sent Mail")
		             (smtpmail-smtp-user . "mu4e.example")
		             (smtpmail-local-domain . "gmail.com")
		             (smtpmail-default-smtp-server . "smtp.gmail.com")
		             (smtpmail-smtp-server . "smtp.gmail.com")
		             (smtpmail-smtp-service . 587)
		             ))
                   )

   )
  )

(use-package mu4e-alert
  :ensure t
  :requires mu4e
  :config
  (mu4e-alert-enable-mode-line-display))

(use-package elfeed
  :ensure t
  :bind
  ("C-c e" . elfeed)
  :config
  (setq
   elfeed-feeds '(
                  ("https://feeds.bbci.co.uk/news/rss.xml?edition=int" news)
                  ("https://www.reddit.com/r/Fedora.rss" reddit))
   )
  )
