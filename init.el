; Required software
; rp (RipGrep) - string searching

; mpv - music

; mu, mbsync (MailDir Utils) - mail
; msync -c ~/.config/mu4e/mbsyncrc -a
; mu init --maildir=~/Mail --my-address=zakariyaoulhadj01@gmail.com
; mu index

;; Use straight.el instead of the built-in package.el for downloading external
;; packages. As we are completely replacing package.el we need to download
;; straight.el without using it. We first create a bootstrap file that will contain
;; the install script and is installed the very first time we launch Emacs.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default nil)


;; The package `no-littering' ensures that the `user-emacs-directory' location
;; is kept "clean" by moving various different files that get created into
;; specific directories. It is important to note that this package must be
;; installed and activated before other Emacs packages are initialised.
(use-package no-littering
  :straight t
  :init
  (setq no-littering-etc-directory (expand-file-name "tmp/config/" user-emacs-directory)
        no-littering-var-directory (expand-file-name "tmp/data/" user-emacs-directory)))

(use-package emacs
  :init
  (setq
   ;; startup
   gc-cons-threshold 20000000 ;; 20mb
   read-process-output-max (* 1024 1024)
   native-comp-async-report-warnings-errors nil
   inhibit-startup-message t
   initial-scratch-message nil
   user-mail-address "zakariyaoulhadj01@gmail.com"
   
   
   ;; files
   custom-file (locate-user-emacs-file "custom.el")
   create-lockfiles nil
   make-backup-files t
   global-auto-revert-non-file-buffers t
   
   ;; ui
   use-dialog-box nil
   column-number-mode t
   show-paren-delay 0.0
   ring-bell-function 'ignore
   display-time-default-load-average nil
   
   ;; exiting
   confirm-kill-emacs nil ; 'y-or-n-p

   ;; other
   fill-column 80
   
   enable-recursive-minibuffers t
   
   default-frame-alist nil
   
   tab-always-indent 'complete
   calendar-date-style "european"
   org-time-stamp-formats '("<%d/%m/%y %a>" . "<%d/%m/%y %a %H:%M>")
   c-default-style "k&r"
   c-basic-offset 4
   compilation-scroll-output nil

   isearch-wrap-pause 'no ; automatically wrap search

   read-extended-command-predicate #'command-completion-default-include-p ; hide commands (M-x) that are not supported in the current mode
   vc-follow-symlinks t

   split-height-threshold nil
   split-width-threshold 0
   )
  
  (setq-default
   indent-tabs-mode nil
   org-display-custom-times t
   dired-listing-switches "-alh"
   )
  
  :config
  ;; startup
  ;;(load custom-file 'noerror 'nomessage)

  ;; files
  (save-place-mode 1)
  (recentf-mode 1)
  (global-auto-revert-mode t)
  (savehist-mode)
  
  ;; ui
  (tool-bar-mode -1)
  (load-theme 'modus-operandi)
  (display-time-mode)
  (show-paren-mode t)
  (scroll-bar-mode -1)
  (global-hl-line-mode 0)
  (blink-cursor-mode 0)

  ;; other
  (fset 'yes-or-no-p 'y-or-n-p)
  (delete-selection-mode t)
    
  ;; Check each font in order and use fallback fonts if current one is
  ;; not found. If none of the specified fonts are found then Emacs
  ;; will use a default font.
  ;;
  ;; Test char and monospace:
  ;; 0123456789abcdefghijklmnopqrstuvwxyz [] () :;,. !@#$^&*
  ;; 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ {} <> "'`  ~-_/|\?
  (cond
   ((find-font (font-spec :name "Cascadia Mono"))
    (set-frame-font "Cascadia Mono-16"))
   ((find-font (font-spec :name "Consolas"))
    (set-frame-font "Consolas-16")))

  :bind
  ("C-x k" . kill-this-buffer)
  ("C-c t" . shell)

  :hook
  (prog-mode . display-fill-column-indicator-mode))

(global-unset-key [mouse-2])

(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-var-directory))
(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-etc-directory))

(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq
 backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory)))
 
 auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
 auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t))
 )


;; The package `diminish' introduces the `:diminish' keyword which can
;; be used together with `use-package' to hide minor modes from the
;; modeline. This allows the modeline to be kept minimal and show only
;; required information.
(use-package diminish
  :straight t)

(use-package magit
  :straight t)

;; The package `which-key' displays a popup window showing all the
;; possible key combinations for the current action. This allows a
;; user to not forget specific commands.
(use-package which-key
  :straight t
  :diminish
  :init (setq which-key-show-early-on-C-h nil
              which-key-idle-delay 1.0
              which-key-idle-secondary-delay nil)
  :config (which-key-mode))


;; The package `exec-path-from-shell' ensures all environment
;; variables are present within Emacs. By default, Emacs only uses a
;; small subset of variables. However, this package works by copying
;; all enviornment variables from the system into Emacs so that all
;; commands are accessible.
(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq
   completion-styles '(orderless basic)
   completion-category-defaults nil
   completion-category-overrides '((file (styles partial-completion)))
   )
  )

(use-package vertico
  :straight t
  :init
  (setq 
   vertico-cycle t
   vertico-resize nil
   vertico-count 10)
  (vertico-mode)
  )

;; Enable vertico-multiform
;;(vertico-multiform-mode)

;; (use-package vertico-posframe
;;   :straight t
;;   :init
;;   (setq vertico-posframe-poshandler #'posframe-poshandler-frame-top-center)
;;   :config
;;   (vertico-posframe-mode 0))

(use-package marginalia
  :straight t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package consult
  :straight t
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
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-projectile
  :straight t)

;; The package `projectile' is a project management package that provides
;; many useful features when working with projets such as searching,
;; navigation and editing.
(use-package projectile
  :straight t
  :diminish
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)
              ("C-c C-f" . projectile-find-file)))

;; (use-package consult-lsp
;;   :ensure t)


;; The package `lsp-mode' is a front-end to LSP which stands for
;; Language Server Protocol and allows for language parsing, debugging
;; and navigation.
(use-package lsp-mode
  :straight t
  :init
  (setq
   lsp-keymap-prefix "C-c l"
   lsp-headerline-breadcrumb-enable nil
   lsp-enable-symbol-highlighting nil
   lsp-enable-links nil
   lsp-idle-delay 0.1
   lsp-warn-no-matched-clients nil
   )
  :hook (
         (prog-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  )


;; (use-package company
;;   :ensure t
;;   :diminish
;;   :config (setq
;;            company-global-modes '(not text-mode term-mode markdown-mode gfm-mode)
;;            company-selection-wrap-around t
;;            company-show-numbers nil
;;            company-tooltip-align-annotations t
;;            company-idle-delay 0.0
;;            company-require-match nil
;;            company-minimum-prefix-length 2)
  
;;   :bind (:map company-active-map
;;         ("C-n" . company-select-next)
;;         ("C-p" . company-select-previous)
;;         ("<tab>" . company-complete-selection))
;;   :hook (prog-mode . company-mode)

;;   )

;; The package `corfu' display a window for autocomplete candidates
;; when writing text. It is a simpler alternative to the highly
;; popular `company' package.
(use-package corfu
  :straight t
  :init
  (setq
   corfu-cycle nil
   corfu-auto t
   corfu-separator ?\s
   corfu-quit-at-boundary 'separator
   corfu-quit-no-match t
   corfu-preview-current nil
   corfu-preselect 'valid
   corfu-on-exact-match 'insert
   corfu-scroll-margin 2
   )
  (global-corfu-mode)
  :bind (:map corfu-map
              ("RET" . nil)
              )
  )


(use-package emms
  :straight t
  :config
  (emms-all)
  (emms-default-players)
  )

(use-package smartparens
  :straight t
  :diminish
  :hook (prog-mode . smartparens-mode)
  )

;; The package `flycheck' shows syntactic highlighting in code that
;; displays information, warning and errors.
(use-package flycheck
  :straight t
  :diminish
  ;; :init (global-flycheck-mode)
  )

;; The package `neotree' is a window that shows the filesystem for the
;; current project or directory.
(use-package neotree
  :straight t
  :bind
  ("C-c n" . neotree-toggle)
  )

;; (use-package mu4e
;;   :straight t
;;   :config
;;   (setq
;;    mu4e-get-mail-command "mbsync -c ~/.config/mu4e/mbsyncrc -a"
;;    mu4e-update-interval 300
;;    message-send-mail-function 'smtpmail-send-it
;;    starttls-use-gnutls t
;;    smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;    smtpmail-auth-credentials '(("smtp.gmail.com" 587 "zakariyaoulhadj01@gmail.com" nil))
;;    smtpmail-default-smtp-server "smtp.gmail.com"
;;    smtpmail-smtp-server "smtp.gmail.com"
;;    smtpmail-smtp-service 587
;;    mu4e-maildir-shortcuts '(("/gmail/Inbox" . ?i)
;;                             ("/gmail/Sent" . ?s)
;;                             ("/gmail/All Mail" . ?a)
;;                             ("/gmail/Trash" . ?t)
;;                             ("/gmail/Drafts" . ?d))
;;    mu4e-sent-folder "/gmail/Sent"
;;    mu4e-drafts-folder "/gmail/Drafts"
;;    mu4e-trash-folder "/gmail/Trash"
;;    mu4e-refile-folder "/gmail/All Mail"
;;    )
;;   :bind
;;   ("C-c m" . mu4e)
;;   )

(use-package mu4e-alert
  :straight t
  :requires mu4e
  :config
  (mu4e-alert-enable-mode-line-display))

;; The package `elfeed' is an RSS client that allows a user to provide
;; a list of RSS sources and the package will retrive the latest news.
(use-package elfeed
  :straight t
  :config
  (setq elfeed-feeds '())
  :bind
  ("C-c e" . elfeed))

;; (use-package ace-window
;;   :ensure t
;;   :bind
;;   ("C-x o" . ace-window))

(use-package web-mode
  :straight t
  :init (setq web-mode-markup-indent-offset 4
              web-mode-css-indent-offset 4
              web-mode-code-indent-offset 4

              web-mode-enable-auto-pairing t
              web-mode-enable-css-colorization t
              web-mode-enable-current-element-highlight t)
  :mode (("\\.html?\\'" . web-mode)
         ("\\.htm?\\'" . web-mode)))

(defun custom/load-config ()
  "Load my Emacs init.el configuration file"
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))
;;(global-set-key (kbd "C-c c") 'custom/load-config)

(defun custom/latex-word-count ()
  (interactive)
  (shell-command (concat "texcount "
                         (buffer-file-name))))
(global-set-key (kbd "C-c w") 'custom/latex-word-count)
