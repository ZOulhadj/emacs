;; =============================================================================

;; Required software
;; tree-sitter, librsvg2-dev, libgccjit0, rg

;; rg (RipGrep) - string searching
;; mpv - music
;; mu, mbsync (MailDir Utils) - mail
;; msync -c ~/.config/mu4e/mbsyncrc -a
;; mu init --maildir=~/Mail --my-address=zakariyaoulhadj01@gmail.com
;; mu index

;; Compiling Emacs from source
;; ./configure --with-native-compilation --with-json --with-pgtk --with-tree-sitter --with-rsvg
;;
;; =============================================================================

;; Use straight.el instead of the built-in package.el for downloading external
;; packages. As we are completely replacing package.el we need to download
;; straight.el without using it. We first create a bootstrap file that will
;; contain the install script and is installed the very first time we launch
;; Emacs.
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
;; is kept "clean" by moving the various different files that get created into
;; specific directories. It is important to note that this package must be
;; installed and activated before other Emacs packages are initialised.
;;
;; https://github.com/emacscollective/no-littering
(use-package no-littering
  :straight t
  :init
  (setq
   no-littering-etc-directory (expand-file-name "tmp/config/" user-emacs-directory)
   no-littering-var-directory (expand-file-name "tmp/data/" user-emacs-directory)))

(use-package emacs
  :config
  (setq
   ;; startup
   gc-cons-threshold 20000000 ;; 20mb
   read-process-output-max (* 16 1024 1024)
   native-comp-async-report-warnings-errors 'silent
   inhibit-startup-message t
   initial-scratch-message nil
   user-full-name "Zakariya Oulhadj"
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
   display-time-24hr-format t
   frame-resize-pixelwise t             ; For seperate frames (C-x 5 2)
   echo-keystrokes 0.02
   use-short-answers t
   frame-title-format "%f"
   isearch-lazy-count t
   
   ;; exiting
   confirm-kill-emacs nil

   ;; other

   
   enable-recursive-minibuffers t
   comint-input-ignoredups t
   default-frame-alist nil
   
   tab-always-indent 'complete
   calendar-date-style "european"
   org-time-stamp-custom-formats '("<%d/%m/%y %a>" . "<%d/%m/%y %a %H:%M>")
   org-display-custom-times t
   
   c-default-style "k&r"
   c-basic-offset 4
   compilation-scroll-output nil

   isearch-wrap-pause 'no ; automatically wrap search without pausing

   read-extended-command-predicate #'command-completion-default-include-p ; hide commands (M-x) that are not supported in the current mode
   vc-follow-symlinks t
   ;;org-agenda-files '("~/Documents/agenda.org")
   
   )
  
  (setq-default
   indent-tabs-mode nil
   dired-listing-switches "-alh"
   fill-column 80
   )
  
  :config
  ;; startup
  ;;(load custom-file 'noerror 'nomessage)

  ;; files
  (save-place-mode 1)
  (recentf-mode 1)
  (savehist-mode)
  (global-auto-revert-mode t)
  (electric-pair-mode 1)
  
  ;; ui
  (tool-bar-mode -1)
  (display-time-mode)
  (show-paren-mode t)
  (scroll-bar-mode -1)
  (global-hl-line-mode 1)
  (blink-cursor-mode 0)
  (toggle-frame-maximized)
  (pixel-scroll-precision-mode 1)
  (load-theme 'modus-vivendi t)
  (toggle-frame-fullscreen)

  ;; other
  (delete-selection-mode t)
    
  ;; Check each font in order and use fallback fonts if current one is not
  ;; found. If none of the specified fonts are found then Emacs will use a
  ;; default font.
  ;;
  ;; Test char and monospace:
  ;; 0123456789abcdefghijklmnopqrstuvwxyz [] () :;,. !@#$^&*
  ;; 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ {} <> "'`  ~-_/|\?
  (set-face-attribute 'default nil :family "Ubuntu Mono" :height 150)

  :bind
  ("C-x k" . kill-this-buffer)

  ;;:hook
  ;;(prog-mode . display-fill-column-indicator-mode)
  )

(use-package isearch
  :bind (:map isearch-mode-map
              ("<backspace>" . isearch-del-char)))

(use-package dabbrev
  :bind
  (("M-/" . dabbrev-completion)
   ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; Adds support for the Lua programming language.
;;
;; https://github.com/immerrr/lua-mode
(use-package lua-mode
  :straight t)

;; The `treesit' package performs fast syntax parsing for languages and allows
;; for other packages to make use of the better context aware functionality.
;;
;; https://github.com/tree-sitter/tree-sitter
(use-package treesit
  :config
  (setq
   treesit-language-source-alist '(
                                   ;; Official grammers
                                   (c "https://github.com/tree-sitter/tree-sitter-c")
                                   (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                                   (rust "https://github.com/tree-sitter/tree-sitter-rust")
                                   (python "https://github.com/tree-sitter/tree-sitter-python")
                                   (bash "https://github.com/tree-sitter/tree-sitter-bash")
                                   (html "https://github.com/tree-sitter/tree-sitter-html")
                                   (css "https://github.com/tree-sitter/tree-sitter-css")
                                   (json "https://github.com/tree-sitter/tree-sitter-json")
                                   
                                   ;; Community grammers
                                   (lua "https://github.com/Azganoth/tree-sitter-lua")
                                   (make "https://github.com/alemuller/tree-sitter-make")
                                   (cmake "https://github.com/uyha/tree-sitter-cmake")
                                   (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
                                   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                                   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                                   (org "https://github.com/milisims/tree-sitter-org")
                                   (glsl "https://github.com/theHamsta/tree-sitter-glsl")
                                   (latex "https://github.com/latex-lsp/tree-sitter-latex")
                                   )
   )
  )

;; Even when Tree sitter is installed and the language grammer is configured,
;; Emacs will not enable it. This is because we must enable the special "ts"
;; modes. So here we remap the default modes to tree-sitter specific modes.
(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (python-mode . python-ts-mode)
        ;; (lua-mode . lua-ts-mode) ;; TODO(zak): No Lua tree sitter mode yet.
        (yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (js2-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (cmake-mode . cmake-ts-mode)))

(setq
 c-ts-mode-indent-offset 4
 c-ts-mode-indent-style "k&r")


;;(add-hook 'help-fns-describe-function-functions #'shortdoc-help-fns-examples-function)
(global-unset-key [mouse-2])

(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-var-directory))
(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-etc-directory))

(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory)))
      auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))



;; The package `diminish' introduces the `:diminish' keyword which can be used
;; together with `use-package' to hide minor modes from the modeline. This
;; allows the modeline to be kept minimal and show only required modes.
;;
;; https://github.com/emacsmirror/diminish
(use-package diminish
  :straight t)

;; A Git client that can be used within Emacs.
;;
;; https://github.com/magit/magit
(use-package magit
  :straight t
  :bind ("C-c g" . magit-status))

;; The package `which-key' displays a popup window showing all the possible key
;; combinations for the current action. This allows a user to not forget
;; specific commands.
;;
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :straight t
  :diminish
  :init (setq which-key-show-early-on-C-h nil
              which-key-idle-delay 1.0
              which-key-idle-secondary-delay nil)
  :config (which-key-mode))


;; The package `exec-path-from-shell' ensures all environment variables are
;; present within Emacs. By default, Emacs only uses a small subset of
;; variables. However, this package works by copying all enviornment variables
;; from the system into Emacs so that all commands are accessible.

;; todo: Take a closer look how this package behaves on Windows since its not
;; Unix based.
;;
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

;; Provides a dashboard/home screen when starting Emacs that lists projects,
;; recent files and more.
;;
;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :straight t
  :init
  (setq
   dashboard-banner-logo-title "Welcome to Emacs!"
   dashboard-set-footer nil
   dashboard-startup-banner 2
   dashboard-center-content nil
   dashboard-show-shortcuts t
   dashboard-set-navigator t
   dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)
                     (registers . 5))
   dashboard-week-agenda t
   ;; dashboard-filter-agenda-entry 'dashboard-no-filter-agenda
   )
  :config
  (dashboard-setup-startup-hook))

;; The package `vertico' provides vertical interactive completion similar to
;; `smex' or the built-in package `ido'.
;;
;; https://github.com/minad/vertico 
(use-package vertico
  :straight t
  :init
  (setq
   vertico-cycle t
   vertico-resize nil
   vertico-count 10)
  (vertico-mode))

;; Adds a small description to each item within the minibuffer completion list.
;;
;; https://github.com/minad/marginalia
(use-package marginalia
  :straight t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; Provides search and navigation commands
;;
;; https://github.com/minad/consult
(use-package consult
  :straight t
  :bind (
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
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-g t" . consult-theme)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ;("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
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

;; Consult users will also want the embark-consult package.
;;
;; 
(use-package embark-consult
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;
;;
;;
(use-package consult-projectile
  :straight t)

;;
;;
;;
(use-package consult-flycheck
  :straight t)

;; The package `projectile' is a project management package that provides many
;; useful features when working with projets such as searching, navigation and
;; editing.
;;
;; https://github.com/bbatsov/projectile
(use-package projectile
  :straight t
  :diminish
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))


;; The package `flycheck' shows syntactic highlighting in code that displays
;; logs, warnings and errors.
;;
;; https://github.com/flycheck/flycheck
(use-package flycheck
  :straight t
  :diminish
  :init (global-flycheck-mode))

;; The package `neotree' shows the filesystem for the current directory.
;;
;; https://github.com/jaypei/emacs-neotree
(use-package neotree
  :straight t
  :config
  (setq
   neo-theme (if (display-graphic-p)
                 'icons 'arrow))
  :bind (("C-c n" . neotree-toggle)))

;; Adds colors to matching brackets based on level
;;
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :straight t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; (use-package consult-lsp
;;   :ensure t)

;; The package `corfu' display a window for autocomplete candidates when writing
;; text. It is a simpler alternative to the highly popular `company'
;; package. This is because it uses the Emacs buit-in
;;
;; https://github.com/minad/corfu
(use-package corfu
  :straight t
  :init
  (setq
   corfu-cycle t
   corfu-auto nil
   corfu-auto-delay 0.2 ; Should not use lower values as this can cause issues
   corfu-separator ?\s
   corfu-quit-at-boundary 'separator
   corfu-quit-no-match t
   corfu-preview-current nil
   corfu-preselect 'valid
   ;;corfu-on-exact-match 'insert
   corfu-scroll-margin 1)
  
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("RET" . nil)))

;; This package changes how completion candidates are displayed within a
;; completion window such as `corfu' or `company'.
;;
;; https://github.com/oantolin/orderless
(use-package orderless
  :straight t
  :init
  (setq
   completion-styles '(orderless partial-completion basic)
   completion-category-defaults nil
   completion-category-overrides nil)
  )

;; The package `lsp-mode' is a front-end to LSP which stands for Language Server
;; Protocol and allows for language parsing, debugging and navigation.
;;
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :straight t
  :custom
  (lsp-completion-provider :none)
  :init
  (setq
   lsp-keymap-prefix "C-c l"
   lsp-headerline-breadcrumb-enable nil
   lsp-enable-symbol-highlighting nil
   lsp-enable-on-type-formatting nil
   lsp-enable-links nil
   lsp-idle-delay 0.1
   lsp-warn-no-matched-clients nil
   lsp-signature-render-documentation nil)
  
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure flex
  :hook
  (prog-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  :commands
  (lsp lsp-deferred))

(use-package dap-mode
  :straight t
  :config
  (require 'dap-cpptools))

;; Adds icon support. Once the package is installed, the actual icons need to be
;; installed manually which can be done using the command `all-the-icons-install-fonts'.
;;
;; https://github.com/iyefrat/all-the-icons.el
(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

;; Adds SVG icons to the `corfu' item dropdown menu. Requires Emacs to be
;; compiled with SVG support (--with-rsvg).
;;
;; https://github.com/jdtsmith/kind-icon
;; (use-package kind-icon
;;   :straight t
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Emacs multimedia system that allows for playing and organising music into
;; different collections/albums. The music is stored locally on the computer.
;;
;; https://github.com/emacsmirror/emms
(use-package emms
  :straight t
  :init (setq
         emms-source-file-default-directory "~/Music")
  :config
  (emms-all)
  (emms-default-players))


;; A Emacs based email client that makes use of mu.
;;
;; https://github.com/djcb/mu
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

;; The package `elfeed' is an RSS client that allows a user to provide a list of
;; RSS sources and the package will retrive the latest news.
;;
;; https://github.com/skeeto/elfeed
(use-package elfeed
  :straight t
  :config (setq
           elfeed-feeds '(("https://www.reddit.com/r/emacs.rss" reddit emacs)))
  :bind (("C-c e" . elfeed)))

(use-package ace-window
  :straight t
  :config
  (setq
   aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 1.0)))))
  :bind
  ("M-o" . ace-window))

(use-package web-mode
  :straight t
  :init (setq web-mode-markup-indent-offset 4
              web-mode-css-indent-offset 4
              web-mode-code-indent-offset 4
              web-mode-indent-style 4
              
              web-mode-enable-auto-pairing t
              web-mode-enable-css-colorization t
              web-mode-enable-current-element-highlight t)
  
  :mode (("\\.html?\\'" . web-mode)
         ("\\.htm?\\'" . web-mode)))

(use-package emmet-mode
  :straight t
  :hook ((web-mode . emmet-mode)
         (sgml-mode . emmet-mode)
         (css-mode . emmet-mode)))

;; (use-package lsp-tailwindcss
;;   :straight t
;;   :init
;;   (setq lsp-tailwindcss-add-on-mode t))


(use-package cmake-mode
  :straight t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :hook (cmake-mode . lsp-deferred))

(use-package cmake-font-lock
  :straight t
  :after cmake-mode
  :config (cmake-font-lock-activate))

;; =============================================================================

(defun custom/load-config ()
  "Load my Emacs init.el configuration file."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))
;;(global-set-key (kbd "C-c c") 'custom/load-config)

;; =============================================================================

;; Unused packages

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
