;; init.el --- Init File -*- lexical-binding: t -*-

;; Use `straight.el' instead of the built-in `package.el' for downloading
;; external packages. As we are completely replacing `package.el' we need to
;; download `straight.el' without using it. We first create a bootstrap file
;; that will contain the install script and is installed the very first time we
;; launch Emacs.
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

(use-package straight
  :custom
  (straight-use-package-by-default nil))

(use-package use-package-core
  :init
  (setopt use-package-verbose t
          use-package-compute-statistics t
          use-package-hook-name-suffix nil
          use-package-always-demand t))

(use-package diminish
  :straight t)

(use-package no-littering
  :straight t
  :init
  (setopt no-littering-etc-directory (expand-file-name "data/etc/" user-emacs-directory)
          no-littering-var-directory (expand-file-name "data/var/" user-emacs-directory)))

(use-package emacs
  :bind
  ("<mouse-2>" . nil)
  ("<mouse-3>" . nil)
  ("C-x k" . kill-current-buffer)
  ("C-c r" . recompile)
  :init
  (setopt read-process-output-max (* 1024 1024)
          load-prefer-newer t
          create-lockfiles nil
          history-length 1000
          history-delete-duplicates t
          use-dialog-box nil
          ring-bell-function 'ignore
          frame-resize-pixelwise t             ; For seperate frames (C-x 5 2)
          echo-keystrokes 0.02
          use-short-answers t
          frame-title-format '("" (:eval (if (buffer-file-name)
                                             (abbreviate-file-name (buffer-file-name))
                                           "%b")))
          enable-recursive-minibuffers t
          initial-scratch-message nil
          inhibit-startup-echo-area-message user-login-name
          inhibit-startup-screen t

          tab-always-indent 'complete
          delete-by-moving-to-trash t
          user-full-name "Zakariya Oulhadj"
          user-mail-address "zakariyaoulhadj01@gmail.com"

          sentence-end-double-space nil
          fill-column 80
          scroll-preserve-screen-position 1
          scroll-margin 0
          scroll-conservatively 10000)
  (setq-default mode-line-format
                '("%e" mode-line-front-space
                  (:propertize
                   ("" mode-line-mule-info mode-line-client mode-line-modified
                    mode-line-remote)
                   display (min-width (5.0)))
                  mode-line-frame-identification mode-line-buffer-identification "   "
                  mode-line-position (vc-mode vc-mode) "  " mode-line-modes
                  mode-line-misc-info mode-line-end-spaces))

  ;; Disable bindings for suspending Emacs in graphical mode
  (when (display-graphic-p)
    (global-unset-key (kbd "C-z"))
    (global-unset-key (kbd "C-x C-z")))

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (menu-bar-mode -1)
  :config
  (set-default-coding-systems 'utf-8)
  (put 'narrow-to-region 'disabled nil)

  (set-face-attribute 'font-lock-comment-face nil
                      :foreground "#49503d"
                      :slant 'italic)
  :custom
  (text-mode-ispell-word-completion nil))

(defun zo/config-load ()
  "Load my Emacs configuration."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(use-package help
  :config
  (setopt help-window-select t))

(use-package comp
  :init
  (setopt native-comp-async-report-warnings-errors 'silent))

(use-package custom
  :init
  (setopt custom-safe-themes t)
  :config
  (setopt custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file :no-error-if-file-is-missing))

(defun prev-window ()
  (interactive)
  (other-window -1))

(use-package window
  :bind
  ("C-," . prev-window)
  ("C-." . other-window)
  ("C-x o" . other-window)

  ("C-1" . delete-other-windows)
  ("C-2" . split-window-below)
  ("C-3" . split-window-right)
  ("C-0" . delete-window)

  ;; These bindings make it easier dealing with windows in `god-mode'.
  ;; @TODO: Only enable these bindings when god-mode is active.
  ("C-x C-1" . delete-other-windows)
  ("C-x C-2" . split-window-below)
  ("C-x C-3" . split-window-right)
  ("C-x C-0" . delete-window))

(use-package frame
  :config
  ;;(toggle-frame-maximized)
  ;;(toggle-frame-fullscreen)
  )

(use-package fringe
  :config
  (fringe-mode '(8 . 0)))

(use-package hl-line
  :config
  (global-hl-line-mode 1))

(use-package which-key
  :init
  (setopt which-key-show-early-on-C-h nil
          which-key-idle-delay 1.0
          which-key-idle-secondary-delay nil)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode)
  :diminish)

(use-package subword
  :hook
  (prog-mode . subword-mode))

(use-package compile
  :bind
  ("<f5>" . recompile)
  :hook
  (compilation-mode-hook . visual-line-mode)
  :init
  (setopt compilation-always-kill t
          compilation-scroll-output 'first-error ;; t
          compilation-ask-about-save nil
          compilation-auto-jump-to-first-error t
          compilation-max-output-line-length nil
          compilation-finish-functions
          (list (lambda (buf status)
                  (when (string-match-p "finished" status)
                    (run-at-time 0.5 nil
                                 (lambda (b)
                                   (let ((win (get-buffer-window b)))
                                     (when win
                                       (quit-window nil win))))
                                 buf))))))

(use-package ansi-color
  :hook
  (compilation-filter-hook . ansi-color-compilation-filter))

(use-package flymake
  :config
  (setopt flymake-fringe-indicator-position nil))

(use-package comint
  :init
  (setopt comint-input-ignoredups t
          comint-process-echoes t))

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package calendar
  :custom
  (calendar-week-start-day 1)
  (calendar-date-style 'european))

(use-package vc-hooks
  :init
  (setopt vc-follow-symlinks t))

(use-package files
  :init
  (setopt large-file-warning-threshold 100000000 ; 100 MB
          make-backup-files nil
          require-final-newline t
          delete-old-versions t
          confirm-kill-emacs nil))

(use-package delsel
  :config
  (delete-selection-mode t))

(use-package saveplace
  :init
  (setopt save-place-limit 500)
  :config
  (save-place-mode 1))

(use-package savehist
  :init
  (setopt savehist-additional-variables '(search-ring regexp-search-ring)
          savehist-autosave-interval 60)
  :config
  (savehist-mode +1))

(use-package desktop
  :init
  ;; @TODO: For some reason dirname is not set?
  (setopt desktop-dirname (expand-file-name "data/var/desktop/" user-emacs-directory)
          desktop-load-locked-desktop t
          desktop-auto-save-timeout 30
          desktop-save t)
  :config
  (desktop-save-mode 1))

(use-package autorevert
  :init
  (setopt global-auto-revert-non-file-buffers t
          auto-revert-verbose nil)
  :config
  (global-auto-revert-mode t))

(use-package simple
  :bind (:map global-map
              ("RET" . newline-and-indent))
  :init
  (setopt read-extended-command-predicate #'command-completion-default-include-p ; hide commands (M-x) that are not supported in the current mode)
          column-number-mode t
          next-line-add-newlines nil
          kill-do-not-save-duplicates t)
  (setq-default indent-tabs-mode nil
                tab-width 4
                standard-indent 4
                indent-line-function #'indent-relative-first-indent-point)
  :config
  (visual-line-mode -1)
  (size-indication-mode 1)
  (set-default 'truncate-lines t))

(use-package time
  :init
  (setopt display-time-24hr-format t)
  :config
  (display-time-mode -1)
  :custom
  (display-time-default-load-average nil))

;; (use-package display-line-numbers
;;   :init
;;   (setopt display-line-numbers-type 'visual)
;;   :hook
;;   (prog-mode-hook . display-line-numbers-mode))

(use-package which-func
  :hook
  (prog-mode-hook . which-function-mode))

(use-package elec-pair
  :config
  (electric-pair-mode 1))

(use-package paren
  :init
  (setopt show-paren-delay 0.125)
  :config
  (show-paren-mode 1))

(use-package recentf
  :init
  (setopt recentf-max-saved-items 500
          recentf-max-menu-items 15
          ;; disable recentf-cleanup on emacs start, because it can cause
          ;; problems with remote files (prelude)
          recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1)
  :custom
  ;; exclude all of files in the no-littering directories from recentf.
  (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-etc-directory)))

(use-package windmove
  :config
  (windmove-default-keybindings))

;; todo: only enable if aspell is installed
;; maybe we can use :ensure-system-package
(use-package flyspell
  :disabled ;; @TODO: C-, conflicts with prev-window
  :init
  (setopt ispell-program-name "aspell"
          ispell-extra-args '("--sug-mode=ultra"))
  :hook
  (prog-mode-hook . flyspell-prog-mode))

(use-package isearch
  :bind
  (:map isearch-mode-map ("<backspace>" . isearch-del-char))
  :config
  (setopt isearch-wrap-pause 'no-ding
          isearch-lazy-count t
          isearch-allow-scroll 'unlimited
          lazy-highlight-cleanup t
          lazy-highlight-buffer t))

(use-package dired
  :bind (:map dired-mode-map
              ([mouse-2] . dired-mouse-find-file))
  :init
  ;; @TODO: on macOS --group-directories-first is not supported.
  ;; See: https://github.com/d12frosted/homebrew-emacs-plus/issues/383#issuecomment-899157143
  (setq-default
   dired-listing-switches "-l --all --no-group --human-readable --group-directories-first --time-style=long-iso")
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setopt dired-recursive-copies 'always
          dired-recursive-deletes 'always
          dired-kill-when-opening-new-dired-buffer t
          dired-hide-details-hide-symlink-targets nil
          dired-dwim-target t
          dired-auto-revert-buffer t))

(use-package org
  :init
                                        ;(setq-default org-display-custom-times t)
  (setopt
   org-directory "~/notes"
   org-agenda-files '("~/notes/personal.org")
   org-agenda-start-with-log-mode '(closed clock)
   org-log-done 'time
   org-log-into-drawer t
   org-startup-indented nil
   org-startup-with-inline-images t
   org-display-remote-inline-images 'download
                                        ;org-time-stamp-custom-formats '("<%d/%m/%y %a>" . "<%d/%m/%y %a %h:%m>")
   org-return-follows-link t
   org-hide-emphasis-markers t)
                                        ;org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  ;;org-export-backends '(html md))
  :hook
  (org-mode-hook . turn-on-auto-fill))

(use-package org-capture
  :bind (("C-c c" . org-capture))
  :init
  (setopt org-default-notes-file "~/notes/personal.org")
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/notes/personal.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/notes/personal.org")
           "* %?\nEntered on %U\n  %i\n  %a"))))

(use-package org-agenda
  :bind
  ("C-c a" . org-agenda))

(use-package org-clock
  :init
  (setopt
   org-clock-persist 'history
   org-clock-idle-time 15)
  :config
  (org-clock-persistence-insinuate))

(use-package gnus
  :init
  (setopt gnus-select-method '(nnnil "")
          gnus-secondary-select-methods '((nnml ""))
          gnus-summary-display-arrow nil
          gnus-auto-select-first nil
          gnus-thread-hide-subtree t))


;; (const :tag "Documentation on hover" :hoverProvider)
;; (const :tag "Code completion" :completionProvider)
;; (const :tag "Function signature help" :signatureHelpProvider)
;; (const :tag "Go to definition" :definitionProvider)
;; (const :tag "Go to type definition" :typeDefinitionProvider)
;; (const :tag "Go to implementation" :implementationProvider)
;; (const :tag "Go to declaration" :declarationProvider)
;; (const :tag "Find references" :referencesProvider)
;; (const :tag "Highlight symbols automatically" :documentHighlightProvider)
;; (const :tag "List symbols in buffer" :documentSymbolProvider)
;; (const :tag "List symbols in workspace" :workspaceSymbolProvider)
;; (const :tag "Execute code actions" :codeActionProvider)
;; (const :tag "Code lens" :codeLensProvider)
;; (const :tag "Format buffer" :documentFormattingProvider)
;; (const :tag "Format portion of buffer" :documentRangeFormattingProvider)
;; (const :tag "On-type formatting" :documentOnTypeFormattingProvider)
;; (const :tag "Rename symbol" :renameProvider)
;; (const :tag "Highlight links in document" :documentLinkProvider)
;; (const :tag "Decorate color references" :colorProvider)
;; (const :tag "Fold regions of buffer" :foldingRangeProvider)
;; (const :tag "Execute custom commands" :executeCommandProvider)
;; (const :tag "Inlay hints" :inlayHintProvider)

(use-package eglot
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities
   '(:hoverProvider
     :documentHighlightProvider
     :documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     ;;:colorProvider
     :foldingRangeProvider
     :inlayHintProvider
     ))
  ;; (setq-default eglot-inlay-hints-mode -1)
  ;; (eldoc-echo-area-use-multiline-p nil)

  :config
  (add-to-list 'eglot-server-programs
               `(zig-mode . ("zls")))
  ;; (setq-default eglot-workspace-configuration
  ;;               '((:zls . (:enable_autofix t))))
  ;; (add-hook 'zig-mode-hook
  ;;           (lambda ()
  ;;             (add-hook 'before-save-hook
  ;;                       (lambda ()
  ;;                         (when (eglot-managed-p)
  ;;                           (eglot-code-actions nil nil "source.fixAll" t)))
  ;;                       nil t)))
  :hook
  ((c-ts-mode-hook . eglot-ensure)
   (c++-ts-mode-hook . eglot-ensure)
   (python-ts-mode-hook . eglot-ensure)
   (zig-mode-hook . eglot-ensure)))

(use-package erc
  :disabled
  :init
  (setopt erc-server "irc.libera.chat"
          erc-nick "zoulhadj"
          erc-user-full-name "Zakariya Oulhadj"
          erc-track-shorten-start 8
          erc-autojoin-channels-alist '(("irc.libera.chat" "#linux" "#emacs"))
          erc-kill-buffer-on-part t
          erc-auto-query 'bury))

;; /////////////////////////////////////////////////////////////////////////////


(use-package keyfreq
  :disabled
  :straight t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package dired-subtree
  :straight t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setopt dired-subtree-use-backgrounds nil))

(use-package trashed
  :straight t
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package ultra-scroll
  :straight t
  :config
  (ultra-scroll-mode +1))

(use-package treesit-auto
  :straight t
  :config
  (global-treesit-auto-mode))

(use-package multiple-cursors
  :straight t
  :bind
  ("C-c l" . 'mc/edit-lines))

;; Adds additional functionality to the default dired mode
;;
;; https://github.com/emacsmirror/dired-plus/tree/master
;; (use-package dired+
;;   :straight t)

;; (defun my-god-mode-update-cursor-type ()
;;   (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))
  
;; :hook
;; (god-mode-enabled . my-god-mode-update-cursor-type)
;; (god-mode-disabled . my-god-mode-update-cursor-type)

(use-package god-mode
  :disabled
  :straight t
  :bind
  ("<escape>" . #'god-local-mode)
  (:map god-local-mode-map
        ("." . repeat)
        ("i" . god-local-mode)
        ("[" . backward-paragraph)
        ("]" . forward-paragraph))
  :init
  (setopt god-exempt-major-modes nil
          god-exempt-predicates nil)
  :config
  (god-mode))

(use-package evil
  :disabled
  :straight t
  :config
  (evil-mode 1))

(use-package evil-escape
  :disabled
  :straight t
  :config
  (evil-escape-mode)
  (setq-default
   evil-escape-key-sequence "jk"
   evil-escape-delay 0.1))

(use-package avy
  :straight t
  :bind
  ("M-j" . avy-goto-char-timer)
  :init
  (setopt avy-timeout-seconds 0.40))

(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

;; Provides a dashboard/home screen when starting Emacs that lists projects,
;; recent files and more.
;;
;; https://github.com/emacs-dashboard/emacs-dashboard
;; (use-package dashboard
;;   :straight t
;;   :init
;;   (setq dashboard-banner-logo-title "Welcome to Emacs!"
;;         dashboard-footer-messages '("")
;;         dashboard-startup-banner 2
;;         dashboard-center-content nil
;;         dashboard-show-shortcuts t
;;         dashboard-set-navigator t
;;         dashboard-projects-backend 'project-el
;;         dashboard-items '((recents  . 5)
;;                           (bookmarks . 5)
;;                           (projects . 5)
;;                           (agenda . 5))
;;         dashboard-week-agenda t
;;         )
;;         ;; dashboard-filter-agenda-entry 'dashboard-no-filter-agenda
;;   :config
;;   (dashboard-setup-startup-hook))

(use-package dumb-jump
  :straight t
  :init
  (setopt xref-show-definitions-function #'xref-show-definitions-completing-read)
  :hook
  (xref-backend-functions-hook . dumb-jump-xref-activate))

;; The package `flycheck' shows syntactic highlighting in code that displays
;; logs, warnings and errors.
;;
;; https://github.com/flycheck/flycheck
(use-package flycheck
  :disabled
  :straight t
  :init
  (global-flycheck-mode)
  :diminish)

(use-package idle-highlight-mode
  :straight t
  :config (setopt idle-highlight-idle-time 0.35)

  :hook ((prog-mode text-mode) . idle-highlight-mode))

(use-package vertico
  :straight t
  :init
  (setopt vertico-cycle t
          vertico-resize nil
          vertico-count 10
          vertico-multiform-commands '((consult-imenu buffer indexed))
          vertico-multiform-categories '((file grid)
                                         (consult-grep buffer)))
  :config
  (vertico-mode)
  (vertico-multiform-mode))

(use-package marginalia
  :straight (
             :after vertico
             :config
             (marginalia-mode)
             :custom
             (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))))

(use-package corfu
  :straight t
  :bind
  (:map corfu-map
        ("RET" . nil)
        ("<return>" . nil)
        ("TAB" . corfu-complete)
        ("<tab>" . corfu-complete))
  :init
  (setopt corfu-cycle t
          corfu-auto nil
          corfu-auto-delay 0.1
          corfu-separator ?\s
          corfu-quit-at-boundary 'separator
          corfu-quit-no-match t
          corfu-preview-current nil
          corfu-preselect 'first
          corfu-on-exact-match nil
          corfu-scroll-margin 2
          corfu-count 5
          corfu-min-width 20
          corfu-popupinfo-delay '(1.25 . 0.5))
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode 1)
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package orderless
  :straight t
  :init
  (setopt completion-styles '(orderless partial-completion basic)
          completion-category-defaults nil
          completion-category-overrides nil))

(use-package embark
  :disabled
  :straight t)

;; Example configuration for Consult
(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded by `use-package'.
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
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("M-g t" . consult-theme)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
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

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
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
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)


(use-package embark-consult
  :straight t
  :requires (embark consult)
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package consult-flycheck
  :disabled
  :requires (consult flycheck)
  :after (consult flycheck)
  :straight t)

(use-package nerd-icons
  :straight t)

(use-package nerd-icons-completion
  :straight t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :straight t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :straight t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package magit
  :straight t
  :bind
  (("C-c g" . magit-status)
   ("C-c f" . magit-file-dispatch))
  :config
  (setopt magit-show-long-lines-warning nil)
  (magit-auto-revert-mode 1))

(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode)
  
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; Allows for lines or regions to be moved.
;;
;; https://github.com/rejeep/drag-stuff.el
;; (use-package drag-stuff
;;   :straight t
;;   :config
;;   (drag-stuff-global-mode 1)
;;   ;;(drag-stuff-define-keys)
;;   :bind
;;   ("M-P" . drag-stuff-up)
;;   ("M-N" . drag-stuff-down)
;;   :diminish)

(use-package elfeed
  :straight t
  :bind
  ("C-c e" . elfee)
  :config
  (setopt elfeed-feeds '(("https://ziglang.org/devlog/index.xml" zig))))

(use-package ace-window
  :straight t
  :bind
  ("M-o" . ace-window)
  :config
  (setopt aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
          aw-background nil
          aw-scope 'frame))

(use-package dtrt-indent
  :straight t
  :diminish
  :config (dtrt-indent-global-mode))

;; (use-package org-bullets
;;   :disabled
;;   :straight t
;;   :hook
;;   (org-mode . org-bullets-mode))

;; ;; @TODO: Maybe do not enable org-roam unless a valid org-roam-directory exists.
;; (use-package org-roam
;;   :straight t
;;   :init
;;   (setq org-roam-directory (file-truename "~/Documents/org-roam")
;;         org-roam-completion-everywhere t)
;;   :config
;;   (org-roam-db-autosync-enable)
;;   :bind
;;   (("C-c n l" . org-roam-buffer-toggle)
;;    ("C-c n f" . org-roam-node-find)
;;    ("C-c n i" . org-roam-node-insert)
;;    ("C-c n r" . org-roam-node-random)
;;    :map org-mode-map
;;    ("C-M-i"   . completion-at-point)))

;; (use-package org-roam-ui
;;   :straight t
;;   :requires org-roam
;;   :init
;;   (setq org-roam-ui-sync-theme t
;;         org-roam-ui-follow t
;;         org-roam-ui-update-on-save t
;;         org-roam-ui-open-on-start t)
;;   :diminish)


(use-package glsl-mode
  :straight t)

(use-package markdown-mode
  :straight t
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do))
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setopt markdown-command "multimarkdown"))

(use-package zig-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))
  (add-hook 'zig-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        (lambda ()
                          (eglot-code-actions nil nil "source.fixAll" t))))))
;; :bind (:map zig-mode-map
;;             ("C-c b" . zig-compile)
;;             ("C-c r" . zig-run)
;;             ("C-c t" . zig-test))


(use-package rust-mode
  :straight t
  :config
  (setopt rust-format-on-save t
          rust-mode-treesitter-derive t)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

(use-package slang-mode
  :straight (:host github :repo "k1ngst0m/slang-mode")
  :mode (("\\.slang\\'" . slang-mode)
         ("\\.sl\\'" . slang-mode)
         ("\\.slangh\\'" . slang-mode))
  :config
  ;; Optional: Enable LSP support
  ;; (require 'slang-lsp)
  ;; (slang-lsp-initialize)

  )


(use-package ox-hugo
  :straight t
  :after ox)

(use-package ghostel
  :straight (ghostel :type git :host github :repo "dakra/ghostel")
  :bind (("C-x m" . ghostel)
         :map ghostel-semi-char-mode-map
         ("C-s"  . consult-line)
         ("C-k"  . my/ghostel-send-C-k-and-kill)
         ;; ;; I'm used to go up/down the shell history with M-n/p from eshell
         ;; ;; Simulate this behavior in ghostel by sending C-p and C-n
         ("M-p" . (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
         ("M-n" . (lambda () (interactive) (ghostel-send-key "n" "ctrl")))
         :map project-prefix-map
         ("m" . ghostel-project)
         ("M" . ghostel-project-list-buffers))
  :custom (ghostel-module-auto-install 'download)
  :config
  (defun my/ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel. Like normal Emacs `C-k'. Kill to end of line and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl"))

  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer)))

(use-package ghostel-eshell
  :hook (eshell-load . ghostel-eshell-visual-command-mode))

;; Replaces compilation mode buffers with ghostel buffers instead that utilizes
;; libghostty-vt behind the scenes.
(use-package ghostel-compile
  :hook (after-init . ghostel-compile-global-mode)
  :config (ghostel-compile-global-mode t))

(use-package ghostel-comint
  :hook (after-init . ghostel-comint-global-mode))

(use-package doom-themes
  :config
  (load-theme 'doom-badger)
  :straight t)

(provide 'init)
;;; init.el ends here

(defun zo/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;; Below fixes copy and pasting to/from emacs-wayland.

;; (setq wl-copy-process nil)
;; (defun wl-copy (text)
;;   (setq wl-copy-process (make-process :name "wl-copy"
;;                                       :buffer nil
;;                                       :command '("wl-copy" "-f" "-n")
;;                                       :connection-type 'pipe
;;                                       :noquery t))
;;   (process-send-string wl-copy-process text)
;;   (process-send-eof wl-copy-process))
;; (defun wl-paste ()
;;   (if (and wl-copy-process (process-live-p wl-copy-process))
;;       nil ; should return nil if we're the current paste owner
;;       (shell-command-to-string "wl-paste -n | tr -d \r")))
;; (setq interprogram-cut-function 'wl-copy)
;; (setq interprogram-paste-function 'wl-paste)

(setopt remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        remote-file-name-inhibit-auto-save-visited t)

(setopt safe-local-variable-directories '("/home/zakariya/code/engine/"))

(org-babel-do-load-languages 'org-babel-load-languages '((python . t)))

(load "work.el" :no-error-if-file-is-missing)
