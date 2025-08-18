;; init.el --- Init File -*- lexical-binding: t -*-
;;; Commentary:
;; This is my personal programming-focused Emacs configuration file.
;;
;;; Code:

;; Use `straight.el' instead of the built-in `package.el' for downloading external
;; packages.  As we are completely replacing `package.el' we need to download
;; `straight.el' without using it.  We first create a bootstrap file that will
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

;; Load private information
;;(load-file (concat user-emacs-directory "secret.el"))

;; The package `diminish' introduces the `:diminish' keyword which can be used
;; together with `use-package' to hide minor modes from the modeline. This
;; allows the modeline to be kept minimal and show only required modes.
;;
;; https://github.com/emacsmirror/diminish
(use-package diminish
  :straight t)

;; (use-package gcmh
;;   :straight t
;;   :config
;;   (gcmh-mode 1)
;;   :diminish)

;; The package `no-littering' ensures that the `user-emacs-directory' location
;; is kept "clean" by moving the various different files that get created into
;; specific directories. It is important to note that this package must be
;; installed and activated before other Emacs packages are initialised.
;;
;; https://github.com/emacscollective/no-littering
(use-package no-littering
  :straight t
  :init
  (setq no-littering-etc-directory (expand-file-name "tmp/config/" user-emacs-directory)
        no-littering-var-directory (expand-file-name "tmp/data/" user-emacs-directory)))

(use-package emacs
  :init
  (setq create-lockfiles nil
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
        ;;inhibit-splash-screen t
        ;;inhibit-startup-message t

        tab-always-indent 'complete
        delete-by-moving-to-trash t
        user-full-name "Zakariya Oulhadj"
        user-mail-address "zakariyaoulhadj01@gmail.com"

        ;; scroll-conservatively most-positive-fixnum
        sentence-end-double-space nil
        )

  (setq-default tab-width 8
                fill-column 80)

  ;; Disable bindings for suspending Emacs in graphical mode since it's super
  ;; annoying.
  (when (display-graphic-p)
    (global-unset-key (kbd "C-z"))
    (global-unset-key (kbd "C-x C-z")))

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (menu-bar-mode 1)

  :config
  (put 'narrow-to-region 'disabled nil)

  :custom
  (text-mode-ispell-word-completion nil)

  :bind
  ("C-x k" . kill-current-buffer))

(defun zo/config-load ()
  "Load my Emacs init.el configuration file."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

;;(global-set-key (kbd "C-c c") 'custom/load-config)
(global-set-key (kbd "C-c r") 'recompile)

;; ========== [Core] ==========
(use-package use-package-core
  :init
  (setq use-package-verbose t))

(use-package comp
  :init
  (setq native-comp-async-report-warnings-errors 'silent))

(use-package custom
  :init
  (setq custom-safe-themes t))

;; ========== [User Interface] ==========


(defun prev-window ()
  (interactive)
  (other-window -1))

(use-package window
  :init
  (setq switch-to-buffer-obey-display-actions nil)
  :config
  (global-set-key (kbd "C-x o") 'other-window)

  :bind
  ("C-," . prev-window)
  ("C-." . other-window)

  ("C-1" . delete-other-windows)
  ("C-2" . split-window-below)
  ("C-3" . split-window-right)
  ("C-0" . delete-window)

  ;; These bindings make it easier dealing with windows in `god-mode'.
  ;; @TODO: Only enable these bindings when god-mode is active.
  ("C-x C-1" . delete-other-windows)
  ("C-x C-2" . split-window-below)
  ("C-x C-3" . split-window-right)
  ("C-x C-0" . delete-window)

  )

(use-package frame
  :config
  ;;(toggle-frame-maximized)
  ;;(toggle-frame-fullscreen)
  )

(use-package display-fill-column-indicator
  :config
  (set-face-attribute 'fill-column-indicator nil :foreground "grey14")
  ;; :hook
  ;; (prog-mode . display-fill-column-indicator-mode)
  )

(use-package fringe
  :config
  (fringe-mode nil))

(use-package hl-line
  :config
  (global-hl-line-mode 0))

;; The package `which-key' displays a popup window showing all the possible key
;; combinations for the current action. This allows a user to not forget
;; specific commands.
(use-package which-key
  :init
  (setq which-key-show-early-on-C-h nil
        which-key-idle-delay 2.0
        which-key-idle-secondary-delay nil)
  :config
  ;;(which-key-enable-god-mode-support)
  (which-key-setup-side-window-bottom)
  (which-key-mode)
  :diminish)

(use-package face-remap
  :config
  (setq text-scale-mode-step 1.2))

;; @TODO: Move this into eglot configuration
(use-package cc-vars
  :init
  (setq c-default-style "k&r"
        c-basic-offset 4))

(use-package subword
  :config
  (global-subword-mode 1))

(use-package compile
  :init
  (setq compilation-scroll-output nil
        compilation-ask-about-save nil)
  ;; Make the compilation window automatically disappear - from enberg on #emacs
  ;; (setq compilation-finish-functions
  ;;       (lambda (buf str)
  ;;         (if (null (string-match ".*exited abnormally.*" str))
  ;;             ;;no errors, make the compilation window go away in a few seconds
  ;;             (progn
  ;;               (run-at-time
  ;;                "1 sec" nil 'delete-windows-on
  ;;                (get-buffer-create "*compilation*"))
  ;;               (message "No Compilation Errors!")))))
  :bind
  ("<f5>" . recompile)
  ;;("C-c c" . recompile)
  )

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package comint
  :init
  (setq comint-input-ignoredups t
        comint-process-echoes t))

(use-package whitespace
  :hook
  (before-save . whitespace-cleanup))

(use-package calendar
  :init
  (setq calendar-date-style "iso"
        calendar-week-start-day 1))

(use-package vc-hooks
  :init
  (setq vc-follow-symlinks t))

(use-package files
  :init
  (setq large-file-warning-threshold 100000000 ; warn when opening files bigger than 100MB
        make-backup-files nil
        require-final-newline t
        delete-old-versions t
        confirm-kill-emacs nil))

(use-package delsel
  :config
  (delete-selection-mode t))

(use-package saveplace
  :init
  (setq save-place-limit 500)
  :config
  (save-place-mode 1))

(use-package savehist
  :init
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring)
        savehist-autosave-interval 60)
  :config
  (savehist-mode +1))

(use-package desktop
  :disabled ;; todo: messes up the previously loaded theme.
  :init
  (setq
   ;; @TODO: For some reason dirname is not set?
   desktop-dirname (expand-file-name "tmp/data/desktop/" user-emacs-directory)
   desktop-load-locked-desktop t
   desktop-auto-save-timeout 30)
  :config
  (desktop-save-mode 1))

;; (use-package pixel-scroll
;;   :config
;;   ;; @TODO: Disabled for now because it feels laggy.
;;   (pixel-scroll-precision-mode 0))

(use-package autorevert
  :init
  (setq global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode t))

(use-package simple
  :init
  (setq read-extended-command-predicate #'command-completion-default-include-p ; hide commands (M-x) that are not supported in the current mode)
        column-number-mode t
        next-line-add-newlines t
        kill-do-not-save-duplicates t)
  (setq-default indent-tabs-mode nil)
  :config
  (visual-line-mode -1)
  (size-indication-mode -1)
  (set-default 'truncate-lines t))

(use-package time
  :init
  (setq display-time-24hr-format t)
  :config
  (display-time-mode -1)
  :custom
  (display-time-default-load-average nil))

(use-package display-line-numbers
  :init
  (setq display-line-numbers-type 'visual)
  ;;:hook
  ;;(prog-mode . display-line-numbers-mode)
  ;;(org-mode . display-line-numbers-mode)
  )

(use-package which-func
  :hook
  (prog-mode . which-function-mode))

(use-package elec-pair
  :config
  (electric-pair-mode 0))

(use-package paren
  :init
  (setq show-paren-delay 0.0)
  :config
  (show-paren-mode 1))

(use-package recentf
  :init
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on emacs start, because it can cause
        ;; problems with remote files (prelude)
        recentf-auto-cleanup 'never)

  :config
  (recentf-mode 1)

  :custom
  ;; exclude all of files in the no-littering directories from recentf.
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory)))

(use-package windmove
  :config
  (windmove-default-keybindings))

;; todo: only enable if aspell is installed
;; maybe we can use :ensure-system-package
(use-package flyspell
  :disabled ;; @TODO: C-, conflicts with prev-window
  :init
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"))
  :hook
  (prog-mode . flyspell-prog-mode))

(use-package isearch
  :config
  (setq isearch-wrap-pause t
        isearch-lazy-count t
        isearch-allow-scroll 'unlimited)
  :bind (:map isearch-mode-map
              ("<backspace>" . isearch-del-char)))

(use-package dired
  :init
  (setq dired-kill-when-opening-new-dired-buffer t
        dired-hide-details-hide-symlink-targets nil
        dired-dwim-target t)
  ;; @TODO: on macOS --group-directories-first is not supported.
  ;; See: https://github.com/d12frosted/homebrew-emacs-plus/issues/383#issuecomment-899157143
  (setq-default dired-listing-switches "-l --all --no-group --human-readable --group-directories-first --time-style=long-iso")
  :config
  (define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)
  ;; :hook
  ;; (dired-mode . dired-hide-details-mode)
  )

(use-package org
  :init
  ;(setq-default org-display-custom-times t)
  (setq org-agenda-files (list "~/Documents/org/personal.org"
                               "~/Documents/org/school.org")
        org-startup-indented nil
        org-startup-with-inline-images t
        org-display-remote-inline-images t
        ;org-time-stamp-custom-formats '("<%d/%m/%y %a>" . "<%d/%m/%y %a %h:%m>")
        org-return-follows-link t
        org-hide-emphasis-markers t
        ;;org-export-backends '(html md))
        )
  :hook
  (org-mode . turn-on-auto-fill))

(use-package org-capture
  :init
  (setq org-default-notes-file "~/Documents/org/personal.org"
        org-capture-templates '(("t" "TODO" entry (file+headline "~/Documents/org/todo.org" "TODO")
                                 "* TODO %?\n  %i\n  %a")
                                ("d" "Diary" entry (file+datetree "~/Documents/org/diary.org")
                                 "* %?\nEntered on %U\n  %i\n  %a")))
  :bind (("C-c c" . org-capture)))

(use-package org-clock
  :init
  (setq org-clock-persist 'history
        org-clock-idle-time 15)
  :config
  (org-clock-persistence-insinuate))

;; (use-package gnus
;;   :init
;;   (setq gnus-select-method '(nntp "news.gmane.io")
;;         gnus-thread-hide-subtree t
;;         gnus-newsgroup-maximum-articles 50
;;         gnus-secondary-select-methods '((nntp "news.tilde.club"))))

;; The `treesit' package performs fast syntax parsing for languages and allows
;; for other packages to make use of the better context aware functionality.
;;
;; https://github.com/tree-sitter/tree-sitter
(use-package treesit
  :init
  (setq treesit-language-source-alist
        '((c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (python "https://github.com/tree-sitter/tree-sitter-python")))

  ;; Even when tree sitter is installed and the language grammer is configured,
  ;; emacs will not enable it. this is because we must enable the special "ts"
  ;; modes. so here we remap the default modes to tree-sitter specific modes.
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (c-or-c++-mode . c-or-c++-ts-mode)
          (python-mode . python-ts-mode))))

;; @TODO: Requires c/c++ language server
(use-package c-ts-mode
  :requires treesit
  :init
  (setq c-ts-mode-indent-offset 4
        c-ts-mode-indent-style 'k&r))

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
  :init
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider
                                            :inlayHintProvider)
        eglot-autoshutdown t
        eglot-events-buffer-size 0)

  :config
  ;; (add-hook 'eglot-managed-mode-hook (lambda () (eldoc-mode -1)))
  (add-to-list 'eglot-server-programs
               '((c-ts-mode c++-ts-mode c-mode c++-mode)
                 . ("clangd"
                    "-j=16"
                    ;; "--log=error"
                    "--malloc-trim"
                    "--background-index"
                    ;; "--clang-tidy"
                    "--completion-style=detailed"
                    "--pch-storage=memory"
                    "--header-insertion=never"
                    "--header-insertion-decorators=0"))
               '(zig-mode . (
                             ;; Use `zls` if it is in your PATH
                             "zls"
                             ;; There are two ways to set config options:
                             ;;   - edit your `zls.json` that applies to any editor that uses ZLS
                             ;;   - set in-editor config options with the `initializationOptions` field below.
                             ;;
                             ;; Further information on how to configure ZLS:
                             ;; https://zigtools.org/zls/configure/
                             ;;
                             ;; Support for `initializationOptions` in Emacs requires at least ZLS `0.14.0-dev.22+a263b8dc6`.
                             :initializationOptions
                             (;; Whether to enable build-on-save diagnostics
                              ;;
                              ;; Further information about build-on save:
                              ;; https://zigtools.org/zls/guides/build-on-save/
                              ;;enable_build_on_save t

                              ;; omit the following line if `zig` is in your PATH
                              ;;:zig_exe_path "/path/to/zig_executable"
                              ))))

  ;; :custom
  ;; (setq-default eglot-inlay-hints-mode -1)
  ;; (eldoc-echo-area-use-multiline-p nil)


  ;; :bind (:map eglot-mode-map
  ;;             ("C-c C-d" . eldoc)
  ;;             ("C-c C-r" . eglot-rename))

  :hook
  (prog-mode . eglot-ensure)
  )
  ;;:hook
  ;;(add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))

;; (use-package tab-bar
;;   :init
;;   (setq
;;    tab-bar-show t
;;    tab-bar-new-tab-to 'rightmost
;;    ;;tab-bar-new-tab-choice "*dashboard*"
;;    )
;;   :config
;;   (tab-bar-mode 0))

(use-package erc
  :disabled
  :init
  (setq erc-server "irc.libera.chat"
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

;; (use-package undo-tree
;;   :straight t
;;   :init
;;   (setq undo-tree-history-directory-alist
;;       `((".*" . ,temporary-file-directory)))
;;   :config
;;   (global-undo-tree-mode)
;;   :diminish)

;; (use-package ultra-scroll
;;   :straight (ultra-scroll :type git :host github :repo "jdtsmith/ultra-scroll")
;;   :init
;;   (setq scroll-conservatively 101 ; important!
;;         scroll-margin 0)
;;   :config
;;   (ultra-scroll-mode 1))

;; ;;(add-hook 'help-fns-describe-function-functions #'shortdoc-help-fns-examples-function)
;; (global-unset-key [mouse-2])


;; (make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
;; (setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory)))
;;       auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
;;       auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t))
;;       custom-file (no-littering-expand-etc-file-name "custom.el"))

;; Adds additional functionality to the default dired mode
;;
;; https://github.com/emacsmirror/dired-plus/tree/master
;; (use-package dired+
;;   :straight t)



;; (defun my-god-mode-update-cursor-type ()
;;   (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

;; (use-package god-mode
;;   :straight t
;;   :init
;;   (setq god-exempt-major-modes nil
;;         god-exempt-predicates nil)
;;   :config
;;   (god-mode)
;;   :bind
;;   ("<escape>" . #'god-local-mode)
;;   (:map god-local-mode-map
;;         ("." . repeat)
;;         ("i" . god-local-mode)
;;         ("[" . backward-paragraph)
;;         ("]" . forward-paragraph))
;;   ;; :hook
;;   ;; (god-mode-enabled . my-god-mode-update-cursor-type)
;;   ;; (god-mode-disabled . my-god-mode-update-cursor-type)
;;   )


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
  (setq-default evil-escape-key-sequence "jk"
                evil-escape-delay 0.1)
  )

(use-package avy
  :straight t
  :init
  (setq avy-timeout-seconds 0.40)
  :config
  (global-set-key (kbd "M-j") 'avy-goto-char-timer))

;; ;; The package `exec-path-from-shell' ensures all environment variables are
;; ;; present within Emacs. By default, Emacs only uses a small subset of
;; ;; variables. However, this package works by copying all enviornment variables
;; ;; from the system into Emacs so that all commands are accessible.

;; ;; todo: Take a closer look how this package behaves on Windows since its not
;; ;; Unix based.
;; ;;
;; ;; https://github.com/purcell/exec-path-from-shell
;; (use-package exec-path-from-shell
;;   :straight t
;;   :config
;;   (exec-path-from-shell-initialize))

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
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; The package `flycheck' shows syntactic highlighting in code that displays
;; logs, warnings and errors.
;;
;; https://github.com/flycheck/flycheck
(use-package flycheck
  :disabled
  :straight t
  :init (global-flycheck-mode)
  :diminish)


;; The package `vertico' provides vertical interactive completion similar to
;; `smex' or the built-in package `ido'.
;;
;; https://github.com/minad/vertico
(use-package vertico
  :straight t
  :init
  (setq vertico-cycle t
        vertico-resize nil
        vertico-count 10
        vertico-multiform-commands '((consult-imenu buffer indexed))
        vertico-multiform-categories '((file grid)
                                       (consult-grep buffer)))
  :config
  (vertico-mode)
  (vertico-multiform-mode))

;; Adds a small description to each item within the minibuffer completion list.
;;
;; https://github.com/minad/marginalia
(use-package marginalia
  :straight t
  :after vertico
  :config
  (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

;; The package `corfu' display a window for autocomplete candidates when writing
;; text. It is a simpler alternative to the highly popular `company'
;; package. This is because it uses the Emacs buit-in completion system.
;;
;; https://github.com/minad/corfu
(use-package corfu
  :straight t
  :init
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-delay 0.2 ; Should not use lower values as this can cause issues
        corfu-separator ?\s
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match t
        corfu-preview-current nil
        corfu-preselect 'valid
        corfu-on-exact-match 'insert
        corfu-scroll-margin 1)
  :config
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
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package embark
  :disabled
  :straight t)

;; Provides search and navigation commands
;;
;; https://github.com/minad/consult
(use-package consult
  :straight t
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<")
  ;; Hide buffers that start with a * and only shown them if SPC is pressed
  (add-to-list 'consult-buffer-filter "^\\*")

  :bind (("C-c M-x" . consult-mode-command)
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
  :hook (completion-list-mode . consult-preview-at-point-mode))

;; Consult users will also want the embark-consult package.
;;
;;
(use-package embark-consult
  :straight t
  :requires (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-flycheck
  :disabled
  :requires (consult flycheck)
  :after (consult flycheck)
  :straight t)

;; A Git client that can be used within Emacs.
;;
;; https://github.com/magit/magit
(use-package magit
  :straight t
  :config
  (magit-auto-revert-mode 1)
  :bind
  ("C-c g" . magit-status)
  ("C-c f" . magit-file-dispatch))

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

;; A Emacs based email client that makes use of mu.
;;
;; https://github.com/djcb/mu
;; todo: only enable if mu (maildir-utils) is installed
;; maybe we can use :ensure-system-package
;; Commands:
;; mu init --maildir=MAILDIR --my-address=MYADDRESS
;; mu index
;; (use-package mu4e
;;   :disabled
;;   :straight (:local-repo "/usr/share/emacs/site-lisp/mu4e/"
;;                          :type built-in)
;;   :commands mu4e
;;   :init
;;   (setq mu4e-maildir "~/Mail"
;;         mu4e-get-mail-command "mbsync -c ~/.dotfiles/mbsync/.mbsyncrc -a"
;;         mu4e-update-interval 60
;;         mu4e-mu-debug nil
;;         ;;mu4e-confirm-quit nil
;;         ;;mu4e-context-policy 'pick-first
;;         mu4e-change-filenames-when-moving t
;;         ;;mu4e-headers-fields `((:human-date . 12)
;;         ;;                      (:flags . 6)
;;         ;;                      (:mailing-list . 10)
;;         ;;                      (:from . 22)
;;         ;;                      (:subject))
;;         mu4e-sent-folder   "/[Gmail].Sent Mail"
;;         mu4e-refile-folder "/[Gmail].All Mail"
;;         mu4e-drafts-folder "/[Gmail].Drafts"
;;         mu4e-trash-folder  "/[Gmail].Trash"
;;         mu4e-maildir-shortcuts '(("/Inbox"             . ?i)
;;                                  ("/[Gmail].Sent Mail" . ?s)
;;                                  ("/[Gmail].All Mail"  . ?a)
;;                                  ("/[Gmail].Trash"     . ?t)
;;                                  ("/[Gmail].Drafts"    . ?d))

;;         smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;         smtpmail-auth-credentials '(("smtp.gmail.com" 587 "zakariyaoulhadj01@gmail.com" nil))
;;         smtpmail-default-smtp-server "smtp.gmail.com"
;;         smtpmail-smtp-server "smtp.gmail.com"
;;         smtpmail-smtp-service 587
;;    )

;;    ;;message-kill-buffer-on-exit t)
;;    ;;mu4e-headers-date-format "%d/%m/%Y %H:%M"
;;                                ;; message-send-mail-function 'smtpmail-send-it
;;                                ;; starttls-use-gnutls t
;;                                ;; smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;                                ;; smtpmail-auth-credentials '(("smtp.gmail.com" 587 "zakariyaoulhadj01@gmail.com" nil))
;;                                ;; smtpmail-default-smtp-server "smtp.gmail.com"
;;                                ;; smtpmail-smtp-server "smtp.gmail.com"
;;                                ;; smtpmail-smtp-service 587

;;   :bind
;;   ("C-c m" . mu4e))

;; (use-package mu4e-alert
;;   :straight t
;;   :requires mu4e
;;   :config
;;   (mu4e-alert-enable-mode-line-display))

;; (use-package mu4e-marker-icons
;;   :requires all-the-icons
;;   :straight t
;;   :init (mu4e-marker-icons-mode 1))

;; The package `elfeed' is an RSS client that allows a user to provide a list of
;; RSS sources and the package will retrive the latest news.
;;
;; https://github.com/skeeto/elfeed
(use-package elfeed
  :straight t
  :defer t
  :commands (elfeed)
  :config
  (setq elfeed-feeds
   '(("https://www.kernel.org/feeds/kdist.xml" linux)
     ("https://protesilaos.com/codelog.xml" emacs prot)
     ("https://ziglang.org/devlog/index.xml" zig)))
  :bind
  (("C-c e" . elfeed)))

(use-package ace-window
  :straight t
  :config
  (setq
   aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
   aw-background t)
  :bind
  ("M-o" . ace-window))

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


(use-package zig-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))
  )

(use-package vterm
  :straight t)

(use-package ox-hugo
  :straight t
  :after ox)

(use-package naysayer-theme
  :disabled
  :straight t
  :config
  (load-theme 'naysayer))

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-gruvbox-light))


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

(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                                      :buffer nil
                                      :command '("wl-copy" "-f" "-n")
                                      :connection-type 'pipe
                                      :noquery t))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r")))
(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)




(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t)

(setq safe-local-variable-directories '(
                                        "/home/zakariya/dev/game/"
                                        "/home/zakariya/dev/emacs/"
                                        ))
