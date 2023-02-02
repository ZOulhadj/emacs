
(setq gc-cons-threshold 20000000) ;; 20mb
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
;;(setq default-directory "~/")
(delete-selection-mode t)
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(global-hl-line-mode 0)
(setq show-paren-delay 0.0)
(show-paren-mode t)


(setq ring-bell-function 'ignore)
(add-hook 'prog-mode-hook 'subword-mode)
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default dired-listing-switches "-alh")

(setq confirm-kill-emacs 'y-or-n-p)
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq vc-follow-symlinks t)
(setq column-number-mode t)
(setq use-dialog-box nil)
(setq-default indent-tabs-mode nil)
(setq compilation-scroll-output nil)
(recentf-mode 1)
(save-place-mode 1)

(setq split-height-threshold nil)
(setq split-width-threshold 0)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

(set-frame-font "Consolas 16" nil t)
(setq default-frame-alist nil)

(defun custom/open-emacs-config ()
  "Load my Emacs init.el configuration file"
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun custom/latex-word-count ()
  (interactive)
  (shell-command (concat "texcount "
                         ; "uncomment then options go here "
                         (buffer-file-name))))

(global-set-key (kbd "C-c c") 'custom/open-emacs-config)
(global-set-key (kbd "M-.") 'c-fill-paragraph)
(global-set-key (kbd "M-o") 'ff-find-other-file)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "M-/") 'dabbrev-expand)
(global-set-key (kbd "<f6>") 'first-error)
(global-set-key (kbd "<f7>") 'previous-error)
(global-set-key (kbd "<f8>") 'next-error)
(global-set-key (kbd "<f9>") 'compile)
(global-set-key (kbd "C-c w") 'custom/latex-word-count)
(global-unset-key [mouse-2])


(setq c-default-style "k&r")
(setq c-basic-offset 4)

(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
	(font-lock-add-keywords
	 mode
	 '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)



;; Theme
(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
(set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-type-face nil :foreground "DarkGoldenrod3")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3")
(set-foreground-color "burlywood3")
(set-background-color "#161616")
(set-cursor-color "#40FF40")

;; Packages

(package-initialize)
(require 'package)
(add-to-list 'package-archives
    '("melpa" . "http://melpa.org/packages/") t)

(dolist (package '(use-package))
   (unless (package-installed-p package)
       (package-install package)))

(use-package magit
  :ensure t)

;;(setq flycheck-highlighting-mode 'lines)
;;(setq flycheck-highlighting-style 'level-face)

;;(use-package flycheck
;;  :ensure t
;;  :init (global-flycheck-mode))
;;(set-face-attribute 'flycheck-error nil :background "red" :foreground "white")
;;(set-face-attribute 'flycheck-warning nil :background "orange" :foreground "white")
