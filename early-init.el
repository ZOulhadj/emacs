;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; (setq gc-cons-threshold most-positive-fixnum
;;       read-process-output-max (* 16 1024 1024))

(setq-default default-font "IBM Plex Mono Medium")
(set-face-attribute 'default nil :family default-font :height 140)

(setq package-enable-at-startup nil)

;; Move all eln-cache into the no-littering directory.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "data/var/" user-emacs-directory))))
