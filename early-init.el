;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

(let ((mono-spaced-font "Cascadia Mono")
      (proportionately-spaced-font "Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 140)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

(setopt package-enable-at-startup nil)

;; Move all eln-cache into the no-littering directory.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "data/var/" user-emacs-directory))))
