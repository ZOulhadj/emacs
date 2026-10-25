;;; early-init.el --- Early Init File -*- lexical-binding: t -*-


(setopt gc-cons-threshold most-positive-fixnum
        package-enable-at-startup nil
        frame-inhibit-implied-resize t
        package-native-compile t)


(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 50 1000 1000) ; 50MB
                  gc-cons-percentage 0.2)))

(let ((mono-spaced-font "Cascadia Mono")
      (proportionately-spaced-font "Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 140)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

;; Move all eln-cache into the no-littering directory.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "data/var/" user-emacs-directory))))
