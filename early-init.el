;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Copyright (c) 2020-2023  Zakariya Oulhadj

;; Author: Zakariya Oulhadj <contact@zakariyaoulhadj.com>
;; URL: https://github.com/ZOulhadj/emacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:

(setq gc-cons-threshold most-positive-fixnum
      read-process-output-max (* 16 1024 1024))

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-splash-screen t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(menu-bar-mode 1)

(setq package-enable-at-startup nil)

;; Move all eln-cache into the no-littering directory.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "tmp/data/" user-emacs-directory))))

(add-hook 'after-init-hook
          (lambda ()
            (message "Emacs loaded in %s seconds with %d garbage collections"
                       (emacs-init-time) gcs-done)))
