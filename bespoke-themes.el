;;; bespoke-themes.el --- A set of minimal and medium contrast light/dark themes
;; Copyright (C) 2020 Colin McLear
;; -------------------------------------------------------------------
;; Authors: Colin McLear
;; -------------------------------------------------------------------
;; URL: https://github.com/mclear-tools/bespoke-themes
;; -------------------------------------------------------------------
;; Version: 1
;; Package-Requires: ((emacs "25.1"))
;; -------------------------------------------------------------------
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with this
;; program. If not, see <http://www.gnu.org/licenses/>
;; -------------------------------------------------------------------
;; Commentary: This theme offers a set of light/dark bespoke themes and custom mode
;; line for the discerning yak-shaver. There is also an optional mode line
;; configuration, which may be used either as a header line or a foot. Options and
;; useful function below. See README for further info
;; -------------------------------------------------------------------
;; Code:
;;


;;; Version Requirements
(unless (>= emacs-major-version 26)
  (error "Requires Emacs 26 or later"))

;;; Theme Options

(defcustom bespoke-set-theme 'light
  "Choose which theme variant, light or dark, to use"
  :group 'bespoke-themes
  :type 'symbol)

(defcustom set-bespoke-mode-line 'header
  "If header then use bespoke header line; if footer use bespoke mode line; if nil then do nothing to mode line."
  :group 'bespoke-themes
  :type 'symbol)

(defcustom set-bespoke-evil-cursors t
  "If t then use bespoke evil cursor colors"
  :group 'bespoke-themes
  :type 'boolean)

(defcustom set-bespoke-visual-bell t
  "If t then use bespoke evil cursor colors"
  :group 'bespoke-themes
  :type 'boolean)

;;; After Load Theme Hook
(defvar bespoke-after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'bespoke-after-load-theme-hook))


;;; Disable Theme Function
(defun bespoke--disable-all-themes ()
  "Disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;;; Theme Toggle
;;;###autoload
(defun bespoke/toggle-theme ()
  "Toggle between dark and light variants"
  (if (eq bespoke-set-theme 'light)
      (progn
        (bespoke--disable-all-themes)
        (setq bespoke-set-theme 'dark)
        (load-theme 'bespoke t))
    (progn
      (bespoke--disable-all-themes)
      (setq bespoke-set-theme 'light)
      (load-theme 'bespoke t))))





;;; Provide Theme
(provide 'bespoke-themes)

;;; End bespoke-themes.el
