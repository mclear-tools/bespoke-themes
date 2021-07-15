;;; bespoke-themes.el --- A set of minimal and medium contrast light/dark themes
;; Copyright (C) 2020 Colin McLear
;; -------------------------------------------------------------------
;; Authors: Colin McLear
;; -------------------------------------------------------------------
;; URL: https://github.com/mclear-tools/bespoke-themes
;; -------------------------------------------------------------------
;; Created: 2021-03-16
;; Version: 0.6
;; Package-Requires: ((emacs "26.1"))
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
;;; Commentary: This theme offers a set of light/dark bespoke themes and custom mode
;; line for the discerning yak shaver. There is also an optional mode line
;; configuration, which may be used either as a header line or a foot. Options and
;; useful function below. See README for further info
;; -------------------------------------------------------------------
;;; Code:
;;


;;;; Version Requirements
(unless (>= emacs-major-version 26)
  (error "Requires Emacs 26 or later"))

;;;; Theme Options

(defcustom bespoke-set-theme 'light
  "Choose which theme variant, light or dark, to use."
  :group 'bespoke-themes
  :type 'symbol)

;; Modeline options
(defcustom bespoke-set-mode-line 'header
  "Whether bespoke-theme should set its own modeline, and if so, where.
If nil, don't set modeline content or position, only its base colors.
If the value is `header', then set modeline as headerline.
If the value is `footer', then set modeline as the standard footer line."
  :group 'bespoke-themes
  :type '(choice
          (const :tag "Nil" nil)
          (const :tag "Header" header)
          (const :tag "Footer" footer)))

(defcustom bespoke-set-mode-line-size 3
  "Set the size of the mode line as an integer
Initial value is 3."
  :group 'bespoke-themes
  :type 'integer)

(defcustom bespoke-set-mode-line-cleaner nil
  "If t then show abbreviated mode symbol in modeline. Default is
nil. To change the values of the major-mode symbols see the value
of bespoke-mode-line-cleaner-alist"
  :group 'bespoke-themes
  :type 'boolean)

(defcustom bespoke-set-git-diff-mode-line t
  "If t then show diff lines in modeline."
  :group 'bespoke-themes
  :type 'boolean)

;; Mode line symbols
(defcustom bespoke-mode-line-gui-ro-symbol " ⨂ "
  "Modeline gui read-only symbol."
  :group 'bespoke-themes
  :type 'string)

(defcustom bespoke-mode-line-gui-mod-symbol " ⨀ "
  "Modeline gui modified symbol."
  :group 'bespoke-themes
  :type 'string)

(defcustom bespoke-mode-line-gui-rw-symbol " ◯ "
  "Modeline gui read-write symbol."
  :group 'bespoke-themes
  :type 'string)

(defcustom bespoke-mode-line-tty-ro-symbol " RO "
  "Modeline tty read-only symbol."
  :group 'bespoke-themes
  :type 'string)

(defcustom bespoke-mode-line-tty-mod-symbol " ** "
  "Modeline tty modified symbol."
  :group 'bespoke-themes
  :type 'string)

(defcustom bespoke-mode-line-tty-rw-symbol " RW "
  "Modeline tty read-write symbol."
  :group 'bespoke-themes
  :type 'string)

;; Cursors
(defcustom bespoke-set-evil-cursors t
  "If t then use bespoke evil cursor colors."
  :group 'bespoke-themes
  :type 'boolean)

;; Visual Bell
(defcustom bespoke-set-visual-bell t
  "If t then use bespoke-visual-bell."
  :group 'bespoke-themes
  :type 'boolean)

;; Font options
(defcustom bespoke-set-italic-comments t
  "If t then use italics for comments."
  :group 'bespoke-themes
  :type 'boolean)

(defcustom bespoke-set-italic-keywords t
  "If t then use italics for keywords."
  :group 'bespoke-themes
  :type 'boolean)

(defcustom bespoke-set-variable-pitch t
  "If t then use variable-pitch for headings."
  :group 'bespoke-themes
  :type 'boolean)

;;;; After Load Theme Hook
(defvar bespoke-after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'bespoke-after-load-theme-hook))


;;;; Disable Theme Function
(defun bespoke--disable-all-themes ()
  "Disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;;;; Theme Toggle
;;;###autoload
(defun bespoke/toggle-theme ()
  "Toggle between dark and light variants"
  (interactive)
  (if (eq bespoke-set-theme 'light)
      (progn
        (bespoke--disable-all-themes)
        (setq bespoke-set-theme 'dark)
        (load-theme 'bespoke t))
    (progn
      (bespoke--disable-all-themes)
      (setq bespoke-set-theme 'light)
      (load-theme 'bespoke t))))

;;;; Call Theme Functions
;;;###autoload
(defun bespoke/light-theme ()
  "Set light variant of bespoke-theme"
  (interactive)
  (bespoke--disable-all-themes)
  (setq bespoke-set-theme 'light)
  (load-theme 'bespoke t))

;;;###autoload
(defun bespoke/dark-theme ()
  "Set dark variant of bespoke-theme"
  (interactive)
  (bespoke--disable-all-themes)
  (setq bespoke-set-theme 'dark)
  (load-theme 'bespoke t))




;;; Provide Theme
(provide 'bespoke-themes)

;;; End bespoke-themes.el
