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
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>
;; -------------------------------------------------------------------
;; Commentary:
;; This theme offers a set of light/dark bespoke themes and custom mode line
;; for the discerning yakshaver
;; -------------------------------------------------------------------
;; Code:
;;



;;; Version Requirements
(unless (>= emacs-major-version 25)
  (error "Requires Emacs 25 or later"))

;;; After Load Theme Hook
(defvar bespoke-after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'bespoke-after-load-theme-hook))

;;; Faces
(require 'bespoke-faces)
;;; Modeline
(require 'bespoke-modeline)

;;; Evil Cursors
(require 'bespoke-evil-cursors)

;;; Provide path to file

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;; Provide Theme
(provide 'bespoke-themes)

;;; End bespoke-themes.el
