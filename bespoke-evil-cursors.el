;;; bespoke-evil-cursors.el -- custom evil cursor colors for bespoke theme  ;; -*- lexical-binding: t -*-
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
;; A custom set of evil cursor colors for the discerning yakshaver.
;; -------------------------------------------------------------------
;;

;;; Set variable
(defcustom set-bespoke-evil-cursors t
  "If t then use bespoke evil cursor colors"
  :group 'bespoke-themes
  :type 'boolean)


;;; Define evil cursor colors
(defun bespoke-evil-load-cursors ()
  "Load theme specific cursor colors"
  (interactive)
  (setq evil-emacs-state-cursor    `(,bespoke-salient box))
  (setq evil-normal-state-cursor   `(,bespoke-yellow box))
  (setq evil-visual-state-cursor   `(,bespoke-faded box))
  (setq evil-insert-state-cursor   `(,bespoke-red (bar . 2)))
  (setq evil-replace-state-cursor  `(,bespoke-critical hbar))
  (setq evil-motion-state-cursor   `(,bespoke-green box))
  (setq evil-operator-state-cursor `(,bespoke-brown hollow)))

(when set-bespoke-evil-cursors
  (add-hook 'bespoke-after-load-theme-hook #'bespoke-evil-load-cursors))

;;; Provide file
(provide 'bespoke-evil-cursors)

;;; End bespoke-evil-cursors.el
