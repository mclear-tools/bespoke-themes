;;; bespoke-faces.el -- custom faces for bespoke theme  ;; -*- lexical-binding: t -*-
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
;; Custom faces for the discerning yakshaver.
;; This started as a fork of nano-emacs.
;; See https://github.com/rougier/nano-emacs
;; -------------------------------------------------------------------


;;; Define group & colors

(defgroup bespoke '()
  "Faces and colors for bespoke theme")

;; Derive our default color set from core Emacs faces.
;; This allows use of bespoke colors in independently themed Emacsen
;;
;; We memorize the default colorset in this var in order not to confuse
;; customize: the STANDARD argument of defcustom gets re-evaluated by customize
;; to determine if the current value is default or not.
(defvar bespoke-base-colors--defaults
  `((foreground . ,(face-foreground 'default nil t))
    (background . ,(face-background 'default nil t))
    (highlight . ,(face-background 'fringe nil t))
    (critical . ,(face-foreground 'error nil t))
    (salient . ,(face-foreground 'font-lock-keyword-face nil t))
    (strong . ,(face-foreground 'default nil t))
    (popout . ,(face-foreground 'font-lock-string-face nil t))
    (subtle . ,(face-background 'mode-line-inactive nil t))
    (faded . ,(face-foreground 'shadow nil t))))

(defun bespoke-base-colors--get (name)
  "Get default color associated with symbol NAME."
  (cdr (assoc name bespoke-base-colors--defaults)))

(defcustom bespoke-foreground (bespoke-base-colors--get 'foreground)
  ""
  :type 'color
  :group 'bespoke)

(defcustom bespoke-background (bespoke-base-colors--get 'background)
  ""
  :type 'color
  :group 'bespoke)

(defcustom bespoke-highlight (bespoke-base-colors--get 'highlight)
  ""
  :type 'color
  :group 'bespoke)

(defcustom bespoke-critical (bespoke-base-colors--get 'critical)
  ""
  :type 'color
  :group 'bespoke)

(defcustom bespoke-salient (bespoke-base-colors--get 'salient)
  ""
  :type 'color
  :group 'bespoke)

(defcustom bespoke-strong (bespoke-base-colors--get 'strong)
  ""
  :type 'color
  :group 'bespoke)

(defcustom bespoke-popout (bespoke-base-colors--get 'popout)
  ""
  :type 'color
  :group 'bespoke)

(defcustom bespoke-subtle (bespoke-base-colors--get 'subtle)
  ""
  :type 'color
  :group 'bespoke)

(defcustom bespoke-faded (bespoke-base-colors--get 'faded)
  ""
  :type 'color
  :group 'bespoke)


;;; Define Faces
;; The themes are fully defined by these faces

(defface bespoke-default nil
  "Default face is used for regular information."
  :group 'bespoke)

(defface bespoke-critical nil
  "Critical face is for information that requires immediate action.
It should be of high constrast when compared to other faces. This
can be realized (for example) by setting an intense background
color, typically a shade of red. It must be used scarcely."
  :group 'bespoke)

(defface bespoke-popout nil
  "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect."
  :group 'bespoke)

(defface bespoke-strong nil
  "Strong face is used for information of a structural nature.
It has to be the same color as the default color and only the
weight differs by one level (e.g., light/regular or
regular/bold). IT is generally used for titles, keywords,
directory, etc."
  :group 'bespoke)

(defface bespoke-salient nil
  "Salient face is used for information that are important.
To suggest the information is of the same nature but important,
the face uses a different hue with approximately the same
intensity as the default face. This is typically used for links."
  :group 'bespoke)

(defface bespoke-faded nil
  "Faded face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information and also replace italic (which is generally
abused anyway)."
  :group 'bespoke)

(defface bespoke-subtle nil
  "Subtle face is used to suggest a physical area on the screen.
It is important to not disturb too strongly the reading of
information and this can be made by setting a very light
background color that is barely perceptible."
  :group 'bespoke)

(defface bespoke-red nil
  "A reddish accent face"
  :group 'bespoke)

(defface bespoke-green nil
  "A greenish accent face"
  :group 'bespoke)

(defface bespoke-blue nil
  "A bluish accent face"
  :group 'bespoke)

(defface bespoke-yellow nil
  "A yellowish accent face"
  :group 'bespoke)

(defface bespoke-brown nil
  "A brownish accent face"
  :group 'bespoke)


;;; Define Color Palettes
;;;; Light Colors

(defun bespoke-theme-set-light ()
  "Define and apply bespoke-light."
  (interactive)

  (setq bespoke-white        "#FFFFFF")
  (setq bespoke-light        "#FFFEF9")
  (setq bespoke-subtle       "#ECEFF1")
  (setq bespoke-modeline     "#E3E7EF")
  (setq bespoke-highlight    "#D8DEE9")

  (setq bespoke-faded        "#8a93a8")
  (setq bespoke-salient      "#333fa6")
  (setq bespoke-popout       "#940b96")
  (setq bespoke-critical     "#f53137")

  (setq bespoke-blue         "#90185a")
  (setq bespoke-red          "#b71c1c")
  (setq bespoke-green        "#00796b")
  (setq bespoke-brown        "#966e53")
  (setq bespoke-yellow       "#e7b04b")

  (setq bespoke-dark         "#292e39")
  (setq bespoke-strong       "#000000"))



;;;; Dark Colors

(defun bespoke-theme-set-dark ()
  "Define and apply bespoke-light."
  (interactive)

  (setq bespoke-black      "#000000")
  (setq bespoke-dark       "#292e39")
  (setq bespoke-subtle     "#333a47")
  (setq bespoke-modeline   "#3C4353")
  (setq bespoke-highlight  "#596377")

  (setq bespoke-faded      "#7c89a2")
  (setq bespoke-salient    "#81a1c1")
  (setq bespoke-popout     "#a44da2")
  (setq bespoke-critical   "#f46715")

  (setq bespoke-blue       "#88c0d0")
  (setq bespoke-green      "#8eb89d")
  (setq bespoke-red        "#bf616a")
  (setq bespoke-brown      "#d08770")
  (setq bespoke-yellow     "#e9b85d")

  (setq bespoke-light      "#eceff4")
  (setq bespoke-strong     "#ffffff"))

;;; End bespoke-faces.el
(provide 'bespoke-faces)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; End:
