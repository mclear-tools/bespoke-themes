;; bespoke-faces-colors.el --- A custom theme  -*- lexical-binding: t; -*-
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
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>
;; -------------------------------------------------------------------
;;; Commentary:
;; This theme started as a fork of nano-emacs.
;; See https://github.com/rougier/nano-emacs.
;; Color palatte has been expanded and face definitions revised
;; -------------------------------------------------------------------
;;; Code

;;;; Requirements
(require 'bespoke-themes)

(defvar evil-emacs-state-cursor)
(defvar evil-normal-state-cursor)
(defvar evil-visual-state-cursor)
(defvar evil-insert-state-cursor)
(defvar evil-replace-state-cursor)
(defvar evil-motion-state-cursor)
(defvar evil-operator-state-cursor)
(defvar hl-todo-keyword-faces)

;;;; Define group & colors

(defgroup bespoke-themes nil
  "Faces and colors for bespoke themes"
  :group 'faces)

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
  :group 'bespoke-themes)

(defcustom bespoke-background (bespoke-base-colors--get 'background)
  ""
  :type 'color
  :group 'bespoke-themes)

(defcustom bespoke-highlight (bespoke-base-colors--get 'highlight)
  ""
  :type 'color
  :group 'bespoke-themes)

(defcustom bespoke-critical (bespoke-base-colors--get 'critical)
  ""
  :type 'color
  :group 'bespoke-themes)

(defcustom bespoke-salient (bespoke-base-colors--get 'salient)
  ""
  :type 'color
  :group 'bespoke-themes)

(defcustom bespoke-strong (bespoke-base-colors--get 'strong)
  ""
  :type 'color
  :group 'bespoke-themes)

(defcustom bespoke-popout (bespoke-base-colors--get 'popout)
  ""
  :type 'color
  :group 'bespoke-themes)

(defcustom bespoke-subtle (bespoke-base-colors--get 'subtle)
  ""
  :type 'color
  :group 'bespoke-themes)

(defcustom bespoke-faded (bespoke-base-colors--get 'faded)
  ""
  :type 'color
  :group 'bespoke-themes)


;;;; Define Faces
;; The themes are fully defined by these faces

;;;;; Core faces
(defface bespoke-default nil
  "Default face is for regular use."
  :group 'faces)

(defface bespoke-critical nil
  "Critical face is for information that requires action---e.g.,
syntax or spelling errors. It should be of high constrast when
compared to other faces. This can be realized (for example) by
setting an intense background color, typically a shade of red or
orange. It should be used rarely."
  :group 'faces)

(defface bespoke-popout nil
  "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect (see
https://metapraxis.com/blog/blog/the-pop-out-effect/)."
  :group 'faces)

(defface bespoke-strong nil
  "Strong face is used for information of a structural nature.
It is the same color as the default color. Only the
weight differs by one level (e.g., light/regular or
regular/bold). Usage might include titles, keywords,
directory, etc."
  :group 'faces)

(set-face-attribute 'bespoke-strong nil
                    :foreground (face-foreground 'bespoke-default)
                    :weight 'bold)

(defface bespoke-salient nil
  "Salient face is used for important information, though not
necessarily that which needs immediate action or attention. To
suggest the information is important, the face uses a different
hue with approximately the same intensity as the default face.
This might be used, e.g., for links."
  :group 'faces)

(defface bespoke-faded nil
  "Faded face is for less (immediately) important information. It
is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information."
  :group 'faces)

(defface bespoke-subtle nil
  "Subtle face is used to suggest a physical area on the screen.
It's main use is for differentiating regions without drawing a
significant amount of attention. It is also closely related in
shade to the modeline color and to the highlight color."
  :group 'faces)

;;;;; Accent faces
;; The accent colors are used to fill out the color palatte. They are meant to be
;; used for attention or contrast with the core colors. Readability is important.

(defface bespoke-highlight nil
  "This should be used primarily for highlighting. It is meant
subtlety stand out from the mode line and other adjacent faces."
  :group 'faces)

(defface bespoke-modeline nil
  "Default face for the mode line."
  :group 'faces)

(defface bespoke-inactive nil
  "Face for the inactive mode line"
  :group 'faces)

(defface bespoke-red nil
  "A reddish accent face"
  :group 'faces)

(defface bespoke-green nil
  "A greenish accent face"
  :group 'faces)

(defface bespoke-blue nil
  "A bluish accent face"
  :group 'faces)

(defface bespoke-yellow nil
  "A yellowish accent face"
  :group 'faces)

(defface bespoke-brown nil
  "A brownish accent face"
  :group 'faces)

;;;; Define Theme
(deftheme bespoke "A custom theme for yak shaving, with light and dark variants")

;;;; Set Colors

(defun bespoke-theme--light-dark (light dark)
  "Determine theme using the LIGHT or the DARK color variants of bespoke-theme."
  (if (eq bespoke-set-theme 'light)
      light
    dark))
(defalias '--l/d #'bespoke-theme--light-dark)

(defun bespoke--set-theme-variant ()
  "Set theme colors according to LIGHT or DARK variant"
  (setq bespoke-foreground (--l/d "#282b35" "#eceff1"))
  (setq bespoke-background (--l/d "#fffef9" "#282b35"))

  (setq bespoke-modeline   (--l/d "#e3e7ef" "#3c4353"))
  (setq bespoke-highlight  (--l/d "#dbe1eb" "#444B5c"))
  (setq bespoke-inactive   (--l/d "#cbd3e1" "#525868"))

  (setq bespoke-critical   (--l/d "#f53137" "#f46715"))
  (setq bespoke-salient    (--l/d "#303db4" "#88c0d0"))
  (setq bespoke-strong     (--l/d "#000000" "#ffffff"))
  (setq bespoke-popout     (--l/d "#940b96" "#bc85cf"))
  (setq bespoke-subtle     (--l/d "#eceff1" "#333a47"))
  (setq bespoke-faded      (--l/d "#727d97" "#959eb1"))

  (setq bespoke-blue       (--l/d "#30608c" "#81a1c1"))
  (setq bespoke-green      (--l/d "#00796b" "#8eb89d"))
  (setq bespoke-red        (--l/d "#960d36" "#bf616a"))
  (setq bespoke-brown      (--l/d "#966e53" "#d08770"))
  (setq bespoke-yellow     (--l/d "#e0a500" "#e9b85d")))

;;;; Customize Faces

;; Call color settings
(bespoke--set-theme-variant)

;; Declare class and set faces
(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   `bespoke
   `(default ((,class :foreground ,bespoke-foreground :background ,bespoke-background)))


;;;;; Basic Faces
   `(buffer-menu-buffer                            ((,class :foreground ,bespoke-strong)))
   `(minibuffer-prompt                             ((,class :foreground ,bespoke-green)))
   `(link                                          ((,class :foreground ,bespoke-salient)))
   `(region                                        ((,class :background ,bespoke-highlight)))
   `(fringe                                        ((,class :foreground ,bespoke-faded :weight light)))
   `(highlight                                     ((,class :background ,bespoke-subtle)))
   `(lazy-highlight                                ((,class :foreground ,bespoke-green)))
   `(trailing-whitespace                           ((,class :foreground ,bespoke-faded)))
   `(secondary-selection                           ((,class :foreground ,bespoke-yellow :background ,bespoke-subtle)))
   `(show-paren-match                              ((,class :foreground ,bespoke-yellow :weight bold)))
   `(show-paren-mismatch                           ((,class :foreground ,bespoke-critical :weight bold :box t)))
   `(tooltip nil                                   ((,class :height 0.85)))

;;;;; Bespoke Faces
   ;; NOTE: We want the bespoke colors to be available as faces. It seems like there
   ;; should be a better way to do this but...
   `(bespoke-foreground ((,class :foreground ,bespoke-foreground)))
   `(bespoke-background ((,class :background ,bespoke-background)))
   `(bespoke-modeline   ((,class :background ,bespoke-modeline)))
   `(bespoke-highlight  ((,class :foreground ,bespoke-highlight)))
   `(bespoke-inactive   ((,class :foreground ,bespoke-inactive)))
   `(bespoke-critical   ((,class :foreground ,bespoke-critical)))
   `(bespoke-salient    ((,class :foreground ,bespoke-salient)))
   `(bespoke-strong     ((,class :foreground ,bespoke-strong)))
   `(bespoke-popout     ((,class :foreground ,bespoke-popout)))
   `(bespoke-subtle     ((,class :foreground ,bespoke-subtle)))
   `(bespoke-faded      ((,class :foreground ,bespoke-faded)))
   `(bespoke-blue       ((,class :foreground ,bespoke-blue)))
   `(bespoke-green      ((,class :foreground ,bespoke-green)))
   `(bespoke-red        ((,class :foreground ,bespoke-red)))
   `(bespoke-brown      ((,class :foreground ,bespoke-brown)))
   `(bespoke-yellow     ((,class :foreground ,bespoke-yellow)))

;;;;; Buttons
   `(custom-button                                 ((,class :foreground ,bespoke-foreground :background ,bespoke-highlight :box nil)))
   `(custom-button-mouse                           ((,class :foreground ,bespoke-foreground :background ,bespoke-subtle :box nil)))
   `(custom-button-pressed                         ((,class :foreground ,bespoke-background :background ,bespoke-foreground :box nil)))

;;;;; Bookmarks
   `(bookmark-menu-heading                         ((,class :foreground ,bespoke-strong)))
   `(bookmark-menu-bookmark                        ((,class :foreground ,bespoke-salient)))
   `(bookmark-face                                 ((,class :foreground ,bespoke-salient)))

;;;;; Childframes
;;;;;; Mini-Frame
   `(mini-popup-background ((,class :background ,bespoke-subtle)))
   `(mini-popup-border     ((,class :background ,bespoke-subtle)))

;;;;;; Mini-Popup (Childframe)
   `(mini-popup-background ((,class :background ,bespoke-subtle)))
   `(mini-popup-border     ((,class :background ,bespoke-subtle)))

;;;;;; Posframe

   `(which-key-posframe                           ((,class :background ,bespoke-subtle)))
   `(which-key-posframe-border                    ((,class :background ,bespoke-subtle)))
   `(transient-posframe-border                    ((,class :background ,bespoke-subtle)))
   `(transient-posframe                           ((,class :foreground ,bespoke-strong :background ,bespoke-subtle)))

;;;;; Completion/Narrowing

;;;;;; Company
   `(company-scrollbar-fg                          ((,class :foreground ,bespoke-faded)))
   `(company-scrollbar-bg                          ((,class :foreground ,bespoke-faded)))
   `(company-preview                               ((,class :foreground ,bespoke-faded :weight bold)))
   `(company-preview-common                        ((,class :foreground ,bespoke-faded)))
   `(company-tooltip-selection                     ((,class :foreground ,bespoke-salient)))
   `(company-tooltip                               ((,class :background ,bespoke-subtle)))
   `(company-tooltip-common                        ((,class :background ,bespoke-subtle)))
   `(company-tooltip-common-selection              ((,class :foreground ,bespoke-salient)))
   `(company-tooltip-annotation                    ((,class :foreground ,bespoke-faded)))
   `(company-tooltip-annotation-selection          ((,class :foreground ,bespoke-salient)))

;;;;;; Corfu
   `(corfu-annotations                             ((,class :foreground ,bespoke-faded)))
   `(corfu-bar                                     ((,class :foreground ,bespoke-modeline)))
   `(corfu-border                                  ((,class :foreground ,bespoke-subtle)))
   `(corfu-current                                 ((,class :foreground ,bespoke-popout :background ,bespoke-highlight)))
   `(corfu-default                                 ((,class :inherit default :background ,bespoke-subtle)))
   `(corfu-deprecated                              ((,class :foreground ,bespoke-faded)))
   `(corfu-echo                                    ((,class :inherit default)))

;;;;;; Counsel
   `(counsel-active-mode                           ((,class :foreground ,bespoke-salient)))
   `(counsel-application-name                      ((,class :foreground ,bespoke-red)))
   `(counsel-key-binding                           ((,class :inherit default)))
   `(counsel-outline-1                             ((,class :inherit org-level-1)))
   `(counsel-outline-2                             ((,class :inherit org-level-2)))
   `(counsel-outline-3                             ((,class :inherit org-level-3)))
   `(counsel-outline-4                             ((,class :inherit org-level-4)))
   `(counsel-outline-5                             ((,class :inherit org-level-5)))
   `(counsel-outline-6                             ((,class :inherit org-level-6)))
   `(counsel-outline-7                             ((,class :inherit org-level-7)))
   `(counsel-outline-8                             ((,class :inherit org-level-8)))
   `(counsel-outline-default                       ((,class :foreground ,bespoke-foreground)))
   `(counsel-variable-documentation                ((,class :inherit default :foreground ,bespoke-yellow)))

;;;;;; Helm
   `(helm-selection                                ((,class :foreground ,bespoke-subtle :weight bold)))
   `(helm-match                                    ((,class :foreground ,bespoke-strong)))
   `(helm-source-header                            ((,class :foreground ,bespoke-salient)))
   `(helm-visible-mark                             ((,class :foreground ,bespoke-strong)))
   `(helm-swoop-target-line-face                   ((,class :foreground ,bespoke-subtle :weight bold)))
   `(helm-moccur-buffer                            ((,class :foreground ,bespoke-strong)))
   `(helm-ff-file                                  ((,class :foreground ,bespoke-faded)))
   `(helm-ff-prefix                                ((,class :foreground ,bespoke-strong)))
   `(helm-ff-dotted-directory                      ((,class :foreground ,bespoke-faded)))
   `(helm-ff-directory                             ((,class :foreground ,bespoke-strong)))
   `(helm-ff-executable                            ((,class :foreground ,bespoke-popout)))
   `(helm-grep-match                               ((,class :foreground ,bespoke-strong)))
   `(helm-grep-file                                ((,class :foreground ,bespoke-faded)))
   `(helm-grep-lineno                              ((,class :foreground ,bespoke-faded)))
   `(helm-grep-finish                              ((,class :foreground ,bespoke-foreground)))


;;;;;; Ivy
   `(ivy-action                                    ((,class :foreground ,bespoke-faded)))
   `(ivy-completions-annotations                   ((,class :foreground ,bespoke-faded)))
   `(ivy-confirm-face                              ((,class :foreground ,bespoke-faded)))
   `(ivy-current-match                             ((,class :foreground ,bespoke-strong :weight bold :background ,bespoke-highlight)))
   `(ivy-cursor                                    ((,class :inherit default)))
   `(ivy-grep-info                                 ((,class :foreground ,bespoke-strong)))
   `(ivy-grep-line-number                          ((,class :foreground ,bespoke-faded)))
   `(ivy-highlight-face                            ((,class :foreground ,bespoke-strong)))
   `(ivy-match-required-face                       ((,class :foreground ,bespoke-faded)))
   `(ivy-minibuffer-match-face-1                   ((,class :foreground ,bespoke-popout)))
   `(ivy-minibuffer-match-face-2                   ((,class :foreground ,bespoke-popout)))
   `(ivy-minibuffer-match-face-3                   ((,class :foreground ,bespoke-popout)))
   `(ivy-minibuffer-match-face-4                   ((,class :foreground ,bespoke-popout)))
   `(ivy-minibuffer-match-highlight                ((,class :foreground ,bespoke-strong)))
   `(ivy-modified-buffer                           ((,class :foreground ,bespoke-popout)))
   `(ivy-modified-outside-buffer                   ((,class :foreground ,bespoke-strong)))
   `(ivy-org                                       ((,class :foreground ,bespoke-faded)))
   `(ivy-prompt-match                              ((,class :foreground ,bespoke-faded)))
   `(ivy-remote                                    ((,class :inherit default)))
   `(ivy-separator                                 ((,class :foreground ,bespoke-faded)))
   `(ivy-subdir                                    ((,class :foreground ,bespoke-faded)))
   `(ivy-virtual                                   ((,class :foreground ,bespoke-faded)))
   `(ivy-yanked-word                               ((,class :foreground ,bespoke-faded)))

;;;;;; Ido
   `(ido-first-match                               ((,class :foreground ,bespoke-salient)))
   `(ido-only-match                                ((,class :foreground ,bespoke-faded)))
   `(ido-subdir                                    ((,class :foreground ,bespoke-strong)))

;;;;;; Selectrum
   `(selectrum-current-candidate                   ((,class :weight bold :background ,bespoke-highlight)))
   `(selectrum-prescient-secondary-highlight       ((,class :weight bold :foreground ,bespoke-blue)))
   `(selectrum-prescient-primary-highlight         ((,class :weight bold :foreground ,bespoke-salient)))
   `(selectrum-completion-docsig                   ((,class :slant  italic :inherit selectrum-completion-annotation)))
   `(selectrum-completion-annotation               ((,class :inherit completions-annotations)))
   `(selectrum-group-separator                     ((,class :strike-through t :inherit shadow)))
   `(selectrum-group-title                         ((,class :slant  italic :inherit shadow)))
   `(selectrum-quick-keys-match                    ((,class :inherit isearch)))
   `(selectrum-quick-keys-highlight                ((,class :foreground ,bespoke-popout)))

;;;;;; Vertico
   `(vertico-current                               ((,class :weight regular :background ,bespoke-highlight)))

;;;;;; Orderless

   `(orderless-match-face-0                        ((,class :weight bold :foreground ,bespoke-yellow)))
   `(orderless-match-face-1                        ((,class :weight bold :foreground ,bespoke-yellow)))
   `(orderless-match-face-2                        ((,class :weight bold :foreground ,bespoke-yellow)))
   `(orderless-match-face-3                        ((,class :weight bold :foreground ,bespoke-yellow)))



;;;;; Customize
   `(widget-field                                  ((,class :background ,bespoke-subtle)))
   `(widget-button                                 ((,class :foreground ,bespoke-foreground :bold t)))
   `(widget-single-line-field                      ((,class :background ,bespoke-subtle)))
   `(custom-group-subtitle                         ((,class :foreground ,bespoke-foreground :bold t)))
   `(custom-group-tag                              ((,class :foreground ,bespoke-foreground :bold t)))
   `(custom-group-tag-1                            ((,class :foreground ,bespoke-foreground :bold t)))
   `(custom-comment                                ((,class :foreground ,bespoke-faded)))
   `(custom-comment-tag                            ((,class :foreground ,bespoke-faded)))
   `(custom-changed                                ((,class :foreground ,bespoke-salient)))
   `(custom-modified                               ((,class :foreground ,bespoke-salient)))
   `(custom-face-tag                               ((,class :foreground ,bespoke-foreground :bold t)))
   `(custom-variable-tag                           ((,class :foreground ,bespoke-foreground :bold t)))
   `(custom-invalid                                ((,class :foreground ,bespoke-popout)))
   `(custom-visibility                             ((,class :foreground ,bespoke-salient)))
   `(custom-state                                  ((,class :foreground ,bespoke-salient)))
   `(custom-link                                   ((,class :foreground ,bespoke-salient)))
   `(custom-button                                 ((,class :foreground ,bespoke-faded :background ,bespoke-background :box `(:line-width 1 :color ,(face-foreground 'bespoke-faded) :style nil))))
   `(custom-button-mouse                           ((,class :foreground ,bespoke-faded :background ,bespoke-subtle :box `(:line-width 1 :color ,(face-foreground 'bespoke-faded) :style nil))))
   `(custom-button-pressed                         ((,class :foreground ,bespoke-foreground :background ,bespoke-salient :inverse-video nil :box `(:line-width 1 :color ,(face-foreground 'bespoke-salient) :style nil))))

;;;;; Deft
   `(deft-filter-string-error-face                 ((,class :foreground ,bespoke-popout)))
   `(deft-filter-string-face                       ((,class :foreground ,bespoke-yellow)))
   `(deft-header-face                              ((,class :foreground ,bespoke-salient)))
   `(deft-separator-face                           ((,class :foreground ,bespoke-faded)))
   `(deft-summary-face                             ((,class :foreground ,bespoke-faded)))
   `(deft-time-face                                ((,class :foreground ,bespoke-salient)))
   `(deft-title-face                               ((,class :foreground ,bespoke-strong :weight semi-bold)))

;;;;; Diff
   `(diff-header                                   ((,class :foreground ,bespoke-faded)))
   `(diff-file-header                              ((,class :foreground ,bespoke-strong)))
   `(diff-context                                  ((,class :inherit    default)))
   `(diff-removed                                  ((,class :foreground ,bespoke-faded)))
   `(diff-changed                                  ((,class :foreground ,bespoke-popout)))
   `(diff-added                                    ((,class :foreground ,bespoke-salient)))
   `(diff-refine-added                             ((,class :foreground ,bespoke-strong)))
   `(diff-refine-changed                           ((,class :foreground ,bespoke-popout)))
   `(diff-refine-removed                           ((,class :foreground ,bespoke-faded :strike-through t)))
   `(magit-section-highlight                       ((,class :background ,bespoke-subtle)))


;;;;; Dired
;;;;;; All The Icons Dired
   `(all-the-icons-dired-dir-face                  ((,class :foreground ,bespoke-salient)))

;;;;;; Dired (plus)
   `(diredp-write-priv                             ((,class :foreground ,bespoke-critical)))
   `(diredp-tagged-autofile-name                   ((,class :foreground ,bespoke-background)))
   `(diredp-symlink                                ((,class :foreground ,bespoke-popout)))
   `(diredp-read-priv                              ((,class :foreground ,bespoke-popout)))
   `(diredp-rare-priv                              ((,class :foreground ,bespoke-popout :background ,bespoke-critical)))
   `(diredp-other-priv                             ((,class :background ,bespoke-red)))
   `(diredp-omit-file-name                         ((,class :strike-through ,bespoke-faded :inherit diredp-ignored-file-name)))
   `(diredp-number                                 ((,class :foreground ,bespoke-salient)))
   `(diredp-no-priv                                ((,class :foreground ,bespoke-critical)))
   `(diredp-mode-line-flagged                      ((,class :foreground ,bespoke-critical)))
   `(diredp-mode-line-marked                       ((,class :foreground ,bespoke-salient)))
   `(diredp-link-priv                              ((,class :foreground ,bespoke-popout)))
   `(diredp-ignored-file-name                      ((,class :foreground ,bespoke-faded)))
   `(diredp-flag-mark-line                         ((,class :foreground ,bespoke-popout)))
   `(diredp-flag-mark                              ((,class :foreground ,bespoke-popout :background ,bespoke-salient)))
   `(diredp-file-suffix                            ((,class :foreground ,bespoke-faded)))
   `(diredp-file-name                              ((,class :foreground ,bespoke-foreground)))
   `(diredp-executable-tag                         ((,class :foreground ,bespoke-critical)))
   `(diredp-exec-priv                              ((,class :foreground ,bespoke-critical)))
   `(diredp-dir-priv                               ((,class :foreground ,bespoke-faded)))
   `(diredp-dir-name                               ((,class :foreground ,bespoke-green)))
   `(diredp-dir-heading                            ((,class :inherit ,(if bespoke-set-variable-pitch 'variable-pitch 'default) :foreground ,bespoke-blue :background ,bespoke-subtle)))
   `(diredp-deletion-file-name                     ((,class :foreground ,bespoke-critical)))
   `(diredp-deletion                               ((,class :foreground ,bespoke-popout :background ,bespoke-critical)))
   `(diredp-date-time                              ((,class :foreground ,bespoke-faded)))
   `(diredp-compressed-file-suffix                 ((,class :foreground ,bespoke-faded)))
   `(diredp-compressed-file-name                   ((,class :foreground ,bespoke-background)))
   `(diredp-autofile-name                          ((,class :background ,bespoke-subtle)))

;;;;;; Dired Colors (Diredfl)
   `(diredfl-write-priv                            ((,class :foreground ,bespoke-critical)))
   `(diredfl-tagged-autofile-name                  ((,class :foreground ,bespoke-background)))
   `(diredfl-symlink                               ((,class :foreground ,bespoke-popout)))
   `(diredfl-read-priv                             ((,class :foreground ,bespoke-popout)))
   `(diredfl-rare-priv                             ((,class :foreground ,bespoke-popout :background ,bespoke-critical)))
   `(diredfl-other-priv                            ((,class :background ,bespoke-red)))
   `(diredfl-omit-file-name                        ((,class :strike-through ,bespoke-faded :inherit diredp-ignored-file-name)))
   `(diredfl-number                                ((,class :foreground ,bespoke-salient)))
   `(diredfl-no-priv                               ((,class :foreground ,bespoke-critical)))
   `(diredfl-mode-line-flagged                     ((,class :foreground ,bespoke-critical)))
   `(diredfl-mode-line-marked                      ((,class :foreground ,bespoke-salient)))
   `(diredfl-link-priv                             ((,class :foreground ,bespoke-popout)))
   `(diredfl-ignored-file-name                     ((,class :foreground ,bespoke-faded)))
   `(diredfl-flag-mark-line                        ((,class :foreground ,bespoke-popout)))
   `(diredfl-flag-mark                             ((,class :foreground ,bespoke-popout :background ,bespoke-salient)))
   `(diredfl-file-suffix                           ((,class :foreground ,bespoke-faded)))
   `(diredfl-file-name                             ((,class :foreground ,bespoke-foreground)))
   `(diredfl-executable-tag                        ((,class :foreground ,bespoke-critical)))
   `(diredfl-exec-priv                             ((,class :foreground ,bespoke-critical)))
   `(diredfl-dir-priv                              ((,class :foreground ,bespoke-faded)))
   `(diredfl-dir-name                              ((,class :foreground ,bespoke-green)))
   `(diredfl-dir-heading                           ((,class :inherit ,(if bespoke-set-variable-pitch 'variable-pitch 'default) :foreground ,bespoke-blue :background ,bespoke-subtle)))
   `(diredfl-deletion-file-name                    ((,class :foreground ,bespoke-critical)))
   `(diredfl-deletion                              ((,class :foreground ,bespoke-popout :background ,bespoke-critical)))
   `(diredfl-date-time                             ((,class :foreground ,bespoke-faded)))
   `(diredfl-compressed-file-suffix                ((,class :foreground ,bespoke-faded)))
   `(diredfl-compressed-file-name                  ((,class :foreground ,bespoke-background)))
   `(diredfl-autofile-name                         ((,class :background ,bespoke-subtle)))

;;;;; Flyspell
   `(flyspell-duplicate                            ((,class :foreground ,bespoke-red)))
   `(flyspell-incorrect                            ((,class :foreground ,bespoke-critical)))

;;;;; Font Lock
   `(font-lock-comment-face                        ((,class :foreground ,bespoke-faded :slant ,(if bespoke-set-italic-comments 'italic 'normal))))
   `(font-lock-comment-delimiter-face              ((,class :foreground ,bespoke-faded :weight bold :slant ,(if bespoke-set-italic-comments 'italic 'normal))))
   `(font-lock-doc-face                            ((,class :foreground ,bespoke-faded)))
   `(font-lock-string-face                         ((,class :foreground ,bespoke-popout)))
   `(font-lock-constant-face                       ((,class :foreground ,bespoke-green)))
   `(font-lock-builtin-face                        ((,class :foreground ,bespoke-green)))
   `(font-lock-function-name-face                  ((,class :foreground ,bespoke-strong :weight semi-bold)))
   `(font-lock-variable-name-face                  ((,class :foreground ,bespoke-yellow)))
   `(font-lock-type-face                           ((,class :foreground ,bespoke-salient)))
   `(font-lock-keyword-face                        ((,class :foreground ,bespoke-salient :slant ,(if bespoke-set-italic-keywords 'italic 'normal))))
   `(font-lock-reference-face                      ((,class :foreground ,bespoke-salient)))
   `(font-lock-warning-face                        ((,class :foreground ,bespoke-critical)))
   `(font-lock-regexp-grouping-backslash           ((,class :foreground ,bespoke-critical)))
   `(font-lock-regexp-grouping-construct           ((,class :foreground ,bespoke-critical)))

;;;;; Git
;;;;;; Git-gutter
   `(git-gutter:added        ((,class :foreground ,bespoke-green)))
   `(git-gutter:deleted      ((,class :foreground ,bespoke-red)))
   `(git-gutter:modified     ((,class :foreground ,bespoke-popout)))
   `(git-gutter:separator    ((,class :foreground ,bespoke-subtle)))
   `(git-gutter:unchanged    ((,class :foreground ,bespoke-background)))
;;;;;; Git-gutter-fr
   `(git-gutter-fr:added        ((,class :foreground ,bespoke-green)))
   `(git-gutter-fr:deleted      ((,class :foreground ,bespoke-red)))
   `(git-gutter-fr:modified     ((,class :foreground ,bespoke-popout)))

;;;;; Goggles
   `(goggles-added   ((,class :background ,bespoke-green)))
   `(goggles-changed ((,class :background ,bespoke-popout)))
   `(goggles-removed ((,class :background ,bespoke-red)))

;;;;; Help(ful)

   `(helpful-heading ((,class :inherit ,(if bespoke-set-variable-pitch 'variable-pitch 'default) :foreground ,bespoke-blue :height 1.25)))


;;;;; Highlight-Indentation
   `(highlight-indentation-face ((,class :inherit ,bespoke-highlight)))
   `(highlight-indentation-current-column-face ((,class :background ,bespoke-yellow)))

;;;;; Highlight Indentation Guides
   `(highlight-indent-guides-stack-odd-face        ((,class :foreground ,bespoke-brown)))
   `(highlight-indent-guides-stack-even-face       ((,class :foreground ,bespoke-yellow)))
   `(highlight-indent-guides-top-odd-face          ((,class :foreground ,bespoke-brown)))
   `(highlight-indent-guides-top-even-face         ((,class :foreground ,bespoke-yellow)))
   `(highlight-indent-guides-odd-face              ((,class :foreground ,bespoke-brown)))
   `(highlight-indent-guides-even-face             ((,class :foreground ,bespoke-yellow)))
   `(highlight-indent-guides-character-face        ((,class :foreground ,bespoke-highlight)))
   `(highlight-indent-guides-top-character-face    ((,class :foreground ,bespoke-highlight)))
   `(highlight-indent-guides-stack-character-face  ((,class :foreground ,bespoke-highlight)))

;;;;; Imenu List
   `(imenu-list-entry-face-0                       ((,class :inherit imenu-list-entry-face :foreground ,bespoke-faded)))
   `(imenu-list-entry-face-1                       ((,class :inherit imenu-list-entry-face :foreground ,bespoke-faded)))
   `(imenu-list-entry-face-2                       ((,class :inherit imenu-list-entry-face :foreground ,bespoke-faded)))
   `(imenu-list-entry-face-3                       ((,class :inherit imenu-list-entry-face :foreground ,bespoke-faded)))

;;;;; Info (Documentation)
   `(info-menu-header                              ((,class :foreground ,bespoke-strong)))
   `(info-header-node                              ((,class :foreground ,bespoke-green)))
   `(info-index-match                              ((,class :foreground ,bespoke-salient)))
   `(Info-quoted                                   ((,class :foreground ,bespoke-faded)))
   `(info-title-1                                  ((,class :foreground ,bespoke-strong)))
   `(info-title-2                                  ((,class :foreground ,bespoke-strong)))
   `(info-title-3                                  ((,class :foreground ,bespoke-strong)))
   `(info-title-4                                  ((,class :foreground ,bespoke-strong)))

;;;;; Interface
   `(widget-field                                  ((,class :background ,bespoke-subtle)))
   `(widget-button                                 ((,class :foreground ,bespoke-strong)))
   `(widget-single-line-field                      ((,class :foreground ,bespoke-subtle)))
   `(custom-group-subtitle                         ((,class :foreground ,bespoke-strong)))
   `(custom-group-tag                              ((,class :foreground ,bespoke-strong)))
   `(custom-group-tag-1                            ((,class :foreground ,bespoke-strong)))
   `(custom-comment                                ((,class :foreground ,bespoke-faded)))
   `(custom-comment-tag                            ((,class :foreground ,bespoke-faded)))
   `(custom-changed                                ((,class :foreground ,bespoke-salient)))
   `(custom-modified                               ((,class :foreground ,bespoke-salient)))
   `(custom-face-tag                               ((,class :foreground ,bespoke-strong)))
   `(custom-variable-tag                           ((,class :inherit    default)))
   `(custom-invalid                                ((,class :foreground ,bespoke-popout)))
   `(custom-visibility                             ((,class :foreground ,bespoke-salient)))
   `(custom-state                                  ((,class :foreground ,bespoke-salient)))
   `(custom-link                                   ((,class :foreground ,bespoke-salient)))

;;;;; Markdown Mode
   `(markdown-blockquote-face                      ((,class :foreground ,bespoke-salient)))
   `(markdown-bold-face                            ((,class :foreground ,bespoke-strong :weight bold)))
   `(markdown-code-face                            ((,class :inherit    default)))
   `(markdown-comment-face                         ((,class :foreground ,bespoke-faded)))
   `(markdown-footnote-marker-face                 ((,class :inherit    default)))
   `(markdown-footnote-text-face                   ((,class :inherit    default)))
   `(markdown-gfm-checkbox-face                    ((,class :inherit    default)))
   `(markdown-header-delimiter-face                ((,class :foreground ,bespoke-faded)))
   `(markdown-header-face                          ((,class :inherit ,(if bespoke-set-variable-pitch 'variable-pitch 'default))))
   `(markdown-header-face-1                        ((,class :inherit outline-1)))
   `(markdown-header-face-2                        ((,class :inherit outline-2)))
   `(markdown-header-face-3                        ((,class :inherit outline-1)))
   `(markdown-header-face-4                        ((,class :inherit outline-2)))
   `(markdown-header-face-5                        ((,class :inherit outline-1)))
   `(markdown-header-face-6                        ((,class :inherit outline-2)))
   `(markdown-header-rule-face                     ((,class :inherit default)))
   `(markdown-highlight-face                       ((,class :inherit default)))
   `(markdown-hr-face                              ((,class :inherit default)))
   `(markdown-html-attr-name-face                  ((,class :inherit default)))
   `(markdown-html-attr-value-face                 ((,class :inherit default)))
   `(markdown-html-entity-face                     ((,class :inherit default)))
   `(markdown-html-tag-delimiter-face              ((,class :inherit default)))
   `(markdown-html-tag-name-face                   ((,class :inherit default)))
   `(markdown-inline-code-face                     ((,class :foreground ,bespoke-popout)))
   `(markdown-italic-face                          ((,class :foreground ,bespoke-strong :slant italic)))
   `(markdown-language-info-face                   ((,class :inherit   default)))
   `(markdown-language-keyword-face                ((,class :inherit   default)))
   `(markdown-line-break-face                      ((,class :inherit   default)))
   `(markdown-link-face                            ((,class :foreground ,bespoke-salient)))
   `(markdown-link-title-face                      ((,class :inherit    default)))
   `(markdown-list-face                            ((,class :foreground ,bespoke-faded)))
   `(markdown-markup-face                          ((,class :foreground ,bespoke-faded)))
   `(markdown-math-face                            ((,class :inherit    default)))
   `(markdown-metadata-key-face                    ((,class :foreground ,bespoke-faded)))
   `(markdown-metadata-value-face                  ((,class :foreground ,bespoke-faded)))
   `(markdown-missing-link-face                    ((,class :inherit    default)))
   `(markdown-plain-url-face                       ((,class :inherit    default)))
   `(markdown-pre-face                             ((,class :inherit    default)))
   `(markdown-reference-face                       ((,class :foreground ,bespoke-salient)))
   `(markdown-strike-through-face                  ((,class :foreground ,bespoke-faded)))
   `(markdown-table-face                           ((,class :inherit    default)))
   `(markdown-url-face                             ((,class :foreground ,bespoke-salient)))

;;;;; Magit
   `(magit-branch-current      ((,class :foreground ,bespoke-salient :box t :weight semi-bold)))
   `(magit-branch-local        ((,class :foreground ,bespoke-salient :weight semi-bold)))
   `(magit-branch-remote       ((,class :foreground ,bespoke-green :weight semi-bold)))
   `(magit-branch-remote-head  ((,class :foreground ,bespoke-popout :box t)))
   `(magit-branch-upstream     ((,class :inherit italic)))
   `(magit-cherry-equivalent   ((,class :background ,bespoke-background :foreground ,bespoke-popout)))
   `(magit-cherry-unmatched ((,class :background ,bespoke-background :foreground ,bespoke-salient)))
   `(magit-head                ((,class :inherit magit-branch-local)))
   `(magit-header-line ((,class :foreground ,bespoke-foreground)))
   `(magit-header-line-key ((,class :foregrond ,bespoke-green)))
   `(magit-header-line-log-select ((,class :foreground ,bespoke-foreground)))
   `(magit-keyword ((,class :foreground ,bespoke-popout)))
   `(magit-keyword-squash ((,class :inherit bold :foreground ,bespoke-yellow)))
   `(magit-section ((,class :background ,bespoke-subtle :foreground ,bespoke-foreground)))
   `(magit-section-heading     ((,class :weight semi-bold :foreground ,bespoke-yellow)))
   `(magit-section-heading-selection ((,class :foreground ,bespoke-salient)))
   `(magit-section-highlight ((,class :background ,bespoke-highlight)))
   `(magit-tag                 ((,class :foreground ,bespoke-yellow)))
   `(magit-header-line         ((,class :foreground ,bespoke-foreground
                                        :background ,bespoke-modeline
                                        :box (:line-width (if (fboundp 'bespoke-modeline) bespoke-modeline-size 3))
                                        :color ,bespoke-modeline
                                        :style nil)
                                :overline nil
                                :underline nil))
   `(magit-header-line-log-select ((,class :foreground ,bespoke-foreground
                                           :background ,bespoke-modeline
                                           :box (:line-width (if (fboundp 'bespoke-modeline) bespoke-modeline-size 3))
                                           :color ,bespoke-modeline
                                           :style nil)
                                   :overline nil
                                   :underline nil))

;;;;; Message
   `(message-cited-text                            ((,class :foreground ,bespoke-faded)))
   `(message-header-cc                             ((,class :inherit default)))
   `(message-header-name                           ((,class :foreground ,bespoke-strong)))
   `(message-header-newsgroups                     ((,class :inherit default)))
   `(message-header-other                          ((,class :inherit default)))
   `(message-header-subject                        ((,class :foreground ,bespoke-salient)))
   `(message-header-to                             ((,class :foreground ,bespoke-salient)))
   `(message-header-xheader                        ((,class :inherit default)))
   `(message-mml                                   ((,class :foreground ,bespoke-popout)))
   `(message-separator                             ((,class :foreground ,bespoke-faded)))

;;;;; Meow
   `(meow-normal-cursor         ((,class :background ,bespoke-yellow)))
   `(meow-insert-cursor         ((,class :background ,bespoke-critical)))
   `(meow-keypad-cursor         ((,class :background ,bespoke-brown)))
   `(meow-motion-cursor         ((,class :background ,bespoke-green)))
   `(meow-kmacro-cursor         ((,class :background ,bespoke-salient)))
   `(meow-beacon-cursor         ((,class :background ,bespoke-yellow)))
   `(meow-beacon-fake-selection ((,class :background ,bespoke-modeline)))
   `(meow-beacon-fake-cursor    ((,class :background ,bespoke-yellow)))

;;;;; Mode line/Header line
;;;;;; Conditional Loading
   ;; NOTE: these settings are specifically for bespoke-modeline
   ;; See https://github.com/mclear-tools/bespoke-modeline
   ;; Mode line settings based on position
   (when (fboundp 'bespoke-modeline)
     (when (eq bespoke-modeline-position 'top)
       `(header-line ((,class :foreground ,bespoke-foreground
                              :background ,bespoke-modeline
                              :box (:line-width ,bespoke-modeline-size
                                    :color ,bespoke-modeline
                                    :style nil)
                              :overline nil
                              :underline nil)))))

   (when (fboundp 'bespoke-modeline)
     (when (eq bespoke-modeline-position 'top)
       `(mode-line  ((,class :height 0.1
                             :underline ,bespoke-subtle
                             :overline nil
                             :box nil)))))


   (when (fboundp 'bespoke-modeline)
     (when (eq bespoke-modeline-position 'top)
       `(mode-line-inactive  ((,class :height 0.1
                                      :underline ,bespoke-subtle
                                      :overline nil
                                      :box nil)))))


   (when (fboundp 'bespoke-modeline)
     (when (eq bespoke-modeline-position 'bottom)
       `(mode-line ((,class :foreground ,bespoke-foreground
                            :background ,bespoke-modeline
                            :box (:line-width ,bespoke-modeline-size
                                  :color ,bespoke-modeline
                                  :style nil)
                            :overline nil
                            :underline nil)))))

   (when (fboundp 'bespoke-modeline)
     (when (eq bespoke-modeline-position 'bottom)
       `(mode-line-inactive ((,class :foreground ,bespoke-subtle
                                     :background ,bespoke-modeline
                                     :box (:line-width ,bespoke-modeline-size
                                           :color ,bespoke-modeline
                                           :style nil)
                                     :overline nil
                                     :underline nil)))))

   ;; No underline in terminal
   ;; FIXME: for some reason this seems necessary
   ;; to disable underline in terminal
   (when (not (display-graphic-p))
     (set-face-attribute 'mode-line nil
                         :underline nil)
     (set-face-attribute 'mode-line-inactive nil
                         :underline nil))


   (when (fboundp 'bespoke-modeline)
     (when (eq bespoke-modeline-position nil)
       `(mode-line ((,class :foreground ,bespoke-foreground
                            :background ,bespoke-modeline
                            :box (:line-width ,bespoke-modeline-size
                                  :color ,bespoke-modeline
                                  :style nil)
                            :overline nil
                            :underline nil)))))

   (when (fboundp 'bespoke-modeline)
     (when (eq bespoke-modeline-position nil)
       `(mode-line-inactive ((,class :foreground ,bespoke-faded
                                     :background ,bespoke-modeline
                                     :box (:line-width ,bespoke-modeline-size
                                           :color ,bespoke-modeline
                                           :style nil)
                                     :overline nil
                                     :underline nil)))))

;;;;;; Mode line indicators

   ;; Active
   (when (fboundp 'bespoke-modeline)
     `(bespoke-modeline-active               ((,class (:foreground ,bespoke-foreground
                                                       :background ,bespoke-modeline
                                                       :box (:line-width ,bespoke-modeline-size
                                                             :color ,bespoke-modeline
                                                             :style nil)
                                                       :overline nil
                                                       :underline nil)))))

   `(bespoke-modeline-active-name          ((,class (:background ,bespoke-modeline
                                                     :foreground ,bespoke-foreground))))
   `(bespoke-modeline-active-primary       ((,class (:foreground ,bespoke-faded :weight light))))
   `(bespoke-modeline-active-secondary     ((,class (:foreground ,bespoke-foreground))))
   `(bespoke-modeline-active-status-RW ((,class :foreground ,bespoke-background
                                                :background ,bespoke-blue
                                                :box (:line-width 1 :color ,bespoke-blue :style nil))))

   `(bespoke-modeline-active-status-** ((,class :foreground ,bespoke-background
                                                :background ,bespoke-red
                                                :box (:line-width 1 :color ,bespoke-red :style nil))))

   `(bespoke-modeline-active-status-RO ((,class :foreground ,bespoke-background
                                                :background ,bespoke-yellow
                                                :box (:line-width 1 :color ,bespoke-yellow :style nil))))

   ;; Inactive
   (when (fboundp 'bespoke-modeline)
     `(bespoke-modeline-inactive             ((,class (:foreground ,bespoke-subtle
                                                       :background ,bespoke-modeline
                                                       :box (:line-width ,bespoke-modeline-size
                                                             :color ,bespoke-modeline
                                                             :style nil)
                                                       :overline nil
                                                       :underline nil)))))
   `(bespoke-modeline-inactive-name        ((,class (:foreground ,bespoke-faded :background ,bespoke-modeline :weight light))))
   `(bespoke-modeline-inactive-primary     ((,class (:foreground ,bespoke-faded :background ,bespoke-modeline :weight light))))
   `(bespoke-modeline-inactive-secondary   ((,class (:foreground ,bespoke-faded :background ,bespoke-modeline :weight light))))

   `(bespoke-modeline-inactive-status-RO   ((,class :foreground ,bespoke-subtle
                                                    :background ,bespoke-inactive
                                                    :box (:line-width 1
                                                          :color ,bespoke-inactive
                                                          :style nil)
                                                    :overline nil
                                                    :underline nil)))

   `(bespoke-modeline-inactive-status-RW ((,class :foreground ,bespoke-subtle
                                                  :background ,bespoke-inactive
                                                  :box (:line-width 1
                                                        :color ,bespoke-inactive
                                                        :style nil)
                                                  :overline nil
                                                  :underline nil)))

   `(bespoke-modeline-inactive-status-**  ((,class :foreground ,bespoke-subtle
                                                   :background ,bespoke-inactive
                                                   :box (:line-width 1
                                                         :color ,bespoke-inactive
                                                         :style nil)
                                                   :overline nil
                                                   :underline nil)))

   (when (not (fboundp 'bespoke-modeline))
     `(mode-line ((,class :foreground ,bespoke-foreground
                          :background ,bespoke-modeline
                          :box (:line-width 3
                                :color ,bespoke-modeline
                                :style nil)
                          :overline nil
                          :underline nil))))

   (when (not (fboundp 'bespoke-modeline))
     `(mode-line-inactive ((,class :foreground ,bespoke-faded
                                   :background ,bespoke-modeline
                                   :box (:line-width 3
                                         :color ,bespoke-modeline
                                         :style nil)
                                   :overline nil
                                   :underline nil))))

;;;;; Mu4e
   `(mu4e-attach-number-face                      ((,class :foreground ,bespoke-strong)))
   `(mu4e-cited-1-face                            ((,class :foreground ,bespoke-faded)))
   `(mu4e-cited-2-face                            ((,class :foreground ,bespoke-faded)))
   `(mu4e-cited-3-face                            ((,class :foreground ,bespoke-faded)))
   `(mu4e-cited-4-face                            ((,class :foreground ,bespoke-faded)))
   `(mu4e-cited-5-face                            ((,class :foreground ,bespoke-faded)))
   `(mu4e-cited-6-face                            ((,class :foreground ,bespoke-faded)))
   `(mu4e-cited-7-face                            ((,class :foreground ,bespoke-faded)))
   `(mu4e-compose-header-face                     ((,class :foreground ,bespoke-faded)))
   `(mu4e-compose-separator-face                  ((,class :foreground ,bespoke-faded)))
   `(mu4e-contact-face                            ((,class :foreground ,bespoke-salient)))
   `(mu4e-context-face                            ((,class :foreground ,bespoke-faded)))
   `(mu4e-draft-face                              ((,class :foreground ,bespoke-faded :weight light :slant italic)))
   `(mu4e-flagged-face                            ((,class :foreground ,bespoke-yellow)))
   `(mu4e-footer-face                             ((,class :foreground ,bespoke-faded)))
   `(mu4e-forwarded-face                          ((,class :inherit    default)))
   `(mu4e-header-face                             ((,class :inherit    default)))
   `(mu4e-header-highlight-face                   ((,class :inherit highlight)))
   `(mu4e-header-key-face                         ((,class :foreground ,bespoke-strong :weight bold)))
   `(mu4e-header-marks-face                       ((,class :foreground ,bespoke-faded)))
   `(mu4e-header-title-face                       ((,class :foreground ,bespoke-strong)))
   `(mu4e-header-value-face                       ((,class :inherit    default)))
   `(mu4e-highlight-face                          ((,class :foreground ,bespoke-salient)))
   `(mu4e-link-face                               ((,class :foreground ,bespoke-salient)))
   `(mu4e-modeline-face                           ((,class :foreground ,bespoke-modeline)))
   `(mu4e-moved-face                              ((,class :foreground ,bespoke-faded)))
   `(mu4e-ok-face                                 ((,class :foreground ,bespoke-faded)))
   `(mu4e-region-code                             ((,class :foreground ,bespoke-faded)))
   `(mu4e-replied-face                            ((,class :foreground ,bespoke-popout)))
   `(mu4e-special-header-value-face               ((,class :inherit    default)))
   `(mu4e-system-face                             ((,class :foreground ,bespoke-faded)))
   `(mu4e-title-face                              ((,class :weight bold :foreground ,bespoke-popout)))
   `(mu4e-trashed-face                            ((,class :foreground ,bespoke-inactive :weight light)))
   `(mu4e-unread-face                             ((,class :inherit    bold)))
   `(mu4e-url-number-face                         ((,class :foreground ,bespoke-faded)))
   `(mu4e-view-body-face                          ((,class :inherit    default)))
   `(mu4e-warning-face                            ((,class :foreground ,bespoke-critical)))

;;;;; Org-agenda
   `(org-agenda-calendar-event                    ((,class :inherit default)))
   `(org-agenda-calendar-sexp                     ((,class :foreground ,bespoke-faded)))
   `(org-agenda-clocking                          ((,class :foreground ,bespoke-faded)))
   `(org-agenda-column-dateline                   ((,class :foreground ,bespoke-faded)))
   `(org-agenda-current-time                      ((,class :foreground ,bespoke-faded)))
   `(org-agenda-date                              ((,class :foreground ,bespoke-salient)))
   `(org-agenda-date-today                        ((,class :inherit ,(if bespoke-set-variable-pitch 'variable-pitch 'default) :height 1.25 :foreground ,bespoke-blue)))
   `(org-super-agenda-header                      ((,class :inherit ,(if bespoke-set-variable-pitch 'variable-pitch 'default) :foreground ,bespoke-blue)))
   `(org-agenda-date-weekend                      ((,class :foreground ,bespoke-faded)))
   `(org-agenda-diary                             ((,class :foreground ,bespoke-faded)))
   `(org-agenda-dimmed-todo-face                  ((,class :foreground ,bespoke-faded)))
   `(org-agenda-done                              ((,class :foreground ,bespoke-faded :strike-through t)))
   `(org-agenda-filter-category                   ((,class :foreground ,bespoke-faded)))
   `(org-agenda-filter-effort                     ((,class :foreground ,bespoke-faded)))
   `(org-agenda-filter-regexp                     ((,class :foreground ,bespoke-faded)))
   `(org-agenda-filter-tags                       ((,class :foreground ,bespoke-faded)))
   `(org-agenda-restriction-lock                  ((,class :foreground ,bespoke-faded)))
   `(org-agenda-structure                         ((,class :foreground ,bespoke-faded)))

;;;;; Org mode
   `(org-archived                                 ((,class :foreground ,bespoke-faded)))
   `(org-block                                    ((,class :foreground ,bespoke-faded)))
   `(org-block-begin-line                         ((,class :foreground ,bespoke-faded)))
   `(org-block-end-line                           ((,class :foreground ,bespoke-faded)))
   `(org-checkbox                                 ((,class :foreground ,bespoke-faded)))
   `(org-checkbox-statistics-done                 ((,class :foreground ,bespoke-faded)))
   `(org-checkbox-statistics-todo                 ((,class :foreground ,bespoke-faded)))
   `(org-cite                                     ((,class :foreground ,bespoke-salient)))
   `(org-cite-key                                 ((,class :foreground ,bespoke-green)))
   `(org-clock-overlay                            ((,class :foreground ,bespoke-faded)))
   `(org-code                                     ((,class :foreground ,bespoke-faded)))
   `(org-column                                   ((,class :foreground ,bespoke-faded)))
   `(org-column-title                             ((,class :foreground ,bespoke-faded)))
   `(org-date                                     ((,class :foreground ,bespoke-faded)))
   `(org-date-selected                            ((,class :foreground ,bespoke-faded)))
   `(org-default                                  ((,class :foreground ,bespoke-faded)))
   `(org-document-info                            ((,class :foreground ,bespoke-faded :weight light)))
   `(org-document-info-keyword                    ((,class :foreground ,bespoke-faded :weight light)))
   `(org-document-title                           ((,class :inherit ,(if bespoke-set-variable-pitch 'variable-pitch 'default) :height 1.1 :foreground ,bespoke-salient)))
   `(org-done                                     ((,class :foreground ,bespoke-faded :strike-through t)))
   `(org-drawer                                   ((,class :foreground ,bespoke-faded :weight light)))
   `(org-ellipsis                                 ((,class :foreground ,bespoke-faded)))
   `(org-footnote                                 ((,class :foreground ,bespoke-faded)))
   `(org-formula                                  ((,class :foreground ,bespoke-faded)))
   `(org-habit-alert-face                         ((,class :inherit default)))
   `(org-headline-done                            ((,class :foreground ,bespoke-faded)))
   `(org-latex-and-related                        ((,class :foreground ,bespoke-faded)))
   `(org-level-1                                  ((,class :inherit 'outline-1)))
   `(org-level-2                                  ((,class :inherit 'outline-2)))
   `(org-level-3                                  ((,class :inherit 'outline-3)))
   `(org-level-4                                  ((,class :inherit 'outline-4)))
   `(org-level-5                                  ((,class :inherit 'outline-5)))
   `(org-level-6                                  ((,class :inherit 'outline-6)))
   `(org-level-7                                  ((,class :inherit 'outline-7)))
   `(org-level-8                                  ((,class :inherit 'outline-8)))
   `(org-link                                     ((,class :foreground ,bespoke-salient)))
   `(org-list-dt                                  ((,class :foreground ,bespoke-blue)))
   `(org-macro                                    ((,class :foreground ,bespoke-faded)))
   `(org-meta-line                                ((,class :foreground ,bespoke-faded :weight light)))
   `(org-mode-line-clock                          ((,class :foreground ,bespoke-faded)))
   `(org-mode-line-clock-overrun                  ((,class :foreground ,bespoke-faded)))
   `(org-priority                                 ((,class :foreground ,bespoke-faded)))
   `(org-property-value                           ((,class :foreground ,bespoke-faded :weight light)))
   `(org-quote                                    ((,class :foreground ,bespoke-salient)))
   `(org-scheduled                                ((,class :foreground ,bespoke-salient)))
   `(org-scheduled-previously                     ((,class :foreground ,bespoke-salient)))
   `(org-scheduled-today                          ((,class :foreground ,bespoke-salient)))
   `(org-sexp-date                                ((,class :foreground ,bespoke-faded)))
   `(org-special-keyword                          ((,class :foreground ,bespoke-faded :weight light)))
   `(org-table                                    ((,class :inherit    default)))
   `(org-tag                                      ((,class :foreground ,bespoke-faded)))
   `(org-tag-group                                ((,class :foreground ,bespoke-faded)))
   `(org-target                                   ((,class :foreground ,bespoke-faded)))
   `(org-time-grid                                ((,class :foreground ,bespoke-faded)))
   `(org-todo                                     ((,class :weight normal :foreground ,bespoke-yellow)))
   `(org-upcoming-deadline                        ((,class :foreground ,bespoke-strong)))
   `(org-upcoming-distant-deadline                ((,class :foreground ,bespoke-foreground)))
   `(org-verbatim                                 ((,class :foreground ,bespoke-faded)))
   `(org-verse                                    ((,class :foreground ,bespoke-faded)))
   `(org-warning                                  ((,class :foreground ,bespoke-popout)))

;;;;; Outline
   `(outline-minor-0      ((,class :background ,bespoke-highlight)))
   `(outline-1            ((,class :inherit ,(if bespoke-set-variable-pitch 'variable-pitch 'default) :weight normal :foreground ,bespoke-green)))
   `(outline-2            ((,class :inherit ,(if bespoke-set-variable-pitch 'variable-pitch 'default) :weight normal :foreground ,bespoke-blue)))
   `(outline-3            ((,class :inherit ,(if bespoke-set-variable-pitch 'variable-pitch 'default) :weight normal :foreground ,bespoke-brown)))
   `(outline-4            ((,class :inherit ,(if bespoke-set-variable-pitch 'variable-pitch 'default) :weight normal :foreground ,bespoke-yellow)))
   `(outline-5            ((,class :inherit outline-1)))
   `(outline-6            ((,class :inherit outline-2)))
   `(outline-7            ((,class :inherit outline-3)))
   `(outline-8            ((,class :inherit outline-4)))

;;;;; Rainbow Delimiters
   `(rainbow-delimiters-depth-1-face ((,class :foreground ,bespoke-popout     :weight medium)))
   `(rainbow-delimiters-depth-2-face ((,class :foreground ,bespoke-salient    :weight light)))
   `(rainbow-delimiters-depth-3-face ((,class :foreground ,bespoke-brown      :weight light)))
   `(rainbow-delimiters-depth-4-face ((,class :foreground ,bespoke-yellow     :weight light)))
   `(rainbow-delimiters-depth-5-face ((,class :foreground ,bespoke-green      :weight light)))
   `(rainbow-delimiters-depth-6-face ((,class :foreground ,bespoke-red        :weight light)))
   `(rainbow-delimiters-depth-7-face ((,class :foreground ,bespoke-blue       :weight light)))
   `(rainbow-delimiters-depth-8-face ((,class :foreground ,bespoke-faded      :weight light)))
   `(rainbow-delimiters-depth-9-face ((,class :foreground ,bespoke-foreground :weight light)))

;;;;; Search
   `(evil-ex-search                               ((,class :background ,bespoke-popout)))
   `(isearch                                      ((,class :background ,bespoke-popout :foreground ,bespoke-highlight :weight bold)))
   `(isearch-fail                                 ((,class :background ,bespoke-critical)))
   `(isearch-group-1                              ((,class :background ,bespoke-blue)))
   `(isearch-group-2                              ((,class :background ,bespoke-red)))
   `(query-replace                                ((,class :background ,bespoke-yellow)))

;;;;; Semantic
   `(italic                                       ((,class :slant italic)))
   `(bold                                         ((,class :foreground ,bespoke-strong :weight bold)))
   `(bold-italic                                  ((,class :foreground ,bespoke-strong :weight bold :slant italic)))
   `(underline                                    ((,class :underline t)))
   `(shadow                                       ((,class :foreground ,bespoke-faded)))
   `(success                                      ((,class :foreground ,bespoke-salient)))
   `(warning                                      ((,class :foreground ,bespoke-popout)))
   `(error                                        ((,class :foreground ,bespoke-critical)))
   `(match                                        ((,class :forgeround ,bespoke-popout :weight bold)))

;;;;; Speed Bar

   `(speedbar-button-face                         ((,class :foreground ,bespoke-faded)))
   `(speedbar-directory-face                      ((,class :foreground ,bespoke-foreground :bold t)))
   `(speedbar-file-face                           ((,class :foreground ,bespoke-foreground :background ,bespoke-background)))
   `(speedbar-highlight-face                      ((,class :foreground ,bespoke-highlight)))
   `(speedbar-selected-face                       ((,class :background ,bespoke-subtle :bold t)))
   `(speedbar-separator-face                      ((,class :foreground ,bespoke-faded)))
   `(speedbar-tag-face                            ((,class :foreground ,bespoke-faded)))

;;;;; Tabs
   `(tab-bar-echo-area-tab               ((,class :foreground ,bespoke-faded :underline t :weight bold)))
   `(tab-bar-echo-area-tab-group-current ((,class :foreground ,bespoke-faded)))

;;;;; Term
   `(term-bold                                    ((,class :foreground ,bespoke-strong :weight semi-bold)))
   `(term-color-black                             ((,class :foreground ,bespoke-background :background ,bespoke-background)))
   `(term-color-white                             ((,class :foreground ,bespoke-foreground :background ,bespoke-foreground)))
   `(term-color-blue                              ((,class :foreground ,bespoke-blue :background ,bespoke-blue)))
   `(term-color-cyan                              ((,class :foreground ,bespoke-salient :background ,bespoke-salient)))
   `(term-color-green                             ((,class :foreground ,bespoke-green :background ,bespoke-green)))
   `(term-color-magenta                           ((,class :foreground ,bespoke-popout :background ,bespoke-popout)))
   `(term-color-red                               ((,class :foreground ,bespoke-critical :background ,bespoke-critical)))
   `(term-color-yellow                            ((,class :foreground ,bespoke-yellow :background ,bespoke-yellow)))

;;;;; Window Divs
   ;; divide windows more attractively
   `(window-divider                               ((,class :foreground ,bespoke-background)))
   `(window-divider-first-pixel                   ((,class :foreground ,bespoke-background)))
   `(window-divider-last-pixel                    ((,class :foreground ,bespoke-background)))
   ;; divide windows better in terminal
   ;; see https://www.reddit.com/r/emacs/comments/3u0d0u/how_do_i_make_the_vertical_window_divider_more/
   (when (not (display-graphic-p))
     (set-face-background 'vertical-border bespoke-background)
     (set-face-foreground 'vertical-border (face-background 'vertical-border)))

;;;;; End Custom faces
   ))

;;;; Define evil cursor colors
(defun bespoke--evil-load-cursors ()
  "Load theme specific cursor colors"
  (setq evil-emacs-state-cursor    `(,bespoke-salient box))
  (setq evil-normal-state-cursor   `(,bespoke-yellow box))
  (setq evil-visual-state-cursor   `(,bespoke-faded box))
  (setq evil-insert-state-cursor   `(,bespoke-critical (bar . 2)))
  (setq evil-replace-state-cursor  `(,bespoke-critical hbar))
  (setq evil-motion-state-cursor   `(,bespoke-green box))
  (setq evil-operator-state-cursor `(,bespoke-brown hollow)))

(when bespoke-set-evil-cursors
  (add-hook 'bespoke-after-load-theme-hook #'bespoke--evil-load-cursors))

;;;; Set Hl-Todo
;; inherit faces
(setq hl-todo-keyword-faces
      '(("HOLD" .       query-replace)
        ("TODO" .       warning)
        ("NEXT" .       highlight)
        ("OKAY" .       success)
        ("DONT" .       error)
        ("FAIL" .       error)
        ("DONE" .       shadow)
        ("NOTE" .       warning)
        ("KLUDGE" .     warning)
        ("HACK" .       warning)
        ("TEMP" .       warning)
        ("FIXME" .      error)
        ("XXX+" .       error)
        ("BUG" .        error)
        ("REVIEW" .     shadow)
        ("DEPRECATED" . shadow)))

;;;; Set Minibuffer & Echo Area
(defun bespoke-theme--minibuffer ()
  "Derive minibuffer / echo area faces from bespoke faces."
  ;; Minibuffer / echo area
  (dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                        " *Minibuf-1*" " *Echo Area 1*"))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (face-remap-add-relative 'default 'fringe)))))
(bespoke-theme--minibuffer)

;;; Provide theme

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'bespoke)
(provide 'bespoke-faces-colors)


;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; End:
;;; bespoke-faces-colors.el ends here
