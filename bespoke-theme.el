;;; bespoke-theme.el --- A custom theme  -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Colin McLear
;; -------------------------------------------------------------------
;; Authors: Colin McLear
;; -------------------------------------------------------------------
;; URL: https://github.com/mclear-tools/bespoke-themes
;; -------------------------------------------------------------------
;; Version: 1
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
;; Commentary:
;; This theme started as a fork of nano-emacs.
;; See https://github.com/rougier/nano-emacs.
;; Color palatte has been expanded and face definitions revised
;; -------------------------------------------------------------------

(require 'bespoke-modeline)


;;; Define group & colors

(defgroup bespoke-themes '()
  "Faces and colors for bespoke themes")

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


;;; Define Faces
;; The themes are fully defined by these faces

;;;; Core faces
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
shade to modeline color and to the highlight color."
  :group 'faces)

;;;; Accent faces
;; The accent colors are used to fill out the color palatte. They are meant to be
;; used for attention or contrast with the core colors.

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

;;;; Modeline faces

(defface bespoke-header-default-face nil
  "Default face for ther header line."
  :group 'faces)

(defface bespoke-header-mod-face nil
  "Header line face for modified buffers."
  :group 'faces)

(defface bespoke-header-ro-face nil
  "Header line face for read-only buffers."
  :group 'faces)

;;; Define Theme
(deftheme bespoke "A custom theme for yak shaving, with light and dark variants")

;;; Set Colors

(defun bespoke-theme--light-dark (light dark)
  "Determine theme using the LIGHT or the DARK color variants of bespoke-theme."
  (if (eq bespoke-set-theme 'light)
      light
    dark))
(defalias '--l/d #'bespoke-theme--light-dark)

;; Set colors according to variant so they can be used outside the theme declaration
(defun bespoke/set-theme-variant ()
  (interactive)
  (setq bespoke-foreground (--l/d "#282b35" "#eceff4"))
  (setq bespoke-background (--l/d "#fffef9" "#282b35"))
  (setq bespoke-modeline   (--l/d "#e3e7ef" "#3c4353"))
  (setq bespoke-highlight  (--l/d "#d8dee9" "#444B5c"))

  (setq bespoke-critical   (--l/d "#f53137" "#f46715"))
  (setq bespoke-salient    (--l/d "#303db4" "#81a1c1"))
  (setq bespoke-strong     (--l/d "#000000" "#ffffff"))
  (setq bespoke-popout     (--l/d "#940b96" "#bf369a"))
  (setq bespoke-subtle     (--l/d "#eceff1" "#333a47"))
  (setq bespoke-faded      (--l/d "#8a93a8" "#7c89a2"))

  (setq bespoke-blue       (--l/d "#30608c" "#88c0d0"))
  (setq bespoke-green      (--l/d "#00796b" "#8eb89d"))
  (setq bespoke-red        (--l/d "#b71c1c" "#bf616a"))
  (setq bespoke-brown      (--l/d "#966e53" "#d08770"))
  (setq bespoke-yellow     (--l/d "#e0a500" "#e9b85d")))

;;; Customize Faces

;; Call color settings
(bespoke/set-theme-variant)

;; Declare class and set faces
(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   `bespoke
   `(default ((,class :foreground ,bespoke-foreground :background ,bespoke-background)))


;;;; Basic Faces
   `(buffer-menu-buffer                            ((,class :foreground ,bespoke-strong)))
   `(minibuffer-prompt                             ((,class :foreground ,bespoke-green)))
   `(link                                          ((,class :foreground ,bespoke-salient)))
   `(region                                        ((,class :background ,bespoke-faded)))
   `(fringe                                        ((,class :foreground ,bespoke-faded :weight light)))
   `(highlight                                     ((,class :background ,bespoke-highlight)))
   `(lazy-highlight                                ((,class :foreground ,bespoke-green)))
   `(trailing-whitespace                           ((,class :foreground ,bespoke-faded)))
   `(show-paren-match                              ((,class :foreground ,bespoke-foreground :background ,bespoke-green)))
   `(show-paren-mismatch                           ((,class :foreground ,bespoke-popout :background ,bespoke-critical)))
   `(tooltip nil                                   ((,class :height 0.85)))

;;;; Bookmarks
   `(bookmark-menu-heading                         ((,class :foreground ,bespoke-strong)))
   `(bookmark-menu-bookmark                        ((,class :foreground ,bespoke-salient)))



;;;; Completion/Narrowing

;;;;; Company
   `(company-scrollbar-fg                       ((,class :foreground ,bespoke-faded)))
   `(company-scrollbar-bg                       ((,class :foreground ,bespoke-faded)))
   `(company-preview                            ((,class :foreground ,bespoke-faded :weight bold)))
   `(company-preview-common                     ((,class :foreground ,bespoke-faded)))
   `(company-tooltip-selection                  ((,class :foreground ,bespoke-salient)))
   `(company-tooltip                            ((,class :background ,bespoke-subtle)))
   `(company-tooltip-common                     ((,class :background ,bespoke-subtle)))
   `(company-tooltip-common-selection           ((,class :foreground ,bespoke-salient)))
   `(company-tooltip-annotation                 ((,class :foreground ,bespoke-faded)))
   `(company-tooltip-annotation-selection       ((,class :foreground ,bespoke-salient)))


;;;;; Counsel
   `(counsel-active-mode ((,class :foreground ,bespoke-salient)))
   `(counsel-application-name ((,class :foreground ,bespoke-red)))
   `(counsel-key-binding ((,class :inherit default)))
   `(counsel-outline-1 ((,class :inherit org-level-1)))
   `(counsel-outline-2 ((,class :inherit org-level-2)))
   `(counsel-outline-3 ((,class :inherit org-level-3)))
   `(counsel-outline-4 ((,class :inherit org-level-4)))
   `(counsel-outline-5 ((,class :inherit org-level-5)))
   `(counsel-outline-6 ((,class :inherit org-level-6)))
   `(counsel-outline-7 ((,class :inherit org-level-7)))
   `(counsel-outline-8 ((,class :inherit org-level-8)))
   `(counsel-outline-default ((,class :foreground ,bespoke-foreground)))
   `(counsel-variable-documentation ((,class :inherit default :foreground ,bespoke-yellow)))

;;;;; Ivy
   `(ivy-action ((,class :inherit bold)))
   `(ivy-completions-annotations ((,class :inherit completions-annotations)))
   `(ivy-current-match ((,class :background ,bespoke-highlight :weight bold)))
   `(ivy-cursor ((,class :background ,bespoke-foreground :foreground ,bespoke-background)))
   `(ivy-grep-info ((,class :foreground ,bespoke-green)))
   `(ivy-grep-line-number ((,class :foreground ,bespoke-faded)))
   `(ivy-highlight-face ((,class :foreground ,bespoke-popout)))
   `(ivy-match-required-face ((,class :inherit error)))
   `(ivy-minibuffer-match-face-1 ((,class :background ,bespoke-yellow)))
   `(ivy-minibuffer-match-highlight ((,class :backgrond ,bespoke-yellow)))

;;;;; Ido
   `(ido-first-match                              ((,class :foreground ,bespoke-salient)))
   `(ido-only-match                               ((,class :foreground ,bespoke-faded)))
   `(ido-subdir                                   ((,class :foreground ,bespoke-strong)))

;;;;; Selectrum
   `(selectrum-current-candidate                ((,class :slant  italic :weight bold :background ,bespoke-highlight)))
   `(selectrum-prescient-secondary-highlight    ((,class :weight bold :foreground ,bespoke-blue)))
   `(selectrum-prescient-primary-highlight      ((,class :weight bold :foreground ,bespoke-salient)))
   `(selectrum-completion-docsig                ((,class :slant  italic :inherit selectrum-completion-annotation)))
   `(selectrum-completion-annotation            ((,class :inherit completions-annotations)))
   `(selectrum-group-separator                  ((,class :strike-through t :inherit shadow)))
   `(selectrum-group-title                      ((,class :slant  italic :inherit shadow)))
   `(selectrum-quick-keys-match                 ((,class :inherit isearch)))
   `(selectrum-quick-keys-highlight             ((,class :foreground ,bespoke-popout)))

;;;;; Vertico
   `(vertico-current                            ((,class :slant italic :weight bold :background ,bespoke-highlight)))

;;;;; Orderless

   `(orderless-match-face-0                     ((,class :weight bold :foreground ,bespoke-yellow)))
   `(orderless-match-face-1                     ((,class :weight bold :foreground ,bespoke-yellow)))
   `(orderless-match-face-2                     ((,class :weight bold :foreground ,bespoke-yellow)))
   `(orderless-match-face-3                     ((,class :weight bold :foreground ,bespoke-yellow)))


;;;; Diff
   `(diff-header                                  ((,class :foreground ,bespoke-faded)))
   `(diff-file-header                             ((,class :foreground ,bespoke-strong)))
   `(diff-context                                 ((,class :inherit    default)))
   `(diff-removed                                 ((,class :foreground ,bespoke-faded)))
   `(diff-changed                                 ((,class :foreground ,bespoke-popout)))
   `(diff-added                                   ((,class :foreground ,bespoke-salient)))
   `(diff-refine-added                            ((,class :foreground ,bespoke-strong)))
   `(diff-refine-changed                          ((,class :foreground ,bespoke-popout)))
   `(diff-refine-removed                          ((,class :foreground ,bespoke-faded :strike-through t)))
   `(magit-section-highlight                      ((,class :background ,bespoke-subtle)))


;;;; Dired
;;;;; All The Icons Dired
   `(all-the-icons-dired-dir-face                 ((,class :forground ,bespoke-salient)))

;;;;; Dired (plus)
   `(diredp-write-priv                           ((,class :foreground ,bespoke-critical)))
   `(diredp-tagged-autofile-name                 ((,class :foreground ,bespoke-background)))
   `(diredp-symlink                              ((,class :foreground ,bespoke-popout)))
   `(diredp-read-priv                            ((,class :foreground ,bespoke-popout)))
   `(diredp-rare-priv                            ((,class :foreground ,bespoke-popout :background ,bespoke-critical)))
   `(diredp-other-priv                           ((,class :background ,bespoke-red)))
   `(diredp-omit-file-name                       ((,class :strike-through ,bespoke-faded :inherit diredp-ignored-file-name)))
   `(diredp-number                               ((,class :foreground ,bespoke-salient)))
   `(diredp-no-priv                              ((,class :foreground ,bespoke-critical)))
   `(diredp-mode-line-flagged                    ((,class :foreground ,bespoke-critical)))
   `(diredp-mode-line-marked                     ((,class :foreground ,bespoke-salient)))
   `(diredp-link-priv                            ((,class :foreground ,bespoke-popout)))
   `(diredp-ignored-file-name                    ((,class :foreground ,bespoke-faded)))
   `(diredp-flag-mark-line                       ((,class :foreground ,bespoke-popout)))
   `(diredp-flag-mark                            ((,class :foreground ,bespoke-popout :background ,bespoke-salient)))
   `(diredp-file-suffix                          ((,class :foreground ,bespoke-faded)))
   `(diredp-file-name                            ((,class :foreground ,bespoke-foreground)))
   `(diredp-executable-tag                       ((,class :foreground ,bespoke-critical)))
   `(diredp-exec-priv                            ((,class :foreground ,bespoke-critical)))
   `(diredp-dir-priv                             ((,class :foreground ,bespoke-faded)))
   `(diredp-dir-name                             ((,class :foreground ,bespoke-green)))
   `(diredp-dir-heading                          ((,class :inherit ,(if bespoke-set-variable-pitch 'variable-pitch 'default) :foreground ,bespoke-blue :background ,bespoke-subtle)))
   `(diredp-deletion-file-name                   ((,class :foreground ,bespoke-critical)))
   `(diredp-deletion                             ((,class :foreground ,bespoke-popout :background ,bespoke-critical)))
   `(diredp-date-time                            ((,class :foreground ,bespoke-faded)))
   `(diredp-compressed-file-suffix               ((,class :foreground ,bespoke-faded)))
   `(diredp-compressed-file-name                 ((,class :foreground ,bespoke-background)))
   `(diredp-autofile-name                        ((,class :background ,bespoke-subtle)))

;;;;; Dired Colors (Diredfl)
   `(diredfl-write-priv                           ((,class :foreground ,bespoke-critical)))
   `(diredfl-tagged-autofile-name                 ((,class :foreground ,bespoke-background)))
   `(diredfl-symlink                              ((,class :foreground ,bespoke-popout)))
   `(diredfl-read-priv                            ((,class :foreground ,bespoke-popout)))
   `(diredfl-rare-priv                            ((,class :foreground ,bespoke-popout :background ,bespoke-critical)))
   `(diredfl-other-priv                           ((,class :background ,bespoke-red)))
   `(diredfl-omit-file-name                       ((,class :strike-through ,bespoke-faded :inherit diredp-ignored-file-name)))
   `(diredfl-number                               ((,class :foreground ,bespoke-salient)))
   `(diredfl-no-priv                              ((,class :foreground ,bespoke-critical)))
   `(diredfl-mode-line-flagged                    ((,class :foreground ,bespoke-critical)))
   `(diredfl-mode-line-marked                     ((,class :foreground ,bespoke-salient)))
   `(diredfl-link-priv                            ((,class :foreground ,bespoke-popout)))
   `(diredfl-ignored-file-name                    ((,class :foreground ,bespoke-faded)))
   `(diredfl-flag-mark-line                       ((,class :foreground ,bespoke-popout)))
   `(diredfl-flag-mark                            ((,class :foreground ,bespoke-popout :background ,bespoke-salient)))
   `(diredfl-file-suffix                          ((,class :foreground ,bespoke-faded)))
   `(diredfl-file-name                            ((,class :foreground ,bespoke-foreground)))
   `(diredfl-executable-tag                       ((,class :foreground ,bespoke-critical)))
   `(diredfl-exec-priv                            ((,class :foreground ,bespoke-critical)))
   `(diredfl-dir-priv                             ((,class :foreground ,bespoke-faded)))
   `(diredfl-dir-name                             ((,class :foreground ,bespoke-green)))
   `(diredfl-dir-heading                          ((,class :inherit ,(if bespoke-set-variable-pitch 'variable-pitch 'default) :foreground ,bespoke-blue :background ,bespoke-subtle)))
   `(diredfl-deletion-file-name                   ((,class :foreground ,bespoke-critical)))
   `(diredfl-deletion                             ((,class :foreground ,bespoke-popout :background ,bespoke-critical)))
   `(diredfl-date-time                            ((,class :foreground ,bespoke-faded)))
   `(diredfl-compressed-file-suffix               ((,class :foreground ,bespoke-faded)))
   `(diredfl-compressed-file-name                 ((,class :foreground ,bespoke-background)))
   `(diredfl-autofile-name                        ((,class :background ,bespoke-subtle)))

;;;; Flyspell
   `(flyspell-duplicate                           ((,class :foreground ,bespoke-red)))
   `(flyspell-incorrect                           ((,class :foreground ,bespoke-critical)))

;;;; Font Lock
   `(font-lock-comment-face                        ((,class :foreground ,bespoke-faded :slant ,(if bespoke-set-italic-comments 'italic 'normal))))
   `(font-lock-comment-delimiter-face              ((,class :foreground ,bespoke-faded :weight bold :slant ,(if bespoke-set-italic-comments 'italic 'normal))))
   `(font-lock-doc-face                            ((,class :foreground ,bespoke-faded)))
   `(font-lock-string-face                         ((,class :foreground ,bespoke-popout)))
   `(font-lock-constant-face                       ((,class :foreground ,bespoke-green)))
   `(font-lock-builtin-face                        ((,class :foreground ,bespoke-green)))
   `(font-lock-function-name-face                  ((,class :foreground ,bespoke-brown)))
   `(font-lock-variable-name-face                  ((,class :foreground ,bespoke-yellow)))
   `(font-lock-type-face                           ((,class :foreground ,bespoke-salient)))
   `(font-lock-keyword-face                        ((,class :foreground ,bespoke-blue :slant ,(if bespoke-set-italic-keywords 'italic 'normal))))
   `(font-lock-reference-face                      ((,class :foreground ,bespoke-blue)))
   `(font-lock-warning-face                        ((,class :foreground ,bespoke-critical)))
   `(font-lock-regexp-grouping-backslash           ((,class :foreground ,bespoke-critical)))
   `(font-lock-regexp-grouping-construct           ((,class :foreground ,bespoke-critical)))

;;;; Help(ful)

   `(helpful-heading                            ((,class :inherit ,(if bespoke-set-variable-pitch 'variable-pitch 'default) :foreground ,bespoke-blue :height 1.25)))

;;;; Imenu List
   `(imenu-list-entry-face-0 ((,class :inherit imenu-list-entry-face :foreground ,bespoke-faded)))
   `(imenu-list-entry-face-1 ((,class :inherit imenu-list-entry-face :foreground ,bespoke-faded)))
   `(imenu-list-entry-face-2 ((,class :inherit imenu-list-entry-face :foreground ,bespoke-faded)))
   `(imenu-list-entry-face-3 ((,class :inherit imenu-list-entry-face :foreground ,bespoke-faded)))

;;;; Info (Documentation)
   `(info-menu-header                              ((,class :foreground ,bespoke-strong)))
   `(info-header-node                              ((,class :foreground ,bespoke-green)))
   `(Info-quoted                                   ((,class :foreground ,bespoke-faded)))
   `(info-title-1                                  ((,class :foreground ,bespoke-strong)))
   `(info-title-2                                  ((,class :foreground ,bespoke-strong)))
   `(info-title-3                                  ((,class :foreground ,bespoke-strong)))
   `(info-title-4                                  ((,class :foreground ,bespoke-strong)))


;;;; Interface
   `(widget-field                                 ((,class :background ,bespoke-subtle)))
   `(widget-button                                ((,class :foreground ,bespoke-strong)))
   `(widget-single-line-field                     ((,class :foreground ,bespoke-subtle)))
   `(custom-group-subtitle                        ((,class :foreground ,bespoke-strong)))
   `(custom-group-tag                             ((,class :foreground ,bespoke-strong)))
   `(custom-group-tag-1                           ((,class :foreground ,bespoke-strong)))
   `(custom-comment                               ((,class :foreground ,bespoke-faded)))
   `(custom-comment-tag                           ((,class :foreground ,bespoke-faded)))
   `(custom-changed                               ((,class :foreground ,bespoke-salient)))
   `(custom-modified                              ((,class :foreground ,bespoke-salient)))
   `(custom-face-tag                              ((,class :foreground ,bespoke-strong)))
   `(custom-variable-tag                          ((,class :inherit    default)))
   `(custom-invalid                               ((,class :foreground ,bespoke-popout)))
   `(custom-visibility                            ((,class :foreground ,bespoke-salient)))
   `(custom-state                                 ((,class :foreground ,bespoke-salient)))
   `(custom-link                                  ((,class :foreground ,bespoke-salient)))

;;;; Markdown Mode
   `(markdown-blockquote-face                   ((,class :foreground ,bespoke-salient)))
   `(markdown-bold-face                         ((,class :foreground ,bespoke-strong :weight bold)))
   `(markdown-code-face                         ((,class :inherit    default)))
   `(markdown-comment-face                      ((,class :foreground ,bespoke-faded)))
   `(markdown-footnote-marker-face              ((,class :inherit    default)))
   `(markdown-footnote-text-face                ((,class :inherit    default)))
   `(markdown-gfm-checkbox-face                 ((,class :inherit    default)))
   `(markdown-header-delimiter-face             ((,class :foreground ,bespoke-faded)))
   `(markdown-header-face                       ((,class :inherit ,(if bespoke-set-variable-pitch 'variable-pitch 'default))))
   `(markdown-header-face-1                     ((,class :inherit outline-1)))
   `(markdown-header-face-2                     ((,class :inherit outline-2)))
   `(markdown-header-face-3                     ((,class :inherit outline-1)))
   `(markdown-header-face-4                     ((,class :inherit outline-2)))
   `(markdown-header-face-5                     ((,class :inherit outline-1)))
   `(markdown-header-face-6                     ((,class :inherit outline-2)))
   `(markdown-header-rule-face                  ((,class :inherit default)))
   `(markdown-highlight-face                    ((,class :inherit default)))
   `(markdown-hr-face                           ((,class :inherit default)))
   `(markdown-html-attr-name-face               ((,class :inherit default)))
   `(markdown-html-attr-value-face              ((,class :inherit default)))
   `(markdown-html-entity-face                  ((,class :inherit default)))
   `(markdown-html-tag-delimiter-face           ((,class :inherit default)))
   `(markdown-html-tag-name-face                ((,class :inherit default)))
   `(markdown-inline-code-face                  ((,class :foreground ,bespoke-popout)))
   `(markdown-italic-face                       ((,class :foreground ,bespoke-strong :slant italic)))
   `(markdown-language-info-face                ((,class :inherit   default)))
   `(markdown-language-keyword-face             ((,class :inherit   default)))
   `(markdown-line-break-face                   ((,class :inherit   default)))
   `(markdown-link-face                         ((,class :foreground ,bespoke-salient)))
   `(markdown-link-title-face                   ((,class :inherit    default)))
   `(markdown-list-face                         ((,class :foreground ,bespoke-faded)))
   `(markdown-markup-face                       ((,class :foreground ,bespoke-faded)))
   `(markdown-math-face                         ((,class :inherit    default)))
   `(markdown-metadata-key-face                 ((,class :foreground ,bespoke-faded)))
   `(markdown-metadata-value-face               ((,class :foreground ,bespoke-faded)))
   `(markdown-missing-link-face                 ((,class :inherit    default)))
   `(markdown-plain-url-face                    ((,class :inherit    default)))
   `(markdown-pre-face                          ((,class :inherit    default)))
   `(markdown-reference-face                    ((,class :foreground ,bespoke-salient)))
   `(markdown-strike-through-face               ((,class :foreground ,bespoke-faded)))
   `(markdown-table-face                        ((,class :inherit    default)))
   `(markdown-url-face                          ((,class :foreground ,bespoke-salient)))

;;;; Message
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

;;;; Mode line/Header line

   ;; Mode line settings based on header or footer line
   (when (eq bespoke-set-mode-line 'header)
     `(header-line ((,class :foreground ,bespoke-foreground
                            :background ,bespoke-modeline
                            :box (:line-width 5 :color ,bespoke-modeline :style nil)
                            :overline nil
                            :underline nil))))

   (when (eq bespoke-set-mode-line 'header)
     `(mode-line  ((,class :height 0.1
                           :underline ,bespoke-subtle
                           :overline nil
                           :box nil))))

   (if (eq bespoke-set-mode-line 'header)
       `(mode-line-inactive  ((,class :height 0.1
                                      :underline ,bespoke-subtle
                                      :overline nil
                                      :box nil)))
     `(mode-line-inactive ((,class :foreground ,bespoke-subtle
                                   :background ,bespoke-modeline
                                   :box (:line-width 4
                                         :color ,bespoke-modeline
                                         :height 150)
                                   :overline nil
                                   :underline nil
                                   :height 150))))

   (when (eq bespoke-set-mode-line 'footer)
     `(mode-line ((,class :foreground ,bespoke-foreground
                          :background ,bespoke-modeline
                          :box (:line-width 4 :color ,bespoke-modeline)
                          :overline nil
                          :underline nil))))

   (when (eq bespoke-set-mode-line nil)
     `(mode-line ((,class :foreground ,bespoke-faded
                          :background ,bespoke-modeline
                          :box (:line-width 4 :color ,bespoke-modeline)
                          :overline nil
                          :underline nil))))



   ;;; Mode line indicators

   `(bespoke-header-default-face ((,class :foreground ,bespoke-background
                                          :background ,bespoke-salient
                                          :box (:line-width 1 :color ,bespoke-salient :style nil))))

   `(bespoke-header-mod-face ((,class :foreground ,bespoke-background
                                      :background ,bespoke-red
                                      :box (:line-width 1 :color ,bespoke-red :style nil))))

   `(bespoke-header-ro-face ((,class :foreground ,bespoke-background
                                     :background ,bespoke-yellow
                                     :box (:line-width 1 :color ,bespoke-yellow :style nil))))

;;;; Mu4e
   `(mu4e-attach-number-face                    ((,class :foreground ,bespoke-strong)))
   `(mu4e-cited-1-face                          ((,class :foreground ,bespoke-faded)))
   `(mu4e-cited-2-face                          ((,class :foreground ,bespoke-faded)))
   `(mu4e-cited-3-face                          ((,class :foreground ,bespoke-faded)))
   `(mu4e-cited-4-face                          ((,class :foreground ,bespoke-faded)))
   `(mu4e-cited-5-face                          ((,class :foreground ,bespoke-faded)))
   `(mu4e-cited-6-face                          ((,class :foreground ,bespoke-faded)))
   `(mu4e-cited-7-face                          ((,class :foreground ,bespoke-faded)))
   `(mu4e-compose-header-face                   ((,class :foreground ,bespoke-faded)))
   `(mu4e-compose-separator-face                ((,class :foreground ,bespoke-faded)))
   `(mu4e-contact-face                          ((,class :foreground ,bespoke-salient)))
   `(mu4e-context-face                          ((,class :foreground ,bespoke-faded)))
   `(mu4e-draft-face                            ((,class :foreground ,bespoke-faded)))
   `(mu4e-flagged-face                          ((,class :foreground ,bespoke-faded)))
   `(mu4e-footer-face                           ((,class :foreground ,bespoke-faded)))
   `(mu4e-forwarded-face                        ((,class :inherit    default)))
   `(mu4e-header-face                           ((,class :inherit    default)))
   `(mu4e-header-highlight-face                 ((,class :foreground ,bespoke-subtle)))
   `(mu4e-header-key-face                       ((,class :foreground ,bespoke-strong)))
   `(mu4e-header-marks-face                     ((,class :foreground ,bespoke-faded)))
   `(mu4e-header-title-face                     ((,class :foreground ,bespoke-strong)))
   `(mu4e-header-value-face                     ((,class :inherit    default)))
   `(mu4e-highlight-face                        ((,class :foreground ,bespoke-popout)))
   `(mu4e-link-face                             ((,class :foreground ,bespoke-salient)))
   `(mu4e-modeline-face                         ((,class :foreground ,bespoke-faded)))
   `(mu4e-moved-face                            ((,class :foreground ,bespoke-faded)))
   `(mu4e-ok-face                               ((,class :foreground ,bespoke-faded)))
   `(mu4e-region-code                           ((,class :foreground ,bespoke-faded)))
   `(mu4e-replied-face                          ((,class :foreground ,bespoke-salient)))
   `(mu4e-special-header-value-face             ((,class :inherit    default)))
   `(mu4e-system-face                           ((,class :foreground ,bespoke-faded)))
   `(mu4e-title-face                            ((,class :foreground ,bespoke-strong)))
   `(mu4e-trashed-face                          ((,class :foreground ,bespoke-faded)))
   `(mu4e-unread-face                           ((,class :foreground ,bespoke-strong)))
   `(mu4e-url-number-face                       ((,class :foreground ,bespoke-faded)))
   `(mu4e-view-body-face                        ((,class :inherit    default)))
   `(mu4e-warning-face                          ((,class :foreground ,bespoke-faded)))

;;;; Org-agenda
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

;;;; Org mode
   `(org-archived                               ((,class :foreground ,bespoke-faded)))
   `(org-block                                  ((,class :foreground ,bespoke-faded)))
   `(org-block-begin-line                       ((,class :foreground ,bespoke-faded)))
   `(org-block-end-line                         ((,class :foreground ,bespoke-faded)))
   `(org-checkbox                               ((,class :foreground ,bespoke-faded)))
   `(org-checkbox-statistics-done               ((,class :foreground ,bespoke-faded)))
   `(org-checkbox-statistics-todo               ((,class :foreground ,bespoke-faded)))
   `(org-clock-overlay                          ((,class :foreground ,bespoke-faded)))
   `(org-code                                   ((,class :foreground ,bespoke-faded)))
   `(org-column                                 ((,class :foreground ,bespoke-faded)))
   `(org-column-title                           ((,class :foreground ,bespoke-faded)))
   `(org-date                                   ((,class :foreground ,bespoke-faded)))
   `(org-date-selected                          ((,class :foreground ,bespoke-faded)))
   `(org-default                                ((,class :foreground ,bespoke-faded)))
   `(org-document-info                          ((,class :foreground ,bespoke-faded :weight light)))
   `(org-document-info-keyword                  ((,class :foreground ,bespoke-faded :weight light)))
   `(org-document-title                         ((,class :inherit ,(if bespoke-set-variable-pitch 'variable-pitch 'default) :height 1.25 :foreground ,bespoke-salient)))
   `(org-done                                   ((,class :foreground ,bespoke-faded :strike-through t)))
   `(org-drawer                                 ((,class :foreground ,bespoke-faded :weight light)))
   `(org-ellipsis                               ((,class :foreground ,bespoke-faded)))
   `(org-footnote                               ((,class :foreground ,bespoke-faded)))
   `(org-formula                                ((,class :foreground ,bespoke-faded)))
   `(org-habit-alert-face                       ((,class :inherit default)))
   `(org-headline-done                          ((,class :foreground ,bespoke-faded)))
   `(org-latex-and-related                      ((,class :foreground ,bespoke-faded)))
   `(org-level-1                                ((,class :inherit 'outline-1)))
   `(org-level-2                                ((,class :inherit 'outline-2)))
   `(org-level-3                                ((,class :inherit 'outline-1)))
   `(org-level-4                                ((,class :inherit 'outline-2)))
   `(org-level-5                                ((,class :inherit 'outline-1)))
   `(org-level-6                                ((,class :inherit 'outline-2)))
   `(org-level-7                                ((,class :inherit 'outline-1)))
   `(org-level-8                                ((,class :inherit 'outline-2)))
   `(org-link                                   ((,class :foreground ,bespoke-salient)))
   `(org-list-dt                                ((,class :foreground ,bespoke-blue)))
   `(org-macro                                  ((,class :foreground ,bespoke-faded)))
   `(org-meta-line                              ((,class :foreground ,bespoke-faded :weight light)))
   `(org-mode-line-clock                        ((,class :foreground ,bespoke-faded)))
   `(org-mode-line-clock-overrun                ((,class :foreground ,bespoke-faded)))
   `(org-priority                               ((,class :foreground ,bespoke-faded)))
   `(org-property-value                         ((,class :foreground ,bespoke-faded :weight light)))
   `(org-quote                                  ((,class :foreground ,bespoke-salient)))
   `(org-scheduled                              ((,class :foreground ,bespoke-salient)))
   `(org-scheduled-previously                   ((,class :foreground ,bespoke-salient)))
   `(org-scheduled-today                        ((,class :foreground ,bespoke-salient)))
   `(org-sexp-date                              ((,class :foreground ,bespoke-faded)))
   `(org-special-keyword                        ((,class :foreground ,bespoke-faded :weight light)))
   `(org-table                                  ((,class :inherit    default)))
   `(org-tag                                    ((,class :foreground ,bespoke-faded)))
   `(org-tag-group                              ((,class :foreground ,bespoke-faded)))
   `(org-target                                 ((,class :foreground ,bespoke-faded)))
   `(org-time-grid                              ((,class :foreground ,bespoke-faded)))
   `(org-todo                                   ((,class :weight normal :foreground ,bespoke-yellow)))
   `(org-upcoming-deadline                      ((,class :foreground ,bespoke-strong)))
   `(org-upcoming-distant-deadline              ((,class :foreground ,bespoke-foreground)))
   `(org-verbatim                               ((,class :foreground ,bespoke-faded)))
   `(org-verse                                  ((,class :foreground ,bespoke-faded)))
   `(org-warning                                ((,class :foreground ,bespoke-popout)))

;;;; Outline
   `(outline-minor-0      ((,class :background ,bespoke-subtle)))
   `(outline-1            ((,class :inherit ,(if bespoke-set-variable-pitch 'variable-pitch 'default) :weight normal :foreground ,bespoke-green)))
   `(outline-2            ((,class :inherit ,(if bespoke-set-variable-pitch 'variable-pitch 'default) :weight normal :foreground ,bespoke-blue)))
   `(outline-3            ((,class :inherit outline-1)))
   `(outline-4            ((,class :inherit outline-2)))
   `(outline-5            ((,class :inherit outline-1)))
   `(outline-6            ((,class :inherit outline-2)))
   `(outline-7            ((,class :inherit outline-1)))
   `(outline-8            ((,class :inherit outline-2)))


;;;; Search
   `(evil-ex-search                                ((,class :background ,bespoke-popout)))
   `(isearch                                       ((,class :background ,bespoke-popout :foreground ,bespoke-strong)))
   `(isearch-fail                                  ((,class :background ,bespoke-critical)))
   `(isearch-group-1                               ((,class :background ,bespoke-blue)))
   `(isearch-group-2                               ((,class :background ,bespoke-red)))
   `(query-replace                                 ((,class :background ,bespoke-yellow)))

;;;; Semantic
   `(italic                                        ((,class (:slant italic))))
   `(bold                                          ((,class (:weight bold))))
   `(bold-italic                                   ((,class (:weight bold :slant italic))))
   `(underline                                     ((,class (:underline t))))
   `(shadow                                        ((,class :foreground ,bespoke-faded)))
   `(success                                       ((,class :foreground ,bespoke-salient)))
   `(warning                                       ((,class :foreground ,bespoke-popout)))
   `(error                                         ((,class :foreground ,bespoke-critical)))
   `(match                                         ((,class :forgeround ,bespoke-popout :weight bold)))

;;;; Term
   `(term-bold                                    ((,class :foreground ,bespoke-strong)))
   `(term-color-black                             ((,class :inherit    default)))
   `(term-color-white                             ((,class :foreground "white" :background "white")))
   `(term-color-blue                              ((,class :foreground "#42A5F5" :background "#BBDEFB")))
   `(term-color-cyan                              ((,class :foreground "#26C6DA" :background "#B2EBF2")))
   `(term-color-green                             ((,class :foreground "#66BB6A" :background "#C8E6C9")))
   `(term-color-magenta                           ((,class :foreground "#AB47BC" :background "#E1BEE7")))
   `(term-color-red                               ((,class :foreground "#EF5350" :background "#FFCDD2")))
   `(term-color-yellow                            ((,class :foreground "#a67c00" :background "#FFEE58")))

;;;; Posframe

   `(which-key-posframe                           ((,class :background ,bespoke-subtle)))
   `(which-key-posframe-border                    ((,class :background ,bespoke-subtle)))

;;;; Window Divs
   ;; divide windows more attractively
   `(window-divider             ((,class :foreground ,bespoke-background)))
   `(window-divider-first-pixel ((,class :foreground ,bespoke-background)))
   `(window-divider-last-pixel  ((,class :foreground ,bespoke-background)))

;;;; End Custom faces
   ))

;;; Define evil cursor colors
(defun bespoke-evil-load-cursors ()
  "Load theme specific cursor colors"
  (interactive)
  (setq evil-emacs-state-cursor    `(,bespoke-salient box))
  (setq evil-normal-state-cursor   `(,bespoke-yellow box))
  (setq evil-visual-state-cursor   `(,bespoke-faded box))
  (setq evil-insert-state-cursor   `(,bespoke-critical (bar . 2)))
  (setq evil-replace-state-cursor  `(,bespoke-critical hbar))
  (setq evil-motion-state-cursor   `(,bespoke-green box))
  (setq evil-operator-state-cursor `(,bespoke-brown hollow)))

(when bespoke-set-evil-cursors
  (add-hook 'bespoke-after-load-theme-hook #'bespoke-evil-load-cursors))

;;; Provide theme

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))


(provide-theme 'bespoke)
(provide 'bespoke-theme)


;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; End:
;;; bespoke-theme.el ends here
