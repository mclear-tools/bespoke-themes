;;; bespoke-light-theme.el --- A minimal and medium contrast light theme
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
;;; Commentary:
;; This theme offers a bespoke light theme for the discerning yakshaver
;; -------------------------------------------------------------------
;;; Code:
(require 'bespoke-themes)
(bespoke-theme-set-light)

;;; Theme definition
(deftheme bespoke-light
  "A custom light theme for yakshaving.")

;;; Define class
(let ((class '((class color) (min-colors 89))))

;;; Customize faces
  (custom-theme-set-faces
   'bespoke-light
   `(default ((,class :foreground ,bespoke-dark :background ,bespoke-light)))

;;;; Mode line/Header line

   ;;; Always have header-line colors set, as they are used to define bespoke-modeline colors
   `(header-line ((,class :foreground ,bespoke-dark
                          :background ,bespoke-modeline
                          :box (:line-width 5 :color ,bespoke-modeline :style nil)
                          :overline nil
                          :underline nil)))

   ;;; Conditionally load header and mode line colors
   (when set-bespoke-header-line
     `(mode-line ((,class :height 0.1
                          :underline ,bespoke-modeline
                          :overline nil
                          :box nil))))

   (when set-bespoke-header-line
     `(mode-line-inactive  ((,class :height 0.1
                                    :underline ,bespoke-subtle
                                    :overline nil
                                    :box nil))))

   (when (not set-bespoke-header-line)
     `(mode-line ((,class :foreground ,bespoke-dark
                          :background ,bespoke-modeline
                          :box (:line-width 4 :color ,bespoke-modeline)
                          :overline nil
                          :underline nil))))

 ;;; Mode line indicators
   `(bespoke-header-inactive-face ((,class :background ,bespoke-modeline
                                           :foreground ,bespoke-faded
                                           :box (:line-width 5 :color ,bespoke-modeline :style nil))))

   `(bespoke-header-default-face ((,class :foreground ,bespoke-white
                                          :background ,bespoke-salient
                                          :box (:line-width 1
                                                :color ,bespoke-salient
                                                :style nil))))

   `(bespoke-header-mod-face ((,class :foreground ,bespoke-white
                                      :background ,bespoke-blue
                                      :box (:line-width 1
                                            :color ,bespoke-blue
                                            :style nil))))

   `(bespoke-header-ro-face ((,class :foreground ,bespoke-white
                                     :background ,bespoke-yellow
                                     :box (:line-width 1
                                           :color ,bespoke-yellow
                                           :style nil))))

;;;; Window Divs
   ;; divide windows more attractively
   `(window-divider             ((,class :foreground ,bespoke-light)))
   `(window-divider-first-pixel ((,class :foreground ,bespoke-light)))
   `(window-divider-last-pixel  ((,class :foreground ,bespoke-light)))

;;;; Semantic
   `(italic                                        ((,class (:slant italic))))
   `(bold                                          ((,class (:weight bold))))
   `(bold-italic                                   ((,class (:weight bold :slant italic))))
   `(underline                                     ((,class (:underline t))))
   `(shadow                                        ((,class :foreground ,bespoke-faded)))
   `(success                                       ((,class :foreground ,bespoke-salient)))
   `(warning                                       ((,class :foreground ,bespoke-popout)))
   `(error                                         ((,class :foreground ,bespoke-critical)))
   `(match                                         ((,class :foreground ,bespoke-popout)))

;;;; Basic Faces
   `(buffer-menu-buffer                            ((,class :foreground ,bespoke-strong)))
   `(minibuffer-prompt                             ((,class :foreground ,bespoke-brown)))
   `(link                                          ((,class :foreground ,bespoke-salient)))
   `(region                                        ((,class :background ,bespoke-highlight)))
   `(fringe                                        ((,class :foreground ,bespoke-faded :weight light)))
   `(highlight                                     ((,class :background ,bespoke-highlight)))
   `(lazy-highlight                                ((,class :foreground ,bespoke-brown)))
   `(trailing-whitespace                           ((,class :foreground ,bespoke-faded)))
   `(show-paren-match                              ((,class :foreground ,bespoke-light :background ,bespoke-brown)))
   `(show-paren-mismatch                            ((,class :foreground ,bespoke-popout :background ,bespoke-critical)))
   `(tooltip nil                                    ((,class :height 0.85)))

;;;; Search
   `(evil-ex-search                                ((,class :background ,bespoke-popout)))
   `(isearch                                       ((,class :foreground ,bespoke-popout :weight bold)))
   `(isearch-fail                                  ((,class :background ,bespoke-critical)))
   `(isearch-group-1                               ((,class :background ,bespoke-blue)))
   `(isearch-group-2                               ((,class :background ,bespoke-red)))
   `(query-replace                                 ((,class :background ,bespoke-yellow)))

;;;; Font Lock
   `(font-lock-comment-face                        ((,class :foreground ,bespoke-faded)))
   `(font-lock-doc-face                            ((,class :foreground ,bespoke-faded)))
   `(font-lock-string-face                         ((,class :foreground ,bespoke-strong)))
   `(font-lock-constant-face                       ((,class :foreground ,bespoke-brown)))
   `(font-lock-builtin-face                        ((,class :foreground ,bespoke-brown)))
   `(font-lock-function-name-face                  ((,class :foreground ,bespoke-green)))
   `(font-lock-variable-name-face                  ((,class :foreground ,bespoke-blue)))
   `(font-lock-type-face                           ((,class :foreground ,bespoke-salient)))
   `(font-lock-keyword-face                        ((,class :foreground ,bespoke-popout)))
   `(font-lock-reference-face                      ((,class :foreground ,bespoke-popout)))
   `(font-lock-warning-face                        ((,class :foreground ,bespoke-critical)))
   `(font-lock-regexp-grouping-backslash           ((,class :foreground ,bespoke-critical)))
   `(font-lock-regexp-grouping-construct           ((,class :foreground ,bespoke-critical)))

;;;; Documentation
   `(info-menu-header                              ((,class :foreground ,bespoke-strong)))
   `(info-header-node                              ((,class :foreground ,bespoke-brown)))
   `(Info-quoted                                   ((,class :foreground ,bespoke-faded)))
   `(info-title-1                                  ((,class :foreground ,bespoke-strong)))
   `(info-title-2                                  ((,class :foreground ,bespoke-strong)))
   `(info-title-3                                  ((,class :foreground ,bespoke-strong)))
   `(info-title-4                                  ((,class :foreground ,bespoke-strong)))

;;;; Bookmarks
   `(bookmark-menu-heading                         ((,class :foreground ,bespoke-strong)))
   `(bookmark-menu-bookmark                        ((,class :foreground ,bespoke-salient)))

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


;;;; Outline

   `(outline-minor-0      ((,class :background ,bespoke-subtle)))
   `(outline-1            ((,class :inherit variable-pitch :foreground ,bespoke-brown)))
   `(outline-2            ((,class :inherit variable-pitch :foreground ,bespoke-green)))
   `(outline-3            ((,class :inherit outline-1)))
   `(outline-4            ((,class :inherit outline-2)))
   `(outline-5            ((,class :inherit outline-1)))
   `(outline-6            ((,class :inherit outline-2)))
   `(outline-7            ((,class :inherit outline-1)))
   `(outline-8            ((,class :inherit outline-2)))

;;;; Interface
   `(widget-field                                 ((,class :foreground ,bespoke-subtle)))
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

;;;; Flyspell
   `(flyspell-duplicate                           ((,class :foreground ,bespoke-popout)))
   `(flyspell-incorrect                           ((,class :foreground ,bespoke-critical)))

;;;; Ido
   `(ido-first-match                              ((,class :foreground ,bespoke-salient)))
   `(ido-only-match                               ((,class :foreground ,bespoke-faded)))
   `(ido-subdir                                   ((,class :foreground ,bespoke-strong)))

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

;;;; Term
   `(term-bold                                    ((,class :foreground ,bespoke-strong)))
   `(term-color-black                             ((,class :inherit    default)))
   `(term-color-white                             ((,class :foreground "white" :background "white")))
   `(term-color-blue                              ((,class :foreground "#42A5F5" :background "#BBDEFB")))
   `(term-color-cyan                              ((,class :foreground "#26C6DA" :background "#B2EBF2")))
   `(term-color-green                             ((,class :foreground "#66BB6A" :background "#C8E6C9")))
   `(term-color-magenta                           ((,class :foreground "#AB47BC" :background "#E1BEE7")))
   `(term-color-red                               ((,class :foreground "#EF5350" :background "#FFCDD2")))
   `(term-color-yellow                            ((,class :foreground "#E5D64F" :background "#FFEE58")))

;;;; Org-agenda
   `(org-agenda-calendar-event                    ((,class :inherit default)))
   `(org-agenda-calendar-sexp                     ((,class :foreground ,bespoke-faded)))
   `(org-agenda-clocking                          ((,class :foreground ,bespoke-faded)))
   `(org-agenda-column-dateline                   ((,class :foreground ,bespoke-faded)))
   `(org-agenda-current-time                      ((,class :foreground ,bespoke-faded)))
   `(org-agenda-date                              ((,class :foreground ,bespoke-salient)))
   `(org-agenda-date-today                        ((,class :inherit    variable-pitch :height 1.25 :foreground ,bespoke-brown)))
   `(org-super-agenda-header                      ((,class :inherit    variable-pitch :foreground ,bespoke-brown)))
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
   `(org-document-title                         ((,class :inherit variable-pitch :height 1.25 :foreground ,bespoke-salient)))
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
   `(org-list-dt                                ((,class :foreground ,bespoke-yellow)))
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
   `(org-todo                                   ((,class :foreground ,bespoke-critical)))
   `(org-upcoming-deadline                      ((,class :foreground ,bespoke-strong)))
   `(org-upcoming-distant-deadline              ((,class :foreground ,bespoke-dark)))
   `(org-verbatim                               ((,class :foreground ,bespoke-faded)))
   `(org-verse                                  ((,class :foreground ,bespoke-faded)))
   `(org-warning                                ((,class :foreground ,bespoke-popout)))

;;;; Markdown Mode
   `(markdown-blockquote-face                   ((,class :foreground ,bespoke-salient)))
   `(markdown-bold-face                         ((,class :foreground ,bespoke-strong :weight bold)))
   `(markdown-code-face                         ((,class :inherit    default)))
   `(markdown-comment-face                      ((,class :foreground ,bespoke-faded)))
   `(markdown-footnote-marker-face              ((,class :inherit    default)))
   `(markdown-footnote-text-face                ((,class :inherit    default)))
   `(markdown-gfm-checkbox-face                 ((,class :inherit    default)))
   `(markdown-header-delimiter-face             ((,class :foreground ,bespoke-faded)))
   `(markdown-header-face                       ((,class :inherit variable-pitch)))
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

;;;; Company
   `(company-scrollbar-fg                       ((,class :foreground ,bespoke-faded)))
   `(company-scrollbar-bg                       ((,class :foreground ,bespoke-faded)))
   `(company-preview                            ((,class :foreground ,bespoke-faded :weight bold)))
   `(company-preview-common                     ((,class :foreground ,bespoke-faded)))
   `(company-tooltip-selection                  ((,class :foreground ,bespoke-salient)))
   `(company-tooltip                            ((,class :background ,bespoke-subtle)))
   `(company-tooltip-common                     ((,class :background ,bespoke-subtle)))
   `(company-tooltip-common-selection           ((,class :foreground ,bespoke-faded)))
   `(company-tooltip-annotation                 ((,class :foreground ,bespoke-faded)))
   `(company-tooltip-annotation-selection       ((,class :foreground ,bespoke-faded)))

;;;; Selectrum
   `(selectrum-current-candidate                ((,class :slant  italic :weight bold :background ,bespoke-highlight)))
   `(selectrum-prescient-secondary-highlight    ((,class :weight bold :foreground ,bespoke-brown)))
   `(selectrum-prescient-primary-highlight      ((,class :weight bold :foreground ,bespoke-popout)))
   `(selectrum-completion-docsig                ((,class :slant  italic :inherit selectrum-completion-annotation)))
   `(selectrum-completion-annotation            ((,class :inherit completions-annotations)))
   `(selectrum-group-separator                  ((,class :strike-through t :inherit shadow)))
   `(selectrum-group-title                      ((,class :slant  italic :inherit shadow)))
   `(selectrum-quick-keys-match                 ((,class :inherit isearch)))
   `(selectrum-quick-keys-highlight             ((,class :foreground ,bespoke-red)))

;;;; Vertico
   `(vertico-current                            ((,class :slant italic :weight bold :background ,bespoke-highlight)))

;;;; Orderless

   `(orderless-match-face-0                     ((,class :weight bold :foreground ,bespoke-blue)))
   `(orderless-match-face-1                     ((,class :weight bold :foreground ,bespoke-brown)))
   `(orderless-match-face-2                     ((,class :weight bold :foreground ,bespoke-red)))
   `(orderless-match-face-3                     ((,class :weight bold :foreground ,bespoke-green)))

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

;;;; Help(ful)

   `(helpful-heading                            ((,class :inherit variable-pitch :foreground ,bespoke-yellow :height 1.25)))

;;;; Dired (plus)
   `(diredp-write-priv                           ((,class :foreground ,bespoke-critical)))
   `(diredp-tagged-autofile-name                 ((,class :foreground ,bespoke-dark)))
   `(diredp-symlink                              ((,class :foreground ,bespoke-popout)))
   `(diredp-read-priv                            ((,class :foreground ,bespoke-popout)))
   `(diredp-rare-priv                            ((,class :foreground ,bespoke-popout :background ,bespoke-critical)))
   `(diredp-other-priv                           ((,class :background ,bespoke-green)))
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
   `(diredp-file-name                            ((,class :foreground ,bespoke-dark)))
   `(diredp-executable-tag                       ((,class :foreground ,bespoke-critical)))
   `(diredp-exec-priv                            ((,class :foreground ,bespoke-critical)))
   `(diredp-dir-priv                             ((,class :foreground ,bespoke-faded)))
   `(diredp-dir-name                             ((,class :foreground ,bespoke-brown)))
   `(diredp-dir-heading                          ((,class :inherit    variable-pitch :foreground ,bespoke-yellow :background ,bespoke-subtle)))
   `(diredp-deletion-file-name                   ((,class :foreground ,bespoke-critical)))
   `(diredp-deletion                             ((,class :foreground ,bespoke-popout :background ,bespoke-critical)))
   `(diredp-date-time                            ((,class :foreground ,bespoke-faded)))
   `(diredp-compressed-file-suffix               ((,class :foreground ,bespoke-faded)))
   `(diredp-compressed-file-name                 ((,class :foreground ,bespoke-dark)))
   `(diredp-autofile-name                        ((,class :background ,bespoke-subtle)))

;;;; Imenu List
   `(imenu-list-entry-face-0                     ((,class :inherit imenu-list-entry-face :foreground ,bespoke-faded)))
   `(imenu-list-entry-face-1                     ((,class :inherit imenu-list-entry-face :foreground ,bespoke-faded)))
   `(imenu-list-entry-face-2                     ((,class :inherit imenu-list-entry-face :foreground ,bespoke-faded)))
   `(imenu-list-entry-face-3                     ((,class :inherit imenu-list-entry-face :foreground ,bespoke-faded)))

;;;; Posframe

   `(which-key-posframe                           ((,class (:background ,bespoke-subtle))))
   `(which-key-posframe-border                    ((,class (:background ,bespoke-subtle))))

   ))

;;; Provide theme

(provide-theme 'bespoke-light)

(provide 'bespoke-light-theme)


;;; bespoke-light-theme.el ends here

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; End:
