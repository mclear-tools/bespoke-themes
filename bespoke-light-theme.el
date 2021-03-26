;; Bespoke color theme

(require 'autothemer)

(autothemer-deftheme
 bespoke-light "A custom light theme for yakshaving"

 ((((class color) (min-colors 89)))

;;; Define light color palette
  (bespoke-light        "#FFFEF9")
  (bespoke-dark         "#2E3440")

  (bespoke-critical     "red3")
  (bespoke-popout       "#ff6347")
  (bespoke-salient      "#873AB7")
  (bespoke-strong       "#000000" (:weight 'bold))
  (bespoke-subtle       "#ECEFF1")
  (bespoke-faded        "#949FB2")
  (bespoke-accent1      "#BF616A")
  (bespoke-accent2      "#5E81AC")
  (bespoke-accent3      "#a2d5a5")
  (bespoke-accent4      "#FFAB91")
  (bespoke-accent5      "#D8DEE9")
  (bespoke-header       "#E3E7EF"))

;;; Customize faces

;;;; Light Faces (Modeline/Window Divs)
 ((default (:foreground "#37474F" :background "#FFFEF9"))

  ;; Mode line
  (header-line (:background bespoke-header
                :foreground bespoke-dark
                :box (:line-width 6 :color bespoke-header :height 150)
                :overline nil
                :underline nil
                ;; :inherit 'variable-pitch
                :height 150))

  (mode-line   (:height 10
                :underline bespoke-header
                :overline nil
                :box nil))

  ;; divide windows more attractively
  (window-divider (:foreground bespoke-light))
  (window-divider-first-pixel (:foreground bespoke-light))
  (window-divider-last-pixel (:foreground bespoke-light))

;;;; Semantic
  (shadow                                        (:foreground bespoke-faded))
  (success                                       (:foreground bespoke-salient))
  (warning                                       (:foreground bespoke-popout))
  (error                                         (:foreground bespoke-critical))
  (match                                         (:foreground bespoke-popout))

;;;; Basic Faces
  (buffer-menu-buffer                            (:foreground bespoke-strong))
  (minibuffer-prompt                             (:foreground bespoke-strong))
  (link                                          (:foreground bespoke-salient))
  (region                                        (:background bespoke-accent5))
  (fringe                                        (:foreground bespoke-faded :weight 'light))
  (isearch                                       (:foreground bespoke-strong))
  (isearch-fail                                  (:foreground bespoke-faded))
  (highlight                                     (:background bespoke-subtle))
  (lazy-highlight                                (:foreground bespoke-accent2))
  (trailing-whitespace                           (:foreground bespoke-subtle))
  (show-paren-match                              (:foreground bespoke-dark
                                                  :background bespoke-accent3))
  (show-paren-mismatch                           (:foreground bespoke-critical
                                                  :background bespoke-popout))
  (tooltip nil                                   :height 0.85)

;;;; Font Lock
  (font-lock-comment-face                        (:foreground bespoke-faded))
  (font-lock-doc-face                            (:foreground bespoke-faded))
  (font-lock-string-face                         (:foreground bespoke-popout))
  (font-lock-constant-face                       (:foreground bespoke-salient))
  (font-lock-warning-face                        (:foreground bespoke-popout))
  (font-lock-function-name-face                  (:foreground bespoke-strong))
  (font-lock-variable-name-face                  (:foreground bespoke-strong))
  (font-lock-builtin-face                        (:foreground bespoke-salient))
  (font-lock-type-face                           (:foreground bespoke-salient))
  (font-lock-keyword-face                        (:foreground bespoke-salient))

;;;; Documentation
  (info-menu-header                              (:foreground bespoke-strong))
  (info-header-node                              (:foreground bespoke-accent2))
  (Info-quoted                                   (:foreground bespoke-faded))
  (info-title-1                                  (:foreground bespoke-strong))
  (info-title-2                                  (:foreground bespoke-strong))
  (info-title-3                                  (:foreground bespoke-strong))
  (info-title-4                                  (:foreground bespoke-strong))

;;;; Bookmarks
  (bookmark-menu-heading                         (:foreground bespoke-strong))
  (bookmark-menu-bookmark                        (:foreground bespoke-salient))

;;;; Message
  (message-cited-text                            (:foreground bespoke-faded))
  (message-header-cc                             (:inherit 'default))
  (message-header-name                           (:foreground bespoke-strong))
  (message-header-newsgroups                     (:inherit 'default))
  (message-header-other                          (:inherit 'default))
  (message-header-subject                        (:foreground bespoke-salient))
  (message-header-to                             (:foreground bespoke-salient))
  (message-header-xheader                        (:inherit 'default))
  (message-mml                                   (:foreground bespoke-popout))
  (message-separator                             (:foreground bespoke-faded))


;;;; Outline
  (outline-minor-0      (:background bespoke-subtle))
  (outline-1            (:inherit 'variable-pitch :foreground bespoke-accent1 ))
  (outline-2            (:inherit 'variable-pitch :foreground bespoke-accent2))
  (outline-3            (:inherit 'variable-pitch :foreground bespoke-accent1 ))
  (outline-4            (:inherit 'variable-pitch :foreground bespoke-accent2))
  (outline-5            (:inherit 'variable-pitch :foreground bespoke-accent1 ))
  (outline-6            (:inherit 'variable-pitch :foreground bespoke-accent2))
  (outline-7            (:inherit 'variable-pitch :foreground bespoke-accent1))
  (outline-8            (:inherit 'variable-pitch :foreground bespoke-accent2))

;;;; Interface
  (widget-field                                 (:foreground bespoke-subtle))
  (widget-button                                (:foreground bespoke-strong))
  (widget-single-line-field                     (:foreground bespoke-subtle))
  (custom-group-subtitle                        (:foreground bespoke-strong))
  (custom-group-tag                             (:foreground bespoke-strong))
  (custom-group-tag-1                           (:foreground bespoke-strong))
  (custom-comment                               (:foreground bespoke-faded))
  (custom-comment-tag                           (:foreground bespoke-faded))
  (custom-changed                               (:foreground bespoke-salient))
  (custom-modified                              (:foreground bespoke-salient))
  (custom-face-tag                              (:foreground bespoke-strong))
  (custom-variable-tag                          (:inherit    'default))
  (custom-invalid                               (:foreground bespoke-popout))
  (custom-visibility                            (:foreground bespoke-salient))
  (custom-state                                 (:foreground bespoke-salient))
  (custom-link                                  (:foreground bespoke-salient))


;;;; Flyspell
  (flyspell-duplicate                           (:foreground bespoke-popout))
  (flyspell-incorrect                           (:foreground bespoke-popout))

;;;; Ido
  (ido-first-match                              (:foreground bespoke-salient))
  (ido-only-match                               (:foreground bespoke-faded))
  (ido-subdir                                   (:foreground bespoke-strong))

;;;; Diff
  (diff-header                                  (:foreground bespoke-faded))
  (diff-file-header                             (:foreground bespoke-strong))
  (diff-context                                 (:inherit    'default))
  (diff-removed                                 (:foreground bespoke-faded))
  (diff-changed                                 (:foreground bespoke-popout))
  (diff-added                                   (:foreground bespoke-salient))
  (diff-refine-added                            (:foreground bespoke-strong))
  (diff-refine-changed                          (:foreground bespoke-popout))
  (diff-refine-removed                          (:foreground bespoke-faded :strike-through t))
  (magit-section-highlight                      (:background bespoke-subtle))

;;;; Term
  (term-bold                                    (:foreground bespoke-strong))
  (term-color-black                             (:inherit    'default))
  (term-color-white                             (:foreground "white" :background "white"))
  (term-color-blue                              (:foreground "#42A5F5" :background "#BBDEFB"))
  (term-color-cyan                              (:foreground "#26C6DA" :background "#B2EBF2"))
  (term-color-green                             (:foreground "#66BB6A" :background "#C8E6C9"))
  (term-color-magenta                           (:foreground "#AB47BC" :background "#E1BEE7"))
  (term-color-red                               (:foreground "#EF5350" :background "#FFCDD2"))
  (term-color-yellow                            (:foreground "#E5D64F" :background "#FFEE58"))

;;;; Org-agenda
  (org-agenda-calendar-event                    (:inherit 'default))
  (org-agenda-calendar-sexp                     (:foreground bespoke-faded))
  (org-agenda-clocking                          (:foreground bespoke-faded))
  (org-agenda-column-dateline                   (:foreground bespoke-faded))
  (org-agenda-current-time                      (:foreground bespoke-faded))
  (org-agenda-date                              (:foreground bespoke-salient))
  (org-agenda-date-today                        (:inherit 'variable-pitch
                                                 :height 1.25 :foreground bespoke-accent1))
  (org-super-agenda-header                      (:inherit 'variable-pitch :foreground bespoke-accent1))
  (org-agenda-date-weekend                      (:foregruond bespoke-faded))
  (org-agenda-diary                             (:foregruond bespoke-faded))
  (org-agenda-dimmed-todo-face                  (:foregruond bespoke-faded))
  (org-agenda-done                              (:foregruond bespoke-faded :strike-through t))
  (org-agenda-filter-category                   (:foregruond bespoke-faded))
  (org-agenda-filter-effort                     (:foregruond bespoke-faded))
  (org-agenda-filter-regexp                     (:foregruond bespoke-faded))
  (org-agenda-filter-tags                       (:foregruond bespoke-faded))
  (org-agenda-restriction-lock                  (:foreground bespoke-faded))
  (org-agenda-structure                         (:foreground bespoke-faded))

;;;; Org mode
  (org-archived                               (:foreground bespoke-faded))
  (org-block                                  (:foreground bespoke-faded))
  (org-block-begin-line                       (:foreground bespoke-faded))
  (org-block-end-line                         (:foreground bespoke-faded))
  (org-checkbox                               (:foreground bespoke-faded))
  (org-checkbox-statistics-done               (:foreground bespoke-faded))
  (org-checkbox-statistics-todo               (:foreground bespoke-faded))
  (org-clock-overlay                          (:foreground bespoke-faded))
  (org-code                                   (:foreground bespoke-faded))
  (org-column                                 (:foreground bespoke-faded))
  (org-column-title                           (:foreground bespoke-faded))
  (org-date                                   (:foreground bespoke-faded))
  (org-date-selected                          (:foreground bespoke-faded))
  (org-default                                (:foreground bespoke-faded))
  (org-document-info                          (:foreground bespoke-faded :weight 'light))
  (org-document-info-keyword                  (:foreground bespoke-faded :weight 'light))
  (org-document-title                         (:inherit 'variable-pitch
                                               :height 1.25
                                               :foreground bespoke-salient))
  (org-done                                   (:foreground bespoke-faded :strike-through t))
  (org-drawer                                 (:foreground bespoke-faded :weight 'light))
  (org-ellipsis                               (:foreground bespoke-faded))
  (org-footnote                               (:foreground bespoke-faded))
  (org-formula                                (:foreground bespoke-faded))
  (org-habit-alert-face                       (:inherit 'default))
  (org-headline-done                          (:foreground bespoke-faded))
  ;;  (set-face 'org-hide                                     bespoke-faded)
  ;;  (set-face 'org-indent                                   bespoke-faded)
  (org-latex-and-related                      (:foreground bespoke-faded))
  (org-level-1                                (:inherit 'variable-pitch
                                               :foreground "#BF616A"))
  (org-level-2                                (:inherit 'variable-pitch
                                               :foreground "#5E81AC" ))
  (org-level-3                                (:inherit 'variable-pitch
                                               :foreground "#BF616A" ))
  (org-level-4                                (:inherit 'variable-pitch
                                               :foreground "#5E81AC"))
  (org-level-5                                (:inherit 'variable-pitch
                                               :foreground "#BF616A"))
  (org-level-6                                (:inherit 'variable-pitch
                                               :foreground "#5E81AC"))
  (org-level-7                                (:inherit 'variable-pitch
                                               :foreground "#BF616A"))
  (org-level-8                                (:inherit 'variable-pitch
                                               :foreground "#5E81AC"))
  (org-link                                   (:foreground bespoke-salient))
  (org-list-dt                                (:foreground bespoke-accent1))
  (org-macro                                  (:foreground bespoke-faded))
  (org-meta-line                              (:foreground bespoke-faded :weight 'light))
  (org-mode-line-clock                        (:foreground bespoke-faded))
  (org-mode-line-clock-overrun                (:foreground bespoke-faded))
  (org-priority                               (:foreground bespoke-faded))
  (org-property-value                         (:foreground bespoke-faded :weight 'light))
  (org-quote                                  (:foreground bespoke-salient))
  (org-scheduled                              (:foreground bespoke-salient))
  (org-scheduled-previously                   (:foreground bespoke-salient))
  (org-scheduled-today                        (:foreground bespoke-salient))
  (org-sexp-date                              (:foreground bespoke-faded))
  (org-special-keyword                        (:foreground bespoke-faded :weight 'light))
  (org-table                                  (:inherit    'default))
  (org-tag                                    (:foreground bespoke-faded))
  (org-tag-group                              (:foreground bespoke-faded))
  (org-target                                 (:foreground bespoke-faded))
  (org-time-grid                              (:foreground bespoke-faded))
  (org-todo                                   (:foreground bespoke-critical))
  (org-upcoming-deadline                      (:foreground bespoke-strong))
  (org-upcoming-distant-deadline              (:foreground bespoke-dark))
  (org-verbatim                               (:foreground bespoke-faded))
  (org-verse                                  (:foreground bespoke-faded))
  (org-warning                                (:foreground bespoke-popout))

;;;; Markdown Mode
  (markdown-blockquote-face                   (:foreground bespoke-salient))
  (markdown-bold-face                         (:foreground bespoke-strong :weight 'bold))
  (markdown-code-face                         (:inherit    'default))
  (markdown-comment-face                      (:foreground bespoke-faded))
  (markdown-footnote-marker-face              (:inherit    'default))
  (markdown-footnote-text-face                (:inherit    'default))
  (markdown-gfm-checkbox-face                 (:inherit    'default))
  (markdown-header-delimiter-face             (:foreground bespoke-faded))
  (markdown-header-face                       (:inherit 'variable-pitch))
  (markdown-header-face-1                     (:inherit 'variable-pitch
                                               :foreground bespoke-popout))
  (markdown-header-face-2                     (:inherit 'variable-pitch
                                               :foreground bespoke-salient))
  (markdown-header-face-3                     (:inherit 'variable-pitch
                                               :foreground bespoke-faded))
  (markdown-header-face-4                     (:inherit 'variable-pitch
                                               :foreground bespoke-salient))
  (markdown-header-face-5                     (:inherit 'variable-pitch
                                               :foreground bespoke-faded))
  (markdown-header-face-6                     (:inherit 'variable-pitch
                                               :foreground bespoke-salient))
  (markdown-header-rule-face                  (:inherit 'default))
  (markdown-highlight-face                    (:inherit 'default))
  (markdown-hr-face                           (:inherit 'default))
  (markdown-html-attr-name-face               (:inherit 'default))
  (markdown-html-attr-value-face              (:inherit 'default))
  (markdown-html-entity-face                  (:inherit 'default))
  (markdown-html-tag-delimiter-face           (:inherit 'default))
  (markdown-html-tag-name-face                (:inherit 'default))
  (markdown-inline-code-face                  (:foreground bespoke-popout))
  (markdown-italic-face                       (:foreground bespoke-faded :slant 'italic))
  (markdown-language-info-face                (:inherit   'default))
  (markdown-language-keyword-face             (:inherit   'default))
  (markdown-line-break-face                   (:inherit   'default))
  (markdown-link-face                         (:foreground bespoke-salient))
  (markdown-link-title-face                   (:inherit   'default))
  (markdown-list-face                         (:foreground bespoke-faded))
  (markdown-markup-face                       (:foreground bespoke-faded))
  (markdown-math-face                         (:inherit    'default))
  (markdown-metadata-key-face                 (:foreground bespoke-faded))
  (markdown-metadata-value-face               (:foreground bespoke-faded))
  (markdown-missing-link-face                 (:inherit    'default))
  (markdown-plain-url-face                    (:inherit    'default))
  (markdown-pre-face                          (:inherit    'default))
  (markdown-reference-face                    (:foreground bespoke-salient))
  (markdown-strike-through-face               (:foreground bespoke-faded))
  (markdown-table-face                        (:inherit    'default))
  (markdown-url-face                          (:foreground bespoke-salient))


;;;; Company
  (company-scrollbar-fg                       (:foreground bespoke-faded))
  (company-scrollbar-bg                       (:foreground bespoke-faded))
  (company-preview                            (:foreground bespoke-faded :weight 'bold))
  (company-preview-common                     (:foreground bespoke-faded))
  (company-tooltip-selection                  (:foreground bespoke-salient))
  (company-tooltip                            (:background bespoke-subtle))
  (company-tooltip-common                     (:background bespoke-subtle))
  (company-tooltip-common-selection           (:foreground bespoke-faded))
  (company-tooltip-annotation                 (:foreground bespoke-faded))
  (company-tooltip-annotation-selection       (:foreground bespoke-faded))

;;;; Selectrum
  (selectrum-current-candidate                (:slant 'italic
                                               :weight 'bold
                                               :background bespoke-subtle))
  (selectrum-prescient-secondary-highlight
   (:weight 'bold :foreground bespoke-accent1))
  (selectrum-prescient-primary-highlight
   (:weight 'bold :foreground bespoke-salient))
  (selectrum-completion-docsig
   (:slant 'italic :inherit 'selectrum-completion-annotation))
  (selectrum-completion-annotation
   (:inherit 'completions-annotations))
  ;; (selectrum-secondary-highlight
  ;;  (:weight 'bold :foreground bespoke-faded))
  ;; (selectrum-primary-highlight
  ;;  (:weight 'bold :foreground bespoke-accent1))
  (selectrum-group-separator
   (:strike-through t :inherit 'shadow))
  (selectrum-group-title
   (:slant 'italic :inherit 'shadow))
  (selectrum-quick-keys-match
   (:inherit 'isearch))
  (selectrum-quick-keys-highlight
   (:foreground bespoke-popout))

;;;; Orderless

  (orderless-match-face-0 (:weight 'bold :foreground bespoke-accent1))
  (orderless-match-face-1 (:weight 'bold :foreground bespoke-accent2))
  (orderless-match-face-2 (:weight 'bold :foreground bespoke-accent4))
  (orderless-match-face-3 (:weight 'bold :foreground bespoke-accent3))

;;;; Mu4e
  (mu4e-attach-number-face                      (:foreground bespoke-strong))
  (mu4e-cited-1-face                            (:foreground bespoke-faded))
  (mu4e-cited-2-face                            (:foreground bespoke-faded))
  (mu4e-cited-3-face                            (:foreground bespoke-faded))
  (mu4e-cited-4-face                            (:foreground bespoke-faded))
  (mu4e-cited-5-face                            (:foreground bespoke-faded))
  (mu4e-cited-6-face                            (:foreground bespoke-faded))
  (mu4e-cited-7-face                            (:foreground bespoke-faded))
  (mu4e-compose-header-face                     (:foreground bespoke-faded))
  (mu4e-compose-separator-face                  (:foreground bespoke-faded))
  (mu4e-contact-face                            (:foreground bespoke-salient))
  (mu4e-context-face                            (:foreground bespoke-faded))
  (mu4e-draft-face                              (:foreground bespoke-faded))
  (mu4e-flagged-face                            (:foreground bespoke-faded))
  (mu4e-footer-face                             (:foreground bespoke-faded))
  (mu4e-forwarded-face                          (:inherit 'default))
  (mu4e-header-face                             (:inherit 'default))
  (mu4e-header-highlight-face                   (:foreground bespoke-subtle))
  (mu4e-header-key-face                         (:foreground bespoke-strong))
  (mu4e-header-marks-face                        (:foreground bespoke-faded))
  (mu4e-header-title-face                       (:foreground bespoke-strong))
  (mu4e-header-value-face                        (:inherit 'default))
  (mu4e-highlight-face                          (:foreground bespoke-popout))
  (mu4e-link-face                              (:foreground bespoke-salient))
  (mu4e-modeline-face                            (:foreground bespoke-faded))
  (mu4e-moved-face                               (:foreground bespoke-faded))
  (mu4e-ok-face                                  (:foreground bespoke-faded))
  (mu4e-region-code                              (:foreground bespoke-faded))
  (mu4e-replied-face                           (:foreground bespoke-salient))
  (mu4e-special-header-value-face                (:inherit 'default))
  (mu4e-system-face                              (:foreground bespoke-faded))
  (mu4e-title-face                              (:foreground bespoke-strong))
  (mu4e-trashed-face                             (:foreground bespoke-faded))
  (mu4e-unread-face                             (:foreground bespoke-strong))
  (mu4e-url-number-face                          (:foreground bespoke-faded))
  (mu4e-view-body-face                           (:inherit 'default))
  (mu4e-warning-face                             (:foreground bespoke-faded))

;;;; Help(ful)

  (helpful-heading                               (:inherit 'variable-pitch :foreground bespoke-accent1 :height 1.25))



;;;; Dired (Plus)
  (diredp-write-priv
   (:foreground bespoke-critical))
  (diredp-tagged-autofile-name
   (:foreground bespoke-dark))
  (diredp-symlink
   (:foreground bespoke-popout))
  (diredp-read-priv
   (:foreground bespoke-popout))
  (diredp-rare-priv
   (:foreground bespoke-popout :background bespoke-critical))
  (diredp-other-priv
   (:background bespoke-accent3))
  (diredp-omit-file-name
   (:strike-through bespoke-faded :inherit 'diredp-ignored-file-name))
  (diredp-number
   (:foreground bespoke-salient))
  (diredp-no-priv
   (:foreground bespoke-critical))
  (diredp-mode-line-flagged
   (:foreground bespoke-critical))
  (diredp-mode-line-marked
   (:foreground bespoke-salient))
  (diredp-link-priv
   (:foreground bespoke-popout))
  (diredp-ignored-file-name
   (:foreground bespoke-faded))
  (diredp-flag-mark-line
   (:foreground bespoke-popout))
  (diredp-flag-mark
   (:foreground bespoke-popout :background bespoke-salient))
  (diredp-file-suffix
   (:foreground bespoke-faded))
  (diredp-file-name
   (:foreground bespoke-dark))
  (diredp-executable-tag
   (:foreground bespoke-critical))
  (diredp-exec-priv
   (:foreground bespoke-critical))
  (diredp-dir-priv
   (:foreground bespoke-faded))
  (diredp-dir-name
   (:foreground bespoke-faded))
  (diredp-dir-heading
   (:inherit 'variable-pitch :foreground bespoke-accent1 :background bespoke-subtle))
  (diredp-deletion-file-name
   (:foreground bespoke-critical))
  (diredp-deletion
   (:foreground bespoke-popout :background bespoke-critical))
  (diredp-date-time
   (:foreground bespoke-faded))
  (diredp-compressed-file-suffix
   (:foreground bespoke-faded))
  (diredp-compressed-file-name
   (:foreground bespoke-dark))
  (diredp-autofile-name
   (:background bespoke-subtle))

;;; End Custom Faces
  ))

;;; Other Custom Variables

;;; End theme
(provide-theme 'bespoke-light)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; End:
