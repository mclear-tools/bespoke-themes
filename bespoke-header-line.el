;;; bespoke-header-line.el --- A minimal custom header line
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
;; A custom header line (or mode line if variable set to nil)
;; for the discerning yakshaver
;; -------------------------------------------------------------------
;; Code:


;;; Mode/Header line functions
;; Mode line in header
;; Organize mode line
(defun vc-branch ()
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "" (substring-no-properties vc-mode
                                             (+ (if (eq backend 'Hg) 2 3) 2)) " "))  nil))

;;;; Shorten Dir
;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
;; ---------------------------------------------------------------------
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))



;;; Simplified default mode line
;;;; Mode line left/right
;; Organize mode-line left and right
(defun mode-line-render (left right)
  "Organize mode line entries to left and right"
  (let* ((available-width (- (window-width) (length left) )))
    (format (format "%%s %%%ds" available-width) left right)))

;;;; Default Mode line
(setq-default bespoke--mode-line
              '((:eval
                 (mode-line-render
                  (format-mode-line (list
                                     ;; Buffer status
                                     (cond ((and buffer-file-name (buffer-modified-p))
                                            (propertize " ⦿ " 'face `(:inherit bespoke-header-mod-face :height 1.10)))
                                           ;; other unicode symbols: ✱ Ⓡ ⓦ ⊕ 🞊 ⨁ ⨂ ⨀ ◯ ⦿ ⊗ 🞅
                                           (buffer-read-only
                                            (propertize " ⊗ " 'face `(:inherit bespoke-header-ro-face :height 1.10)))
                                           (t
                                            (propertize " 🞅 " 'face `(:inherit bespoke-header-default-face :height 1.10))))

                                     ;; Divider (deprecated)
                                     ;; (propertize " | " 'face `(:inherit fringe))

                                     ;; Filename (NOTE: not using %b since that leads to redundant info when using uniquify
                                     (if buffer-file-name
                                         (concat " " (file-name-nondirectory (buffer-file-name)))
                                       " %b")

                                     ;; Parent directory
                                     (when buffer-file-name
                                       (propertize (concat " " (file-name-nondirectory (directory-file-name default-directory)) "/") 'face `(:inherit fringe)))

                                     ;; Evil tags
                                     ;; (propertize evil-mode-line-tag 'face `(:inherit bespoke-faded))

                                     ;; Narrowed buffer
                                     (if (buffer-narrowed-p)
                                         (propertize " ⇥"  'face `(:inherit fringe)))

                                     ;; Modes
                                     (propertize " %m " 'face `(:inherit fringe)
                                                 'help-echo "Mode(s) menu"
                                                 'mouse-face 'mode-line-highlight
                                                 'local-map   mode-line-major-mode-keymap)))
                  (format-mode-line (list
                                     ;; Show project name
                                     (when buffer-file-name
                                       (when (bound-and-true-p projectile-mode)
                                         (let ((project-name (projectile-project-name)))
                                           (unless (string= "-" project-name)
                                             (propertize (format "%s " project-name) 'face `(:slant italic :inherit fringe))))))

                                     ;; When buffer-file is tracked in vc add spacer between project & branch
                                     (when vc-mode
                                       (when (vc-registered (buffer-file-name))
                                         (propertize "• " 'face `(:inherit fringe))))
                                     ;; "⦁ "
                                     ;; Show branch name
                                     ;; NOTE: I can't seem to get line/col to display properly without putting them into the conditional
                                     (if vc-mode
                                         (list
                                          (propertize (vc-branch) 'face `(:inherit fringe))
                                          "%l:%c  ")
                                       "%l:%c  ")))))))

;;; Load Mode or Header line
(if set-bespoke-header-line
    (bespoke-modeline)
  ;; (bespoke--set-header-line-content)
  (progn
    (setq-default header-line-format nil)
    (setq-default mode-line-format bespoke--mode-line)))

;;; Bespoke Mode line
;; (setq bespoke--default-mode-line
;;       '((:eval
;;          (mode-line-render
;;           (format-mode-line (list
;;                              ;; (concat evil-mode-line-tag "|")
;;                              ;; (shorten-directory default-directory 32)
;;                              "  %b "
;;                              (if (buffer-narrowed-p)
;;                                  ("⇥"))
;;                              " %m "
;;                              (cond ((and buffer-file-name (buffer-modified-p))
;;                                     (propertize "(**)" 'face `(:foreground "#f08290")))
;;                                    (buffer-read-only "(RO)" ))
;;                              ))
;;           (format-mode-line (list
;;                              (vc-branch)
;;                              " %l:%c:%o"
;;                              "  ")
;;                             )))))

(setq-default bespoke--mode-line
              '((:eval
                 (mode-line-render
                  (format-mode-line (list
                                     ;; Buffer status
                                     (cond ((and buffer-file-name (buffer-modified-p))
                                            (propertize " ⦿ " 'face `(:inherit bespoke-header-mod-face :height 1.10)))
                                           ;; other unicode symbols: ✱ Ⓡ ⓦ ⊕ 🞊 ⨁ ⨂ ⨀ ◯ ⦿ ⊗ 🞅
                                           (buffer-read-only
                                            (propertize " ⊗ " 'face `(:inherit bespoke-header-ro-face :height 1.10)))
                                           (t
                                            (propertize " 🞅 " 'face `(:inherit bespoke-header-default-face :height 1.10))))

                                     ;; Divider (deprecated)
                                     ;; (propertize " | " 'face `(:inherit fringe))

                                     ;; Filename (NOTE: not using %b since that leads to redundant info when using uniquify
                                     (if buffer-file-name
                                         (concat " " (file-name-nondirectory (buffer-file-name)))
                                       " %b")

                                     ;; Parent directory
                                     (when buffer-file-name
                                       (propertize (concat " " (file-name-nondirectory (directory-file-name default-directory)) "/") 'face `(:inherit fringe)))

                                     ;; Evil tags
                                     ;; (propertize evil-mode-line-tag 'face `(:inherit bespoke-faded))

                                     ;; Narrowed buffer
                                     (if (buffer-narrowed-p)
                                         (propertize " ⇥"  'face `(:inherit fringe)))

                                     ;; Modes
                                     (propertize " %m " 'face `(:inherit fringe)
                                                 'help-echo "Mode(s) menu"
                                                 'mouse-face 'mode-line-highlight
                                                 'local-map   mode-line-major-mode-keymap)))
                  (format-mode-line (list
                                     ;; Show project name
                                     (when buffer-file-name
                                       (when (bound-and-true-p projectile-mode)
                                         (let ((project-name (projectile-project-name)))
                                           (unless (string= "-" project-name)
                                             (propertize (format "%s " project-name) 'face `(:slant italic :inherit fringe))))))

                                     ;; When buffer-file is tracked in vc add spacer between project & branch
                                     (when vc-mode
                                       (when (vc-registered (buffer-file-name))
                                         (propertize "• " 'face `(:inherit fringe))))
                                     ;; "⦁ "
                                     ;; Show branch name
                                     ;; NOTE: I can't seem to get line/col to display properly without putting them into the conditional
                                     (if vc-mode
                                         (list
                                          (propertize (vc-branch) 'face `(:inherit fringe))
                                          "%l:%c  ")
                                       "%l:%c  ")))))))

;;; Header line content
(defun bespoke--set-header-line-content ()
  (interactive)
  (define-key mode-line-major-mode-keymap [header-line]
    (lookup-key mode-line-major-mode-keymap [mode-line]))
  (setq-default header-line-format bespoke--mode-line)
  (setq-default mode-line-format '("")))

;;; Function to (re)load header line
;;;###autoload
(defun bespoke-header-line ()
  "Replace header line with mode line"
  (interactive)
  (progn
    (window-divider-mode 1)
    (setq-default header-line-format bespoke--mode-line)
    (setq-default mode-line-format'(""))
    (setq x-underline-at-descent-line t)))




;;; End Bespoke header line
(provide 'bespoke-header-line)
