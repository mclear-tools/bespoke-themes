;;; bespoke-modeline.el -- custom mode line for bespoke theme  ;; -*- lexical-binding: t -*-
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
;; A custom mode line for bespoke-theme
;; This mode line originated as a fork of nano-emacs modeline.
;; See https://github.com/rougier/nano-emacs
;; https://github.com/rougier/nano-modeline
;; -------------------------------------------------------------------
;;
;; Bespoke mode line format:
;;
;; [ status | name (primary)               secondary | item1 | item2 ]
;;
;; -------------------------------------------------------------------

;;; Requirements

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))


;;; Visual bell for mode line

;; See https://github.com/hlissner/emacs-doom-themes for the idea

(require 'face-remap)

(defface bespoke-visual-bell '((t (:underline "red3")))
  "Face to use for the mode-line when `bespoke-themes-visual-bell-config' is used."
  :group 'bespoke-themes)

;;;###autoload
(defun bespoke-themes-visual-bell-fn ()
  "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it."
  (let ((bespoke-themes--bell-cookie (face-remap-add-relative 'mode-line 'bespoke-visual-bell)))
    (force-mode-line-update)
    (run-with-timer 0.15 nil
                    (lambda (cookie buf)
                      (with-current-buffer buf
                        (face-remap-remove-relative cookie)
                        (force-mode-line-update)))
                    bespoke-themes--bell-cookie
                    (current-buffer))))

;;;###autoload
(defun bespoke-themes-visual-bell-config ()
  "Enable flashing the mode-line on error."
  (setq ring-bell-function #'bespoke-themes-visual-bell-fn
        visible-bell t))

(when bespoke-set-visual-bell
  (bespoke-themes-visual-bell-config))

;;; Inactive Modeline
;; See https://emacs.stackexchange.com/a/26345/11934
;; https://github.com/rougier/nano-modeline
;; Set variable to define inactive faces
(defvar bespoke-modeline--selected-window nil
  "Currently selected window.")

;;; Clean mode line
;; https://www.masteringemacs.org/article/hiding-replacing-modeline-strings
;; NOTE: this is only for minor and major modes
(defvar mode-line-cleaner-alist
  `(;; Minor modes
    (abbrev-mode . "")
    (auto-complete-mode . " α")
    (eldoc-mode . "")
    (hi-lock-mode . "")
    (paredit-mode . " π")
    (yas/minor-mode . " υ")
    ;; Major modes
    (dired-mode . "Dir")
    (emacs-lisp-mode . "EL")
    (fundamental-mode . "FL")
    (helpful-mode . "")
    (help-mode . "")
    (lisp-interaction-mode . "λ")
    (markdown-mode . "MD")
    (magit-mode . "MG")
    (nxhtml-mode . "NX")
    (prog-mode . "PR")
    (python-mode . "PY")
    (text-mode . "TX")
    )
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (cl-loop for cleaner in mode-line-cleaner-alist
           do (let* ((mode (car cleaner))
                     (mode-str (cdr cleaner))
                     (old-mode-str (cdr (assq mode minor-mode-alist))))
                (when old-mode-str
                  (setcar old-mode-str mode-str))
                ;; major mode
                (when (eq mode major-mode)
                  (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)


;;; Mode line functions
;;;; Branch display
;; -------------------------------------------------------------------
(defun vc-project-branch ()
  "Show project and branch name"
  (let ((backend (vc-backend buffer-file-name)))
    (when buffer-file-name
      (concat
       (if (bound-and-true-p projectile-mode)
           (let ((project-name (projectile-project-name)))
             ;; Project name
             (unless (string= "-" project-name)
               (concat
                ;; Divider
                (propertize " •" 'face `(:inherit fringe))
                (format " %s" project-name))))
         " ")
       ;; Show branch
       (if vc-mode
           (concat
            "" (substring-no-properties vc-mode
                                         (+ (if (eq backend 'Hg) 2 3) 2)))  nil)))))

;; Git diff in modeline
;; https://cocktailmake.github.io/posts/emacs-modeline-enhancement-for-git-diff/
(when bespoke-set-git-diff-mode-line
  (defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
    "Show the information of git diff on modeline."
    (setq ad-return-value
	      (concat ad-return-value
		          (let ((plus-minus (vc-git--run-command-string
				                     file "diff" "--numstat" "--")))
		            (if (and plus-minus
		                     (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus))
		                (concat
                         " "
			             (format "+%s" (match-string 1 plus-minus))
			             (format "-%s" (match-string 2 plus-minus)))
		              (propertize "" 'face '(:weight bold))))))))


;;;; Dir display
;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
;; ---------------------------------------------------------------------
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name DIR."
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

;;;; Compose mode line
;; -------------------------------------------------------------------

(defun bespoke-modeline-compose (status name primary secondary)
  "Compose a string with provided information"
  (let* ((char-width    (window-font-width nil 'header-line))
         (window        (get-buffer-window (current-buffer)))
         (active        (eq window bespoke-modeline--selected-window))
         (space-up       +0.20)
         (space-down     -0.20)
         ;; Use status letters for TTY display
	     (prefix          (if (display-graphic-p)
                              (cond ((string= status "⨂")
			                         (propertize (if (window-dedicated-p)" –– " " ⨂ ") 'face (if active 'bespoke-modeline-ro-face 'bespoke-modeline-inactive-face)))
                                    ((string= status  "⨀")
			                         (propertize (if (window-dedicated-p)" –– " " ⨀ ") 'face (if active 'bespoke-modeline-mod-face 'bespoke-modeline-inactive-face)))
                                    ((string= status  "◯")
		                             (propertize (if (window-dedicated-p)" –– " " ◯ ") 'face (if active 'bespoke-modeline-default-face 'bespoke-modeline-inactive-face)))
                                    (t (propertize status 'face (if active 'bespoke-modeline-ro-face 'bespoke-modeline-inactive-face))))
                            (cond ((string= status "RO")
			                       (propertize (if (window-dedicated-p)" -- " " RO ") 'face (if active 'bespoke-modeline-ro-face 'bespoke-modeline-inactive-face)))
                                  ((string= status  "**")
			                       (propertize (if (window-dedicated-p)" -- " " ** ") 'face (if active 'bespoke-modeline-mod-face 'bespoke-modeline-inactive-face)))
                                  ((string= status  "RW")
		                           (propertize (if (window-dedicated-p)" -- " " RW ") 'face (if active 'bespoke-modeline-default-face 'bespoke-modeline-inactive-face)))
                                  (t (propertize status 'face (if active 'bespoke-modeline-ro-face 'bespoke-modeline-inactive-face))))))
         (left (concat
                (propertize " "  'face (if active (cond ((eq bespoke-set-mode-line 'header)
                                                         'header-line)
                                                        ((eq bespoke-set-mode-line 'footer)
                                                         'mode-line)
                                                        ((eq bespoke-set-mode-line nil)
                                                         'mode-line))
                                         'fringe)
			                'display `(raise ,space-up))
                (propertize name 'face (if active (cond ((eq bespoke-set-mode-line 'header)
                                                         'header-line)
                                                        ((eq bespoke-set-mode-line 'footer)
                                                         'mode-line)
                                                        ((eq bespoke-set-mode-line nil)
                                                         'mode-line))
                                         'fringe))
                (propertize " "  'face (if active (cond ((eq bespoke-set-mode-line 'header)
                                                         'header-line)
                                                        ((eq bespoke-set-mode-line 'footer)
                                                         'mode-line)
                                                        ((eq bespoke-set-mode-line nil)
                                                         'mode-line))
                                         'fringe)
		                    'display `(raise ,space-down))
                (if (derived-mode-p 'deft-mode)
                    (propertize primary 'face 'match)
	              (propertize primary 'face 'fringe))))
         (right (concat secondary " "))
         (available-width (- (window-total-width)
			                 (length prefix) (length left) (length right)
			                 (/ (window-right-divider-width) char-width)))
         (available-width (max 1 available-width)))
    (concat prefix
	        left
	        (propertize (make-string available-width ?\ )
                        'face (if active (cond ((eq bespoke-set-mode-line 'header)
                                                'header-line)
                                               ((eq bespoke-set-mode-line 'footer)
                                                'mode-line)
                                               ((eq bespoke-set-mode-line nil)
                                                'mode-line))
                                'fringe))
            (propertize right 'face (if active (cond ((eq bespoke-set-mode-line 'header)
                                                      'header-line)
                                                     ((eq bespoke-set-mode-line 'footer)
                                                      'mode-line)
                                                     ((eq bespoke-set-mode-line nil)
                                                      'mode-line))
                                      'fringe)))))

;;;; Mode line status
  ;; ---------------------------------------------------------------------
  (defun bespoke-modeline-status ()
    "Return buffer status: read-only (⨂)/(RO), modified (⨀)/(**), or read-write (◯)/(RW)"
    (let ((read-only   buffer-read-only)
          (modified    (and buffer-file-name (buffer-modified-p))))
      ;; Use status letters for TTY display
      (cond (modified (if (display-graphic-p) "⨀" "**")) (read-only (if (display-graphic-p) "⨂" "RO")) (t (if (display-graphic-p) "◯" "RW")))))

;;;; Default display
(defun bespoke-modeline-default-mode ()
  (let ((buffer-name (format-mode-line (if buffer-file-name (file-name-nondirectory (buffer-file-name)) "%b")))
        (mode-name   (format-mode-line 'mode-name))
        (branch      (vc-project-branch))
        (position    (format-mode-line "%l:%c ")))
    (bespoke-modeline-compose (bespoke-modeline-status)
                              buffer-name
                              (concat "(" mode-name
                                      (when branch
                                        branch)
                                      ")")
                              (concat
                               ;; Narrowed buffer
                               (when (buffer-narrowed-p)
                                 (propertize "⇥ "  'face `(:inherit fringe)))
                               position))))

;;;; Prog & Text Modes
;; ---------------------------------------------------------------------
(defun bespoke-modeline-prog-mode-p ()
  (derived-mode-p 'prog-mode))

(defun bespoke-modeline-elisp-mode-p ()
  (derived-mode-p 'lisp-data-mode))

(defun bespoke-modeline-text-mode-p ()
  (derived-mode-p 'text-mode))


;;;; Info Display
;; ---------------------------------------------------------------------
(setq Info-use-header-line nil)
(defun bespoke-modeline-info-breadcrumbs ()
  (let ((nodes (Info-toc-nodes Info-current-file))
        (cnode Info-current-node)
	    (node Info-current-node)
        (crumbs ())
        (depth Info-breadcrumbs-depth)
	    line)
    (while  (> depth 0)
      (setq node (nth 1 (assoc node nodes)))
      (if node (push node crumbs))
      (setq depth (1- depth)))
    (setq crumbs (cons "Top" (if (member (pop crumbs) '(nil "Top"))
			                     crumbs (cons nil crumbs))))
    (forward-line 1)
    (dolist (node crumbs)
      (let ((text
	         (if (not (equal node "Top")) node
	           (format "%s"
		               (if (stringp Info-current-file)
			               (file-name-sans-extension
			                (file-name-nondirectory Info-current-file))
			             Info-current-file)))))
	    (setq line (concat line (if (null line) "" " > ")
                           (if (null node) "..." text)))))
    (if (and cnode (not (equal cnode "Top")))
        (setq line (concat line (if (null line) "" " > ") cnode)))
    line))

(defun bespoke-modeline-info-mode-p ()
  (derived-mode-p 'Info-mode))

(defun bespoke-modeline-info-mode ()
  (bespoke-modeline-compose (bespoke-modeline-status)
                            "Info"
                            (concat "("
                                    (bespoke-modeline-info-breadcrumbs)
                                    ")")
                            ""))


;;;; Term & Vterm
;; ---------------------------------------------------------------------
(defun bespoke-modeline-term-mode-p ()
  (derived-mode-p 'term-mode))

(defun bespoke-modeline-term-mode ()
  (bespoke-modeline-compose " >_ "
                            "Terminal"
                            (concat "(" shell-file-name ")")
                            (shorten-directory default-directory 32)))

;; ---------------------------------------------------------------------
;; vterm
(defun bespoke-modeline-vterm-mode-p ()
  (derived-mode-p 'vterm-mode))

(defun bespoke-modeline-get-ssh-host (str)
  (let ((split-defdir (split-string default-directory)))
    (if (equal (length split-defdir) 1)
        (car (split-string (shell-command-to-string "hostname") "\n"))
      (cadr split-defdir))))

(defun bespoke-modeline-vterm-mode ()
  (bespoke-modeline-compose " >_ "
                            "vterm"
                            (concat "(" (bespoke-modeline-get-ssh-host default-directory) ")")
                            (shorten-directory (car (last (split-string default-directory ":"))) 32)))


;;;; Message Mode
;; ---------------------------------------------------------------------
(defun bespoke-modeline-message-mode-p ()
  (derived-mode-p 'message-mode))

(defun bespoke-modeline-message-mode ()
  (bespoke-modeline-compose (bespoke-modeline-status)
                            "Message" "(draft)" ""))

;;;; Docview Mode
;;---------------------------------------------------------------------
(defun bespoke-modeline-docview-mode-p ()
  (derived-mode-p 'doc-view-mode))

(defun bespoke-modeline-docview-mode ()
  (let ((buffer-name (format-mode-line "%b"))
	    (mode-name   (format-mode-line 'mode-name))
	    (branch      (vc-project-branch))
	    (page-number (concat
		              (number-to-string (doc-view-current-page)) "/"
		              (or (ignore-errors
			                (number-to-string (doc-view-last-page-number)))
			              "???"))))
    (bespoke-modeline-compose
     (bespoke-modeline-status)
     buffer-name
     (concat "(" mode-name
	         (if branch (concat ", "
				                (propertize branch 'face 'italic)))
	         ")" )
     page-number)))

;;;; PDF View Mode
;; ---------------------------------------------------------------------
(defun bespoke-modeline-pdf-view-mode-p ()
  (derived-mode-p 'pdf-view-mode))

(defun bespoke-modeline-pdf-view-mode ()
  (let ((buffer-name (format-mode-line "%b"))
	    (mode-name   (format-mode-line 'mode-name))
	    (branch      (vc-project-branch))
	    (page-number (concat
		              (number-to-string (pdf-view-current-page)) "/"
		              (or (ignore-errors
			                (number-to-string (pdf-cache-number-of-pages)))
			              "???"))))
    (bespoke-modeline-compose
     (bespoke-modeline-status)
     buffer-name
     (concat "(" mode-name
	         (if branch (concat ", "
				                (propertize branch 'face 'italic)))
	         ")" )
     (concat page-number " "))))

;;;; MenuMode
;; ---------------------------------------------------------------------
(defun buffer-menu-mode-header-line ()
  (face-remap-add-relative
   'header-line `(:background ,(face-background 'bespoke-subtle))))
(add-hook 'Buffer-menu-mode-hook
          #'buffer-menu-mode-header-line)

;;;; Completion
;; ---------------------------------------------------------------------
(defun bespoke-modeline-completion-list-mode-p ()
  (derived-mode-p 'completion-list-mode))

(defun bespoke-modeline-completion-list-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (format-mode-line 'mode-name))
        (position    (format-mode-line "%l:%c")))

    (bespoke-modeline-compose (bespoke-modeline-status)
                              buffer-name "" position)))

;;;; Deft Mode
;; ---------------------------------------------------------------------
(with-eval-after-load 'deft
  (defun deft-print-header ()
    (force-mode-line-update)
    (widget-insert "\n")))

(defun bespoke-modeline-deft-mode-p ()
  (derived-mode-p 'deft-mode))

(defun bespoke-modeline-deft-mode ()
  (let ((prefix " DEFT ")
        (primary "Search:")
        (filter  (if deft-filter-regexp
                     (deft-whole-filter-regexp) "<filter>"))
        (matches (if deft-filter-regexp
                     (format "%d matches" (length deft-current-files))
                   (format "%d notes" (length deft-all-files)))))
    (bespoke-modeline-compose " DEFT "
                              primary filter matches)))

;;;; Calendar Mode
;; ---------------------------------------------------------------------
(defun bespoke-modeline-calendar-mode-p ()
  (derived-mode-p 'calendar-mode))

(defun bespoke-modeline-calendar-mode () "")

;; Calendar (no header, only overline)
(with-eval-after-load 'calendar
  (defun calendar-setup-header ()
    (setq header-line-format "")
    (face-remap-add-relative
     'header-line `(:overline ,(face-foreground 'default)
                    :height 0.5
                    :background ,(face-background 'default))))
  (add-hook 'calendar-initial-window-hook #'calendar-setup-header)

  ;; From https://emacs.stackexchange.com/questions/45650
  (add-to-list 'display-buffer-alist
               `(,(rx string-start "*Calendar*" string-end)
                 (display-buffer-below-selected))))


;;;; Org Capture
;; ---------------------------------------------------------------------
(defun bespoke-modeline-org-capture-mode-p ()
  (bound-and-true-p org-capture-mode))

(defun bespoke-modeline-org-capture-mode ()
  (bespoke-modeline-compose (bespoke-modeline-status)
                            "Capture"
                            "(org)"
                            ""))

(with-eval-after-load 'org-capture
  (defun org-capture-turn-off-header-line ()
    (setq-local header-line-format (default-value 'header-line-format))
    ;; (fit-window-to-buffer nil nil 8)
    ;; (face-remap-add-relative 'header-line '(:background "#ffffff"))
    (message nil))
  (add-hook 'org-capture-mode-hook
            #'org-capture-turn-off-header-line))

;;;; Org Agenda
;; ---------------------------------------------------------------------
(defun bespoke-modeline-org-agenda-mode-p ()
  (derived-mode-p 'org-agenda-mode))

(defun bespoke-modeline-org-agenda-mode ()
  (let* ((space-up       +0.06)
         (space-down     -0.20))
    (bespoke-modeline-compose (bespoke-modeline-status)
                              "Agenda"
                              ""
                              (concat (propertize "◴" 'face 'default 'display `(raise ,space-up)) (format-time-string "%H:%M ")))))

;;;; Org Clock
;; ---------------------------------------------------------------------
(setq org-mode-line-string nil)
(with-eval-after-load 'org-clock
  (add-hook 'org-clock-out-hook
            #'(lambda () (setq org-mode-line-string nil)
                (force-mode-line-update))))

(defun bespoke-modeline-org-clock-mode-p ()
  org-mode-line-string)

(defun bespoke-modeline-org-clock-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (format-mode-line 'mode-name))
        (branch      (vc-project-branch))
        (position    (format-mode-line "%l:%c")))
    (bespoke-modeline-compose (bespoke-modeline-status)
                              buffer-name
                              (concat "(" mode-name
                                      (if branch (concat ", "
                                                         (propertize branch 'face 'italic)))
                                      ")" )
                              org-mode-line-string)))





;;; Set Mode line
;;;; Set content for mode/header line
(defvar bespoke--mode-line
  '((:eval
     (cond ((bespoke-modeline-prog-mode-p)            (bespoke-modeline-default-mode))
           ((bespoke-modeline-deft-mode-p)            (bespoke-modeline-deft-mode))
           ((bespoke-modeline-info-mode-p)            (bespoke-modeline-info-mode))
           ((bespoke-modeline-term-mode-p)            (bespoke-modeline-term-mode))
           ((bespoke-modeline-vterm-mode-p)           (bespoke-modeline-vterm-mode))
           ((bespoke-modeline-text-mode-p)            (bespoke-modeline-default-mode))
           ((bespoke-modeline-pdf-view-mode-p)        (bespoke-modeline-pdf-view-mode))
	       ((bespoke-modeline-docview-mode-p)         (bespoke-modeline-docview-mode))
	       ((bespoke-modeline-completion-list-mode-p) (bespoke-modeline-completion-list-mode))
           ((bespoke-modeline-calendar-mode-p)        (bespoke-modeline-calendar-mode))
           ((bespoke-modeline-org-capture-mode-p)     (bespoke-modeline-org-capture-mode))
           ((bespoke-modeline-org-agenda-mode-p)      (bespoke-modeline-org-agenda-mode))
           ((bespoke-modeline-org-clock-mode-p)       (bespoke-modeline-org-clock-mode))
           ((bespoke-modeline-elisp-mode-p)           (bespoke-modeline-default-mode))
           ((bespoke-modeline-message-mode-p)         (bespoke-modeline-message-mode))
           (t                                      (bespoke-modeline-default-mode))))))

;;;; Mode line header function
;; ---------------------------------------------------------------------
(defun bespoke--header-line ()
  "Install a mode line in header whose content depends on the major mode."
  ;; Update selected window
  (setq bespoke-modeline--selected-window (selected-window))
  (setq-default header-line-format bespoke--mode-line)
  (setq-default mode-line-format (list (propertize "%-" 'face `(:inherit fringe))))
  (force-mode-line-update))

(defun bespoke--footer-line ()
  "Install mode line whose content depends on the major mode."
  ;; Update selected window
  (setq bespoke-modeline--selected-window (selected-window))
  (setq-default header-line-format nil)
  (setq-default mode-line-format bespoke--mode-line)
  (force-mode-line-update))

(defun bespoke/toggle-mode-line ()
  "Toggle betwee a modeline in header and one at footer whose content depends on the
major mode.

Note that you may need to revert buffers to see the modeline properly."
  (interactive)
  (bespoke--disable-all-themes)
  (if (eq bespoke-set-mode-line 'header)
      (progn
        (setq bespoke-set-mode-line 'footer)
        (setq header-line-format nil)
        (setq-default mode-line-format bespoke--mode-line)
        (setq mode-line-format bespoke--mode-line)
        (setq-default header-line-format nil))
    (progn
      (setq bespoke-set-mode-line 'header)
      (setq-default header-line-format bespoke--mode-line)
      (setq-default mode-line-format (list (propertize "%-" 'face `(:inherit fringe))))
      (set-face-attribute 'header-line nil :inherit 'header-line)
      (set-face-attribute 'mode-line nil :inherit 'mode-line)
	  (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line-inactive-face)))
  (load-theme 'bespoke)
  (force-mode-line-update)
  (revert-buffer))

(defun bespoke/disable-custom-mode-line ()
  "Use the Emacs default mode line with bespoke colors

Note that you may need to revert buffers to see the modeline properly"
  (interactive)
  (bespoke--disable-all-themes)
  (progn
    (setq bespoke-set-mode-line nil)
    (setq header-line-format nil)
    (setq-default header-line-format nil)
    (set-face-attribute 'mode-line nil :inherit 'mode-line)
    (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line-inactive)
    (let ((format (set 'mode-line-format (eval (car (get 'mode-line-format 'standard-value))))))
      (setq mode-line-format format)
      (setq-default mode-line-format format)))
  (load-theme 'bespoke)
  (force-mode-line-update 'ALL)
  (revert-buffer))

;;;; Load Mode or Header line
(cond ((eq bespoke-set-mode-line 'header)
       (progn
         (setq eshell-status-in-modeline nil)
         (bespoke--header-line)
         ))
      ((eq bespoke-set-mode-line 'footer)
       (progn
         (setq eshell-status-in-modeline nil)
         (bespoke--footer-line)
         ))
      ((eq bespoke-set-mode-line nil)))

;;;; Inactive window hook

;; This hook is necessary to register selected window so when the window corresponding
;;  to the evaluated modeline is evaluated is always selected.
(when bespoke-set-mode-line t
      (add-hook 'post-command-hook
	            (lambda () (setq bespoke-modeline--selected-window (selected-window)))))

;;; Provide Bespoke Modeline
(provide 'bespoke-modeline)

;;; End bespoke-modeline.el
