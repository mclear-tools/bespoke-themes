;; -*- lexical-binding: t -*-
;;; bespoke-modeline.el
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
;; A custom mode line for the discerning yakshaver.
;; This mode line started as a fork of nano-emacs modeline.
;; See https://github.com/rougier/nano-emacs
;; -------------------------------------------------------------------
;;
;; Bespoke mode line format:
;;
;; [ status | name (primary)               secondary | item1 | item2 ]
;;
;; -------------------------------------------------------------------
(require 'subr-x)
;;; Set variable
(defvar set-bespoke-header-line t "Use bespoke header line theme")

;;; Define extra faces
(defface bespoke-header-default-face nil
  "Default face for ther header line."
  :group 'bespoke)

(defface bespoke-header-mod-face nil
  "Header line face for modified buffers."
  :group 'bespoke)

(defface bespoke-header-ro-face nil
  "Header line face for read-only buffers."
  :group 'bespoke)

(defface bespoke-header-inactive-face nil
  "Header line face for read-only buffers."
  :group 'bespoke)

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


;;;; Inactive Header line
;; https://emacs.stackexchange.com/a/3522/11934
(defun bespoke-update-header ()
  (mapc
   (lambda (window)
     (with-current-buffer (window-buffer window)
       ;; don't mess with buffers that don't have a header line
       (when header-line-format
         (let ((original-format (get 'header-line-format 'original))
               (inactive-face 'bespoke-header-inactive-face)) ; change this to your favorite inactive header line face
           ;; if we didn't save original format yet, do it now
           (when (not original-format)
             (put 'header-line-format 'original header-line-format)
             (setq original-format header-line-format))
           ;; check if this window is selected, set faces accordingly
           (if (eq window (selected-window))
               (setq header-line-format original-format)
             (setq header-line-format `(:propertize ,original-format face ,inactive-face)))))))
   (window-list)))
(add-hook 'buffer-list-update-hook #'bespoke-update-header)


;;; Clean mode line
;; https://www.masteringemacs.org/article/hiding-replacing-modeline-strings
;; NOTE: this is only for minor and major modes
(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (yas/minor-mode . " υ")
    (paredit-mode . " π")
    (eldoc-mode . "")
    (abbrev-mode . "")
    ;; Major modes
    (dired-mode . "Dir")
    (lisp-interaction-mode . "λ")
    (hi-lock-mode . "")
    (python-mode . "Py")
    (emacs-lisp-mode . "EL")
    (nxhtml-mode . "nx")
    (markdown-mode . "MD")
    (fundamental-mode . "FL"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (require'cl)
  (loop for cleaner in mode-line-cleaner-alist
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
  ;; Show project name
  (when buffer-file-name
    (concat
     ;; Divider
     (when (vc-registered (buffer-file-name))
       (propertize "• " 'face `(:inherit fringe)))
     (when (bound-and-true-p projectile-mode)
       (let ((project-name (projectile-project-name)))
         (unless (string= "-" project-name)
           (format "%s" project-name))))
     ;; Show branch
     (if vc-mode
         (let ((backend (vc-backend buffer-file-name)))
           (concat "" (substring-no-properties vc-mode
                                                (+ (if (eq backend 'Hg) 2 3) 2))))  nil))))


;;;; Dir display
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

;;;; Compose mode line
;; -------------------------------------------------------------------
(defun bespoke-modeline-compose (status name primary secondary)
  "Compose a string with provided information"
  (let* ((char-width    (window-font-width nil 'header-line))
         (window        (get-buffer-window (current-buffer)))
         (space-up       +0.15)
         (space-down     -0.20)
	     (prefix (cond ((string= status "⨂")
			            (propertize (if (window-dedicated-p)" -- " " ⨂ ") 'face 'bespoke-header-ro-face))
                       ((string= status "⨀")
			            (propertize (if (window-dedicated-p)" -- " " ⨀ ") 'face 'bespoke-header-mod-face))
                       ((string= status "◯")
		                (propertize (if (window-dedicated-p)" -- " " ◯ ") 'face 'bespoke-header-default-face))
                       (t (propertize status 'face 'bespoke-header-ro-face))))
         (left (concat
                (propertize " "  'face 'header-line
			                'display `(raise ,space-up))
                (propertize name 'face 'header-line)
                (propertize " "  'face 'header-line
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
                        'face 'header-line)
	        (propertize right 'face 'header-line))))

;;;; Mode line status
;; ---------------------------------------------------------------------
(defun bespoke-modeline-status ()
  "Return buffer status: read-only (⨂), modified (⨀) or read-write (◯)"
  (let ((read-only   buffer-read-only)
        (modified    (and buffer-file-name (buffer-modified-p))))
    (cond (modified  "⨀") (read-only "⨂") (t "◯"))))

;;;; Default display
(defun bespoke-modeline-default-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (format-mode-line 'mode-name))
        (branch      (vc-project-branch))
        (position    (format-mode-line "%l:%c ")))
    (bespoke-modeline-compose (bespoke-modeline-status)
                              buffer-name
                              (concat "(" mode-name
                                      (if branch (concat " "
                                                         (propertize branch 'face 'italic)))
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
	    (br'mode-name      (vc-project-branch))
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
     "🞅"
     buffer-name
     (concat "(" mode-name
	         (if branch (concat ", "
				                (propertize branch 'face 'italic)))
	         ")" )
     page-number)))

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
                              (concat (propertize "⏲" 'face 'default 'display `(raise ,space-up)) (format-time-string "%H:%M ")))))

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
;;;; Mode line header function
;; ---------------------------------------------------------------------
(defun bespoke-modeline ()
  "Install a header line whose content depends on the major mode"
  (interactive)
  (setq-default header-line-format
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

                         ;; ((bespoke-modeline-elfeed-search-mode-p)   (bespoke-modeline-elfeed-search-mode))
                         ;; ((bespoke-modeline-elfeed-show-mode-p)     (bespoke-modeline-elfeed-show-mode))
                         ;; ((bespoke-modeline-mu4e-dashboard-mode-p)  (bespoke-modeline-mu4e-dashboard-mode))
                         ;; ((bespoke-modeline-mu4e-main-mode-p)       (bespoke-modeline-mu4e-main-mode))
                         ;; ((bespoke-modeline-mu4e-headers-mode-p)    (bespoke-modeline-mu4e-headers-mode))
                         ;; ((bespoke-modeline-mu4e-view-mode-p)       (bespoke-modeline-mu4e-view-mode))
                         ;; ((bespoke-modeline-bespoke-help-mode-p)       (bespoke-modeline-bespoke-help-mode))
                         (t                                      (bespoke-modeline-default-mode)))))))

;;;; Update Window Display
;; ---------------------------------------------------------------------
(defun bespoke-modeline-update-windows ()
  "Modify the mode line depending on the presence of a window below."

  (dolist (window (window-list))
    (with-selected-window window
      (if (or (one-window-p t)
	          (eq (window-in-direction 'below) (minibuffer-window))
	          (not (window-in-direction 'below)))
	      (with-current-buffer (window-buffer window)
	        (setq mode-line-format (list "")))
        ;; (setq mode-line-format (list "")))
	    (with-current-buffer (window-buffer window)
 	      (setq mode-line-format nil)))
      ;;      (if (window-in-direction 'above)
      ;;	      (face-remap-add-relative 'header-line '(:overline "#777777"))
      ;;	    (face-remap-add-relative 'header-line '(:overline nil)))
      )))
(add-hook 'window-configuration-change-hook 'bespoke-modeline-update-windows)

;;;; Call Mode line
(setq eshell-status-in-modeline nil)
(setq-default mode-line-format (list "%-"))
;; (setq-default mode-line-format "")
(bespoke-modeline)



;;; End Bespoke Modeline
(provide 'bespoke-modeline)
