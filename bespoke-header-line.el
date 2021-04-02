;;; Define Extra Faces
(defface bespoke-header-default-face nil
  "Default face for ther header line."
  :group 'bespoke)

(defface bespoke-header-mod-face nil
  "Header line face for modified buffers."
  :group 'bespoke)

(defface bespoke-header-ro-face nil
  "Header line face for read-only buffers."
  :group 'bespoke)

(defface bespoke-header-line-dark-face nil
  "Header line face for read-only buffers."
  :group 'bespoke)

(defface bespoke-header-line-light-face nil
  "Header line face for read-only buffers."
  :group 'bespoke)

(defface bespoke-footer-dark-face nil
  "Header line face for read-only buffers."
  :group 'bespoke)

(defface bespoke-footer-light-face nil
  "Header line face for read-only buffers."
  :group 'bespoke)

(defface bespoke-mode-line-dark-face nil
  "Header line face for read-only buffers."
  :group 'bespoke)

(defface bespoke-mode-line-light-face nil
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

;;; Focus & Mode line
;; Ensure modeline is inactive when Emacs is unfocused (and active otherwise)
;; See https://github.com/seagle0128/doom-modeline/blob/00bc89b8ded4c452ccf65edce5dce6f9a1e41611/doom-modeline-core.el#L845
;; FIXME

(defvar bespoke-modeline-remap-face-cookie nil)
(defun bespoke-modeline-focus ()
  "Focus mode-line."
  (when bespoke-modeline-remap-face-cookie
    (require 'face-remap)
    (face-remap-remove-relative bespoke-modeline-remap-face-cookie)))
(defun bespoke-modeline-unfocus ()
  "Unfocus mode-line."
  (setq bespoke-modeline-remap-face-cookie
        (face-remap-add-relative 'mode-line 'mode-line-inactive)))

(with-no-warnings
  (if (boundp 'after-focus-change-function)
      (progn
        (defun bespoke-modeline-focus-change (&rest _)
          (if (frame-focus-state)
              (bespoke-modeline-focus)
            (bespoke-modeline-unfocus)))
        (advice-add #'handle-switch-frame :after #'bespoke-modeline-focus-change)
        (add-function :after after-focus-change-function #'bespoke-modeline-focus-change))
    (progn
      (add-hook 'focus-in-hook #'bespoke-modeline-focus)
      (add-hook 'focus-out-hook #'bespoke-modeline-unfocus))))

;;; Set Variable
(defvar set-bespoke-header-line t "Use bespoke header line theme")

;;; Clean Mode Line
;; https://www.masteringemacs.org/article/hiding-replacing-modeline-strings
;; NOTE: this is only for minor and major modes
(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (yas/minor-mode . " υ")
    (paredit-mode . " π")
    (eldoc-mode . "")
    (abbrev-mode . "")
    ;; Major modes
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

;;; Mode/Header line functions
;; Mode line in header
;; Organize mode line
(defun vc-branch ()
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "" (substring-no-properties vc-mode
                                             (+ (if (eq backend 'Hg) 2 3) 2)) " "))  nil))

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


;; Organize mode-line left and right
(defun mode-line-render (left right)
  "Organize mode line entries to left and right"
  (let* ((available-width (- (window-width) (length left) )))
    (format (format "%%s %%%ds" available-width) left right)))

;; Inactive Header line
;; https://emacs.stackexchange.com/a/3522/11934
(defun bespoke-update-header ()
  (mapc
   (lambda (window)
     (with-current-buffer (window-buffer window)
       ;; don't mess with buffers that don't have a header line
       (when header-line-format
         (let ((original-format (get 'header-line-format 'original))
               (inactive-face 'fringe)) ; change this to your favorite inactive header line face
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
                                            (propertize " ⦿ " 'face `(:inherit bespoke-header-mod-face :weight bold :height 1.10)))
                                           ;; other unicode symbols: ✱ Ⓡ ⓦ ⊕ 🞊 ⨁ ⨂ ⨀ ◯ ⦿ ⊗ 🞅
                                           (buffer-read-only
                                            (propertize " ⊗ " 'face `(:inherit bespoke-header-ro-face :weight bold :height 1.10)))
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
  (setq-default header-line-format bespoke--mode-line))


;;; Function to (re)load header line
;;;###autoload
(defun bespoke-header-line ()
  "Replace header line with mode line"
  (interactive)
  (progn
    (window-divider-mode 1)
    (setq-default header-line-format mode-line-format)
    (setq-default mode-line-format'(""))
    (setq x-underline-at-descent-line t)))


;;; Load Mode or Header line
(if set-bespoke-header-line
    (progn
      (bespoke--set-header-line-content))
  (progn
    (setq-default header-line-format nil)
    (setq-default mode-line-format bespoke--mode-line)))

(provide 'bespoke-header-line)
