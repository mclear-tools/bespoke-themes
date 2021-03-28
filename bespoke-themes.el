;;; Version Requirements
(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))


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


;;; Load dark theme for terminal
(when (not (display-graphic-p))
  (load-theme 'bespoke-dark t))

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

;; ;;Modeline
;; (defvar set-bespoke-header-line)

;; (defun set-bespoke-dark-header ()
;;   "set dark header line"
;;   (bespoke-header-line)
;;   (set-face-attribute 'header-line nil
;;                       :foreground "#FFFEF9"
;;                       :background "#3b4252"
;;                       :box `(:line-width 5 :color "#3b4252" :height 150)
;;                       :overline nil
;;                       :underline nil
;;                       :height 150)
;;   (setq mini-frame-internal-border-color "#3B4252")
;;   (set-face-attribute 'mode-line nil
;;                       :height 10
;;                       :underline "#677691"
;;                       :overline nil
;;                       :box nil))

;; (defun set-bespoke-light-header ()
;;   (bespoke-header-line)
;;   (set-face-attribute 'header-line nil
;;                       :background "#E3E7EF"
;;                       :foreground "#2E3440"
;;                       :box '(:line-width 6  :color "#E3E7EF" :height 150)
;;                       :overline nil
;;                       :underline nil
;;                       :height 150)
;;   (setq mini-frame-internal-border-color "#D8DEE9")
;;   (set-face-attribute 'mode-line nil
;;                       :height 10
;;                       :underline "#D8DEE9"
;;                       :overline nil
;;                       :box nil))

;;; Provide path to file

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))


;;; End Theme
(provide 'bespoke-themes)
