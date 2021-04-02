;;; Version Requirements
(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))


;;; Load dark theme for terminal
(when (not (display-graphic-p))
  (load-theme 'bespoke-dark t))

;;; Modeline

(require 'bespoke-header-line)

;;; Provide path to file

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))


;;; End Theme
(provide 'bespoke-themes)
