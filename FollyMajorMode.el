(setq folly-keywords
 '(("HYPOTHESIS:\\|CONCLUSION:" . font-lock-keyword-face)
   ("&\\|->\\|~\\|<->\\|\|" . font-lock-builtin-face)
   ("V\\|E\\|\\." . font-lock-string-face)))

(define-derived-mode folly-mode fundamental-mode
  (setq font-lock-defaults '(folly-keywords))
  (setq mode-name "folly-mode"))

(add-to-list 'auto-mode-alist '("\\.folly\\'" . folly-mode))

(provide 'folly-mode)
