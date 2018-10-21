;;; mx-private.el --- My Private Configuration for mxem


(dolist (hook '(c-mode-hook c++-mode-hook))
  (with-eval-after-load 'company
    (add-hook hook (lambda ()
                     (setq-local company-minimum-prefix-length 2)
                     (setq-local company-backends
                                 '(company-c-headers
                                   company-irony
                                   (company-dabbrev-code company-files)
                                   company-dabbrev))))))

(setq tern-command (list "tern" "--no-port-file"))

(setq-default
 lua-indent-level 4
 lua-indent-string-contents t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(global-set-key (kbd "M-y") 'counsel-yank-pop)

(provide 'mx-private)
