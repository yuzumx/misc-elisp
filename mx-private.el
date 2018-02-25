;;; mx-private.el --- My Private Configuration for XMXE


(dolist (hook '(c-mode-hook c++-mode-hook))
  (with-eval-after-load 'company
    (add-hook hook (lambda ()
                     (setq-local company-minimum-prefix-length 2)
                     (setq-local company-backends '((company-irony company-irony-c-headers)
                                                    company-dabbrev-code
                                                    company-files
                                                    company-dabbrev))))))

(with-eval-after-load 'tern
  (add-to-list 'tern-command "--no-port-file" 'append))

(setq-default lua-indent-level 4)
(setq-default lua-indent-string-contents t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(provide 'mx-private)
