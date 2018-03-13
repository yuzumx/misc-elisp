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

(defun remx/find-mxem-private-config ()
  "Find mxem private configuration file."
  (interactive)
  (let ((remx-private-settings-file (expand-file-name "lisp/mx-private.el" user-emacs-directory)))
    (find-file-existing remx-private-settings-file)))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-M-/") 'company-other-backend))

(provide 'mx-private)
