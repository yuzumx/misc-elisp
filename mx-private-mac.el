;;; mx-private.el --- Private Configuration for mxem on Mac

(set-frame-size (selected-frame) 128 36)

(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(setq dired-use-ls-dired nil)
(setq dired-listing-switches "-alh --group-directories-first")

(remx/set-frame-opacity 88)

(provide 'mx-private)
