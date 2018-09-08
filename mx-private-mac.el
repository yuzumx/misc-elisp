;;; mx-private.el --- Private Configuration for mxem on Mac

(set-frame-size (selected-frame) 128 36)

(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(remx/set-transparency 88)

(provide 'mx-private)
