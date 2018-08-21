;;; mx-private.el --- Private Configuration for mxem on Mac

(set-frame-size (selected-frame) 128 36)

(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(provide 'mx-private)
