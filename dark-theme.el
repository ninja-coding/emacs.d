(add-to-list 'custom-theme-load-path "~/git/emacs.d")
(load-theme 'wombat-custom t)
(set-face-attribute 'mode-line nil :box nil)                  
(set-face-attribute 'mode-line-inactive nil :box nil)

;; Powerline
(add-to-list 'load-path "~/git/powerline")
(require 'powerline)
(powerline-default-theme)


(provide 'dark-theme)
