;; Theme
(add-to-list 'custom-theme-load-path "~/git/emacs.d")
(load-theme 'wombat-custom t)
(set-face-attribute 'mode-line nil :box nil)                  
(set-face-attribute 'mode-line-inactive nil :box nil)

;; Company-mode
(require 'color)

(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; Powerline
(add-to-list 'load-path "~/git/powerline")
(require 'powerline)
(powerline-default-theme)


(provide 'dark-theme)
