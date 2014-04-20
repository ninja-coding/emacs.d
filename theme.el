;; Theme
(add-to-list 'custom-theme-load-path "~/git/emacs.d")
;(load-theme 'wombat-custom t)

(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

;;;;;;;;;;
;; paren mode
(require 'paren)
    (set-face-background 'show-paren-match (face-background 'default))
    (set-face-foreground 'show-paren-match "#00a8b8")
    (set-face-attribute 'show-paren-match nil :weight 'extra-bold)

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
(require 'powerline)
(powerline-default-theme)

(provide 'theme)
