(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "11d069fbfb0510e2b32a5787e26b762898c7e480364cbc0779fe841662e4cf5d" "70cf411fbf9512a4da81aa1e87b064d3a3f0a47b19d7a4850578c8d64cac2353" "97a2b10275e3e5c67f46ddaac0ec7969aeb35068c03ec4157cf4887c401e74b1" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "0f0e3af1ec61d04ff92f238b165dbc6d2a7b4ade7ed9812b4ce6b075e08f49fe" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(erc-hide-list (quote ("JOIN" "QUIT")))
 '(erc-modules (quote (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notifications readonly ring stamp track))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;; UI ;;;;
;(load-theme 'solarized-dark t)                 ;; Color theme
;(add-hook 'kill-emacs-hook 'sr-speedbar-close)
(defadvice save-buffers-kill-emacs (before update-mod-flag activate)
    (sr-speedbar-close))                        ;; Close sr-speedbar before exiting
;(add-to-list 'load-path "~/git/powerline")
;(require 'powerline)                           ;; Mode line replacement
;(powerline-center-theme)
(set-face-attribute 'mode-line nil 
					:box nil)                  
(set-face-attribute 'mode-line-inactive nil
                    :box nil)                  ;; Removes mode line boxes
(global-set-key [f9] 'load-theme)              ;; Select theme bind to F9
(setq frame-title-format "%b - GNU Emacs 24")  ;; Window title
(setq-default tab-width 4)                     ;; Default tab width
(setq mouse-wheel-mode nil)                    ;; Disable mouse wheel
;(helm-mode 1)                                 ;; Useful completion list, but can be annoying.
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)               ;; Hide toolbar, scroll bar, menu bar
  (scroll-bar-mode -1))
(global-set-key (kbd "C-x C-b") 'ibuffer)      ;; ibuffer bind to C-x C-b 
(when (fboundp 'windmove-default-keybindings)  ;; Move between windows with shift + arrows
  (windmove-default-keybindings))

;; Minimalism kit:
(add-to-list 'load-path "~/git/emacs.el")

;; Speedbar:
(load-file "~/.emacs.d/sr-speedbar.el")
(global-set-key (kbd "C-c ñ") 'sr-speedbar-toggle)  ;; sr-speedbar mapped to C-c ñ

;; ERC config
;'(erc-hide-list (quote ("JOIN" "QUIT")))  ;; Hide login and exit messages in erc
;'(erc-modules (quote (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notifications readonly ring stamp track))))

;; Python:
(load-file "~/.emacs.d/emacs-for-python/epy-init.el")
(epy-setup-checker "pyflakes %f")
(setq py-python-command "python3")
(add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'python-mode-hook 'linum-mode)

;; C:
(setq c-default-style "linux"
      c-basic-offset 4)

;; ispell:
(setq-default ispell-program-name "aspell")
(add-hook 'text-mode-hook 'flyspell-mode)          ;; ispell-change-dictionary language
(global-set-key "\C-cç" 'ispell-change-dictionary) ;; binded to C-c ç
(require 'iso-transl)                              ;; accent bug workaround

;; Package repos:
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;;;
;;;; Org Mode
;;;;
;(add-to-list 'load-path (expand-file-name "~/git/org-mode/lisp"))
;(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
;(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
; Standard key bindings for org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
; Agenda files:
(setq org-agenda-files (quote ("~/Dropbox/org/todo.org"
                               "~/git/file1"
                               "~/git/file2")))

;; Login Screen:
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
; Fullscreen / Maximised:
(defun switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))
(defun switch-maximized ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,maximized_vert,maximized_horz"))
(global-set-key [f11] 'switch-full-screen)
(global-set-key [f10] 'switch-maximized)

;;;;
;;;; Saving Sessions
;;;; wg-help for info
(require 'workgroups2)
; Change prefix key (before activating WG):
(setq wg-prefix-key (kbd "C-c z"))
; Change workgroups session file:
(setq wg-default-session-file "~/.emacs.d/.emacs_workgroups")
(workgroups-mode 1)                 ; Must be at the bottom of .emacs
