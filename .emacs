;;;; UI ;;;;
;(load-theme 'solarized-dark t)                 ;; Color theme
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
(load-file "~/git/emacs.el/minimalism.el")

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

;;
;; Package repos:
;;
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; el-get:
(add-to-list 'load-path (expand-file-name "~/git/el-get/"))
(setq el-get-github-default-url-type "https")
(add-to-list 'load-path "~/git/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;;;;
;;;; Org Mode
;;;;
(add-to-list 'load-path (expand-file-name "~/git/org-mode/lisp"))  ;; Load dev version of org-mode
;(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))  ;; Autoload org-mode
;(add-hook 'org-mode-hook 'turn-on-font-lock)  ;; not needed when global-font-lock-mode is on
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
