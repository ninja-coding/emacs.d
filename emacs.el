;; Set up load path
(add-to-list 'load-path (expand-file-name "~/git/emacs.el/"))

;;;;;;;;;;;;;;;;;;;
;; Package repos ;;
;;;;;;;;;;;;;;;;;;;
(require 'setup-package)

;;;;;;;;
;; UI ;;
;;;;;;;;
(load-theme 'solarized-light t)                ;; Color theme

;; Window title ;; %b instead of %f to exclude path
(setq frame-title-format '(buffer-file-name "%f - GNU Emacs 24"))

;; Login Screen
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(global-hl-line-mode 1)                       ;; Highlight current line
(show-paren-mode 1)                           ;; Highlight matching parentheses
(setq echo-keystrokes 0.1)                    ;; Show keystrokes in progress
(global-set-key (kbd "C-x C-b") 'ibuffer)     ;; ibuffer bind to C-x C-b 
(setq-default tab-width 4)                    ;; Default tab width
(set-default 'indent-tabs-mode nil)           ;; Never insert tabs
(setq x-select-enable-clipboard t)            ;; Allow pasting selection outside of emacs
(delete-selection-mode 1)                     ;; Replace region with paste
(global-set-key [f9] 'load-theme)             ;; Select theme bind to F9

;; Removes mode line boxes
(set-face-attribute 'mode-line nil :box nil)                  
(set-face-attribute 'mode-line-inactive nil :box nil)

;; Hide menu bar, toolbar, scroll bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;;;;;;;;;;;;;
;; Defaults ;;
;;;;;;;;;;;;;;
;; Move between windows with shift + arrows
(if (fboundp 'windmove-default-keybindings) (windmove-default-keybindings))

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't be so stingy on the memory, we have lots now. It's 
(setq gc-cons-threshold 20000000)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; M-x enhancement
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Make zooming affect frame instead of buffers
(require 'zoom-frm)

;; Keep cursor away from edges when scrolling up/down
(require 'smooth-scrolling)

;; Represent undo-history as an actual tree (visualize with C-x u)
(setq undo-tree-mode-lighter "")
(require 'undo-tree)
(global-undo-tree-mode)

;; Completion list, can be annoying.
;(helm-mode 1)

;; Load sr-speedbar
(load-file "~/.emacs.d/sr-speedbar.el")
;; sr-speedbar mapped to C-c ñ
(global-set-key (kbd "C-c ñ") 'sr-speedbar-toggle)
;; Close sr-speedbar before exiting
(defadvice save-buffers-kill-emacs (before update-mod-flag activate)
  (sr-speedbar-close))

;; Mode line replacement
;(add-to-list 'load-path "~/git/powerline")
;(require 'powerline)
;(powerline-center-theme)

;; Load minimalism 
(load-file "~/git/emacs.el/minimalism.el")

;; Fullscreen / Maximised:
(defun switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))
(defun switch-maximized ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,maximized_vert,maximized_horz"))
(global-set-key [f11] 'switch-full-screen)
(global-set-key [f10] 'switch-maximized)

;(provide 'setup-defaults) ;; in other file for (require 'setup-defaults)

;;;;;;;;;;;;;;;;
;; ERC config ;;
;;;;;;;;;;;;;;;;
;'(erc-hide-list (quote ("JOIN" "QUIT")))  ;; Hide login and exit messages in erc
;'(erc-modules (quote (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notifications readonly ring stamp track))))

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;
(load-file "~/.emacs.d/emacs-for-python/epy-init.el")
(epy-setup-checker "pyflakes %f")
(setq py-python-command "python3")
(add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'python-mode-hook 'linum-mode)

;;;;;;;
;; C ;;
;;;;;;;
(setq c-default-style "linux"
      c-basic-offset 4)

;;;;;;;;;;;;
;; ispell ;;
;;;;;;;;;;;;
(setq-default ispell-program-name "aspell")
(add-hook 'text-mode-hook 'flyspell-mode)          ;; ispell-change-dictionary language
(global-set-key "\C-cç" 'ispell-change-dictionary) ;; binded to C-c ç
(require 'iso-transl)                              ;; accent bug workaround

;;;;;;;;;;;;;;
;; Org Mode ;;
;;;;;;;;;;;;;;
(add-to-list 'load-path (expand-file-name "~/git/org-mode/lisp"))  ;; Load dev version of org-mode
;(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))  ;; Autoload org-mode
;(add-hook 'org-mode-hook 'turn-on-font-lock)  ;; not needed when global-font-lock-mode is on
;; Standard key bindings for org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
; Agenda files:
(setq org-agenda-files (quote ("~/Dropbox/org/todo.org"
                               "~/git/file1"
                               "~/git/file2")))

;;;;;;;;;;;;;;;;;;;;;
;; Saving Sessions ;;
;;;;;;;;;;;;;;;;;;;;;
;wg-help for info
(require 'workgroups2)
; Change prefix key (before activating WG):
(setq wg-prefix-key (kbd "C-c z"))

; Change workgroups session file:
(setq wg-default-session-file "~/.emacs.d/.emacs_workgroups")
(workgroups-mode 1)                 ; Must be at the bottom of .emacs
