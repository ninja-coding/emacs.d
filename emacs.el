(add-to-list 'load-path (expand-file-name "~/git/emacs.el/"))

;;;;;;;;;;;;;;;;;;;
;; Package repos ;;
;;;;;;;;;;;;;;;;;;;
(require 'setup-package)
(require 'benchmark-init)

;;;;;;;;
;; UI ;;
;;;;;;;;
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

;; Window title ;; %b instead of %f to exclude path
(setq frame-title-format '(buffer-file-name "%f - GNU Emacs 24"))

(setq inhibit-startup-message t)              ;; Login Screen
(show-paren-mode 1)                           ;; Highlight matching parentheses
(setq show-paren-delay 0)
(setq echo-keystrokes 0.1)                    ;; Show keystrokes in progress
(set-default 'indent-tabs-mode nil)           ;; Never insert tabs
(setq-default tab-width 4)                    ;; Default tab width
(setq x-select-enable-clipboard t)            ;; Pasting selection outside of emacs
(delete-selection-mode 1)                     ;; Replace region with paste

(setq fill-column 80)                         ;; Lines should be 80 characters long
(setq enable-recursive-minibuffers t)         ;; Allow recursive minibuffers
(defalias 'yes-or-no-p 'y-or-n-p)             ;; 'y' or 'n' instead of yes o no

;; Hide menu bar, toolbar, scroll bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;;;;;;;;;;;;;;
;; Defaults ;;
;;;;;;;;;;;;;;

;; Save active buffer when frame loses focus (24.4)
;(add-hook 'focus-out-hook 'save-buffer)

;; Move between windows with shift + arrows
(if (fboundp 'windmove-default-keybindings) (windmove-default-keybindings))

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100)

(global-set-key (kbd "C-x C-b") 'ibuffer)         ;; ibuffer bind to C-x C-b
(global-set-key "\C-x\ \C-r" 'recentf-open-files) ;; recent files to C-x C-r

;; UTF-8 please
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; M-x enhancement
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Make zooming affect frame instead of buffers
(require 'zoom-frm)

;; Keep cursor away from edges when scrolling up/down
(require 'smooth-scrolling)

;; Clean up obsolete buffers automatically
(require 'midnight)

;; Represent undo-history as a tree (C-x u)
(setq undo-tree-mode-lighter "")
(require 'undo-tree)
(global-undo-tree-mode)

;; Load sr-speedbar
(load-file "~/git/emacs.el/sr-speedbar.el")
(global-set-key (kbd "C-c ñ") 'sr-speedbar-toggle)
;; Close sr-speedbar before exiting
(defadvice save-buffers-kill-emacs (before update-mod-flag activate)
  (sr-speedbar-close))

;; Load minimalism 
(load-file "~/git/emacs.el/minimalism.el")

;; Hide dired details
(require 'dired-details-plus)

;; Fullscreen / Maximised:
(defun switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))
(defun switch-maximized ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,maximized_vert,maximized_horz"))

;; Pairing parentheses
(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)
(add-hook 'python-mode-hook ; Just for python
          (lambda ()
            (define-key python-mode-map "'" 'skeleton-pair-insert-maybe)))


;;;;;;;;;;;;;;;;;
;; Keybindings ;;
;;;;;;;;;;;;;;;;;

(global-set-key [f11] 'switch-full-screen)
(global-set-key [f10] 'switch-maximized)
(global-set-key [f9] 'load-theme)             ;; Select theme bind to F9


;;;;;;;;;;;;;;
;; ido-mode ;;
;;;;;;;;;;;;;;

(require 'ido)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-case-fold nil
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10)

;; Try out flx-ido for better flex matching between words
(require 'flx-ido)
(flx-ido-mode 1)

;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; Ido at point ( C-, )
(require 'ido-at-point)
(ido-at-point-mode)

;; Use ido everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; flx-ido looks better with ido-vertical-mode
(require 'ido-vertical-mode)
(ido-vertical-mode)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)


;;;;;;;;;;;;;;;;
;; ERC config ;;
;;;;;;;;;;;;;;;;

;; ERC with ido:
(defun rgr/ido-erc-buffer()
  (interactive)
  (switch-to-buffer
   (ido-completing-read "Channel:" 
                        (save-excursion
                          (delq
                           nil
                           (mapcar (lambda (buf)
                                     (when (buffer-live-p buf)
                                       (with-current-buffer buf
                                         (and (eq major-mode 'erc-mode)
                                              (buffer-name buf)))))
                                   (buffer-list)))))))
(global-set-key (kbd "C-c e") 'rgr/ido-erc-buffer)


;;;;;;;;;;;;;;;;;;;;;
;; Auto completion ;;
;;;;;;;;;;;;;;;;;;;;;

;; Global
(require 'auto-complete)
(global-auto-complete-mode 1)
(require 'auto-complete-config nil t)
(setq ac-dwim t)
(ac-config-default)
;; custom keybindings to use tab and enter
(define-key ac-complete-mode-map "\t" 'ac-expand)
(define-key ac-complete-mode-map "\r" 'ac-complete)

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; For Python:
(add-hook 'python-mode-hook 'jedi:setup)


;;;;;;;;;;;;;;
;; Flycheck ;;
;;;;;;;;;;;;;;
(require 'flycheck)
(setq flycheck-highlighting-mode 'sexps) ;; or 'lines
(setq flycheck-completion-system 'ido)   ;; ido completion
(setq flycheck-indication-mode nil)

;; For python:
(add-hook 'python-mode-hook #'flycheck-mode)
;(add-hook 'python-mode-hook '(setq flycheck-highlighting-mode 'sexps))

;; Check when saved only
;(setq flycheck-check-syntax-automatically '(mode-enabled save))


;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

(require 'python-editing)

(require 'highlight-indentation)
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

(require 'flyspell)
(setq-default ispell-program-name "aspell")
(add-hook 'text-mode-hook 'flyspell-mode)          ;; ispell-change-dictionary language
(global-set-key "\C-cç" 'ispell-change-dictionary) ;; binded to C-c ç
(require 'iso-transl)                              ;; accent bug workaround


;;;;;;;;;;;;;;
;; Org Mode ;;
;;;;;;;;;;;;;;
;; git pull
;; make uncompiled

;; Load dev version of org-mode
(add-to-list 'load-path (expand-file-name "~/git/org-mode/lisp"))

;; Standard key bindings for org-mode
;(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


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
