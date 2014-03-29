(add-to-list 'load-path (expand-file-name "~/git/emacs.d/"))

;;;; List of keybindings
;
;; From python-editing.el:
; "C-c d" -> Duplicate line
; "C-c c" -> Duplicate line and comment the first
; "C-c l" -> Mark a line
; "M-<up>" -> Move line or region up
; "M-<down>" -> Move line or region down
; "C-c -" -> Expand snippet
;
;; From emacs.el:
; "C-x C-r" -> Recent files
; "C-c e" -> ERC chat list
; "C-c a" -> org-agenda
; "C-c b" -> org-iswitchb

;;;;;;;;;;;;;;;;;;;
;; Package repos ;;
;;;;;;;;;;;;;;;;;;;
(require 'setup-package)
(require 'benchmark-init)

;;;;;;;;
;; UI ;;
;;;;;;;;
(require 'dark-theme)
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

;; Save a list of recent files visited.
(recentf-mode 1)
(setq recentf-max-saved-items 20)

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
(load-file "~/git/emacs.d/sr-speedbar.el")
(global-set-key (kbd "C-c ñ") 'sr-speedbar-toggle)
;; Close sr-speedbar before exiting
(defadvice save-buffers-kill-emacs (before update-mod-flag activate)
  (sr-speedbar-close))

;; Load minimalism 
(load-file "~/git/emacs.d/minimalism.el")

;; Hide dired details
(require 'dired-details-plus)

;; Fullscreen / Maximized:
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
(global-set-key [f9] 'load-theme)


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

;; ERC with ido: (C-c e)
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

(global-company-mode 1)

;; Avoid yasnippet collision
(defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))

  (defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

  (defun tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (or (not yas/minor-mode)
              (null (do-yas-expand)))
          (if (check-expansion)
              (company-complete-common)
            (indent-for-tab-command)))))

  (global-set-key [tab] 'tab-indent-or-complete)

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; For Python:
;(add-hook 'python-mode-hook 'jedi:setup)

;; <C-tab> jedi:complete
;;     Complete code at point.
;; C-c ? jedi:show-doc
;;     Show the documentation of the object at point.
;; C-c . jedi:goto-definition
;;     Goto the definition of the object at point.
;; C-c , jedi:goto-definition-pop-marker
;;     Goto the last point where jedi:goto-definition was called.
;; variable (jedi:use-shortcuts nil)
;;     If non-nil, enable the following shortcuts:
;;     M-. jedi:goto-definition
;;     M-, jedi:goto-definition-pop-marker

;;;;;;;;;;;;;;
;; Flycheck ;;
;;;;;;;;;;;;;;
(require 'flycheck)
(setq flycheck-highlighting-mode 'sexps) ;; or 'lines
;(setq flycheck-completion-system 'ido)   ;; ido completion
(setq flycheck-indication-mode nil)

;; For python:
(add-hook 'python-mode-hook #'flycheck-mode)

;; Check when saved only
;(setq flycheck-check-syntax-automatically '(mode-enabled save))


;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc)
(add-to-list 'company-backends 'company-anaconda)

(require 'python-editing)
(require 'highlight-indentation)
(add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'python-mode-hook 'nlinum-mode)

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
(add-hook 'text-mode-hook 'flyspell-mode)          ;; Load in text mode
(global-set-key "\C-cç" 'ispell-change-dictionary) ;; Binded to C-c ç
(require 'iso-transl)                              ;; Accent bug workaround


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
