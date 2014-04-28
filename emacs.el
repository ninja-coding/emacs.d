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
; "C-c f" -> anaconda-mode-find-definition

;;;;;;;;;;;;;;;;;;;
;; Package repos ;;
;;;;;;;;;;;;;;;;;;;
(require 'setup-package)

;;;;;;;;
;; UI ;;
;;;;;;;;
(if window-system (require 'theme))
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

;; Window title ;; %b instead of %f to exclude path
(setq frame-title-format '(buffer-file-name "%f - GNU Emacs 24"))

(setq inhibit-startup-message t)              ;; Login Screen
(setq initial-scratch-message "")

(show-paren-mode 1)                           ;; Highlight matching parentheses
(setq show-paren-delay 0)
(setq echo-keystrokes 0.1)                    ;; Show keystrokes in progres
(set-default 'indent-tabs-mode nil)           ;; Never insert tabs
(setq-default tab-width 4)                    ;; Default tab width
(setq x-select-enable-clipboard t)            ;; Pasting selection outside of emacs
(delete-selection-mode 1)                     ;; Replace region with paste
(setq fill-column 80)                         ;; Lines should be 80 characters long
(defalias 'yes-or-no-p 'y-or-n-p)             ;; 'y' or 'n' instead of yes o no

;; Hide menu bar, toolbar, scroll bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Move between windows with shift + arrows
(if (fboundp 'windmove-default-keybindings) (windmove-default-keybindings))

;; Save a list of recent files visited.
(recentf-mode 1)
(setq recentf-max-saved-items 20)

(global-set-key (kbd "C-x C-b") 'ibuffer)            ;; ibuffer bind to C-x C-b
(global-set-key (kbd "C-x C-r") 'recentf-open-files) ;; recent files to C-x C-r

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

;; Keep cursor away from edges when scrolling up/down
(require 'smooth-scrolling)

;; Clean up obsolete buffers automatically
(require 'midnight)

;; Represent undo-history as a tree (C-x u)
(setq undo-tree-mode-lighter "")
(require 'undo-tree)
(global-undo-tree-mode)

;; Load minimalism 
(load-file "~/git/emacs.d/minimalism.el")

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
(setq company-idle-delay t)  ; Start inmediately

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;;; use popup menu for yas-choose-value
(require 'popup)
(define-key popup-menu-keymap (kbd "M-n") 'popup-next)
(define-key popup-menu-keymap (kbd "TAB") 'popup-next)
(define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
(define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
(define-key popup-menu-keymap (kbd "M-p") 'popup-previous)
(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))
(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))


;;;;;;;;;;;;;;
;; Flycheck ;;
;;;;;;;;;;;;;;

(require 'flycheck)
(setq flycheck-highlighting-mode 'sexps) ;; or 'lines
(setq flycheck-indication-mode nil)


;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc)
(add-to-list 'company-backends 'company-anaconda)
(global-set-key (kbd "C-c f") 'anaconda-mode-find-definition)
(add-hook 'python-mode-hook #'flycheck-mode)

(require 'python-editing)
(require 'highlight-indentation)
(add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)


;;;;;;;
;; C ;;
;;;;;;;

(setq c-default-style "linux"
      c-basic-offset 4)
;; (add-hook 'c-mode-hook 'highlight-indentation-current-column-mode)  <- slows emacs in c-mode
(load "c-eldoc")
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'c-mode-hook #'flycheck-mode)


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

;; org-LaTeX
(setq org-latex-create-formula-image-program 'dvipng)
(set-default 'preview-scale-function 1.1)
;(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

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
