;; All credit goes to Bastien Guerry - http://bzg.fr


;;;;
;;;; Hide mode bar
;;;;
(defvar-local hidden-mode-line-mode nil)
(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))
;(hidden-mode-line-mode 1)
;; If you want to hide the mode-line in every buffer by default
;; (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)


;;
;; Big fringe mode
;;
(defvar bzg-big-fringe-mode nil)
(define-minor-mode bzg-big-fringe-mode
  "Minor mode to use big fringe in the current buffer."
  :init-value nil
  :global t
  :variable bzg-big-fringe-mode
  :group 'editing-basics
  (if (not bzg-big-fringe-mode)
      (set-fringe-style nil)
    (set-fringe-mode
     (/ (- (frame-pixel-width)
           (* 100 (frame-char-width)))
        2))))

;; Now activate this global minor mode
(bzg-big-fringe-mode 1)

;; To activate the fringe by default and deactivate it when windows
;; are split vertically:
(add-hook 'window-configuration-change-hook
          (lambda ()
            (if (delq nil
                      (let ((fw (frame-width)))
                        (mapcar (lambda(w) (< (window-width w) fw))
                                (window-list))))
                (bzg-big-fringe-mode 0)
              (bzg-big-fringe-mode 1))))

;; Use a minimal cursor
(setq cursor-type 'hbar)

;; Get rid of the indicators in the fringe
(mapcar (lambda(fb) (set-fringe-bitmap-face fb 'org-hide))
        fringe-bitmaps)

;; Set the color of the fringe
(custom-set-faces
 '(fringe ((t (:background "white")))))
