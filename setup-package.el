(require 'package)

;; Repos
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

;; el-get
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

(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

;; Dash is required
(setq package-list '(dash dash-functional))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'dash)
(defun packages-install (packages)
  (--each packages
    (when (not (package-installed-p it))
      (package-install it)))
  (delete-other-windows))

;;; On-demand installation of packages

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(provide 'setup-package)

;;-m Install requirements
(defun init--install-packages ()
  (packages-install
   '(magit
     gist
     flycheck
     yasnippet
     smartparens
     ido-vertical-mode
     ido-at-point
     smooth-scrolling
     elpy
     flx-ido
     ido-ubiquitous
     git-commit-mode
     gitconfig-mode
     gitignore-mode)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))
