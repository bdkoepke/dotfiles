;;; package --- summary
;;; Commentary:
;;; Code:
(defun setup-packages ()
  "Ensure packages are setup correctly."
  (require 'package)
  (add-to-list 'package-archives
               '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("MELPA" . "https://melpa.org/packages/") t)
  ;; Packages:
  (package-initialize)
  (let ((packages
         (let ((JavaScript '(indium js2-mode))
               (TeX '(auctex))
               (Programming '(company-tern flycheck realgud))
               (General '(websocket use-package)))
           (append JavaScript TeX Programming General))))
    (dolist (package packages)
      (unless (package-installed-p package)
        (package-install package)))))

(defun setup-flycheck ()
  "Initialize flycheck globally."
  (use-package flycheck
    :ensure t
    :init (global-flycheck-mode)))

(defun setup-general ()
  "Setup theme settings including fonts and colors."
  (if window-system
      (progn
        (load-theme 'solarized-light t)
        (tool-bar-mode -1)))
  (set-frame-font "Menlo 14")
  (setq-default indent-tabs-mode nil))

(defun setup-js ()
  "JavaScript settings."
  (setq js-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (require 'indium)
  (add-hook 'js-mode-hook (lambda () (indium-interaction-mode 1))))

(defun setup-tex ()
  "TeX specific settings."
  (setq TeX-PDF-mode t)
  (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
  (setq exec-path (append exec-path '("/Library/TeX/texbin"))))

(setup-packages)
(setup-general)
(setup-flycheck)
(setup-js)
(setup-tex)

(provide '.emacs)
;;; .emacs ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
