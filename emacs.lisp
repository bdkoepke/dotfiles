;;; package --- summary
;;; Commentary:
;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("MELPA Stable" . "http://stable.melpa.org/packages/")
        ("MELPA" . "http://melpa.org/packages/")
        ("ELPA" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(defun setup-programming ()
  "Programming environment settings."
  (use-package projectile
    :ensure t)
  (use-package magit ; Git
    :ensure t
    :config (global-set-key (kbd "C-x g") 'magit-status))
  (use-package realgud ; Debugger
    :ensure t)
  (use-package powerline ;
    :ensure t
    :config (powerline-default-theme))
  (use-package neotree ; Dired alternative
    :ensure t
    :config (global-set-key [f8] 'neotree-toggle))
  (use-package auto-complete ; Text completion
    :ensure t
    :init (add-to-list 'ac-modes 'latex-mode)
    :config (global-auto-complete-mode t))
  (use-package flycheck ; Syntax checking
    :ensure t
    :init (global-flycheck-mode)))

(defun setup-general ()
  "Setup theme settings including fonts and colors."
  (if (display-graphic-p)
      (progn
        (use-package solarized-theme
          :ensure t
          :config (load-theme 'solarized-light t))
        (tool-bar-mode -1)))
  (set-frame-font "Menlo 14")
  (setq inhibit-startup-screen t)
  (setq-default indent-tabs-mode nil))

(defun setup-javascript ()
  "JavaScript settings."
  (defun use-package-prettier-js ()
    (progn
      (add-to-list 'load-path "/usr/local/lib/node_modules/prettier/editors/emacs")
      (require 'prettier-js)
      (setq prettier-target-mode "js2-jsx-mode")
      (add-hook 'js2-mode-hook
                (lambda ()
                  (add-hook 'before-save-hook 'prettier-before-save)))
      (setq prettier-args '("--trailing-comma" "es5"
                            "--single-quote" "true"
                            "--print-width" "100"))))
  (use-package js2-mode
    :ensure t
    :config (progn
              (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
              (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
              (add-hook 'js-mode-hook 'js2-minor-mode)))
  (use-package ac-js2
    :ensure t
    :config (add-hook 'js2-mode-hook 'ac-js2-mode))
  (use-package tern
    :ensure t
    :config (progn
              (add-hook 'js-mode-hook 'tern-mode)
              (add-hook 'js2-mode-hook 'tern-mode)
              (setq tern-command '("node" "/usr/local/bin/tern"))))
  (use-package tern-auto-complete
    :ensure t
    :config (tern-ac-setup))
  "(use-package-prettier-js)"
  (setq js-indent-level 2))

(defun setup-tex ()
  "TeX settings."
  (use-package tex
    :defer t
    :ensure auctex
    :config (progn
              (setq TeX-PDF-mode t)
              (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
              (setq exec-path (append exec-path '("/Library/TeX/texbin")))))
  (use-package ac-math
    :ensure t
    :config (setq ac-sources
                  (append '(ac-source-math-unicode
                            ac-source-math-latex
                            ac-source-latex-commands
                            ac-sources)))))            

(defun setup-shell ()
  "Shell settings."
  (defun toolbear:term-handle-more-ansi-escapes (proc char)
    "Handle additional ansi escapes."
    (cond ; Fix node backspace issue: 
     ((eq char ?G)
      (let ((col (min term-width (max 0 term-terminal-parameter))))
        (term-move-columns (- col (term-current-column)))))
     (t)))
  (advice-add 'term-handle-ansi-escape :before #'toolbear:term-handle-more-ansi-escapes)
  (use-package exec-path-from-shell
    :ensure t
    :config (exec-path-from-shell-initialize))
  (use-package multi-term
    :ensure t
    :config (setq ansi-term-program "/bin/bash"))
  (add-hook 'term-mode-hook
            (lambda ()
              (define-key term-raw-map (kbd "C-y") 'term-paste))))

(defun setup-languages ()
  "Setup Programming Languages."
  (setup-general)
  (setup-shell)
  (setup-javascript)
  (setup-tex))

(setup-general)
(setup-languages)
(setup-programming)
(ac-config-default)

(provide '.emacs)
;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ess solarized-theme use-package realgud magit js2-mode flycheck fiplr exec-path-from-shell company-tern company-auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
