(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete)
(global-auto-complete-mode 't)

(require 'geiser)
(require 'geiser-guile)
(run-guile) ;; sure??
(require 'ac-geiser)

(require 'rainbow-mode)
(require 'rainbow-blocks)
(require 'rainbow-delimiters) ;;; hmm
(require 'pretty-symbols)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 inhibit-startup-screen t
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0
 show-paren-style 'parenthesis
 use-package-always-ensure t
 sentence-end-double-space nil
 create-lockfiles nil
 make-backup-files nil)

(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4)

(electric-indent-mode 0)
(fset 'yes-or-no-p 'y-or-n-p)

(menu-bar-mode -1)
(display-time)
(show-paren-mode +1)
(save-place-mode t) 

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq geiser-repl-use-other-window nil) ;;; hmm?

(global-set-key (kbd "C-<down>") (kbd "C-u 1 C-v"))
(global-set-key (kbd "C-<up>") (kbd "C-u 1 M-v"))

(global-set-key (kbd "<f2> <up>") 'windmove-up)
(global-set-key (kbd "<f2> <down>") 'windmove-down)
(global-set-key (kbd "<f2> <left>") 'windmove-left)
(global-set-key (kbd "<f2> <right>") 'windmove-right)

(global-set-key "\C-]" 'hs-toggle-hiding)
(global-set-key "\C-c\C-w" 'hs-hide-all)
(global-set-key "\C-c\C-s" 'hs-show-all)

(global-set-key (kbd "S-<right>") 'forward-sexp)
(global-set-key (kbd "S-<left>") 'backward-sexp)
(global-set-key (kbd "S-<up>") 'up-list)
(global-set-key (kbd "S-<down>") 'down-list)

(global-set-key (kbd "<f7>")'shell)
(global-set-key (kbd "<f9>") 'compile)
(global-set-key (kbd "<f5>") 'geiser-eval-region) ;;; sure?!

(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'geiser-repl-mode))

(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)

(add-hook 'scheme-mode-hook #'hs-minor-mode)
(add-hook 'scheme-mode-hook #'pretty-symbols-mode)
(add-hook 'scheme-mode-hook #'rainbow-blocks-mode)

(add-hook 'emacs-lisp-mode-hook #'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun toggle-hiding ()
  (interactive)
  (if (eq major-mode 'web-mode)
      (web-mode-fold-or-unfold)
      (hs-toggle-hiding)))

(defun my-pretty-lambda ()
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955) ; λ
          ;("phi" . 966)
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(roguel-ike pretty-symbols sokoban rainbow-mode rainbow-delimiters rainbow-blocks nyan-mode geiser-guile ac-geiser)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "red"))))
 '(font-lock-keyword-face ((t (:foreground "white"))))
 '(font-lock-string-face ((t (:foreground "magenta"))))
 '(rainbow-blocks-depth-1-face ((t (:foreground "orange"))))
 '(rainbow-blocks-depth-2-face ((t (:foreground "green"))))
 '(rainbow-blocks-depth-3-face ((t (:foreground "yellow"))))
 '(rainbow-blocks-depth-4-face ((t (:foreground "blue"))))
 '(rainbow-blocks-depth-5-face ((t (:foreground "orange"))))
 '(rainbow-blocks-depth-6-face ((t (:foreground "green"))))
 '(rainbow-blocks-depth-7-face ((t (:foreground "yellow"))))
 '(rainbow-blocks-depth-8-face ((t (:foreground "blue"))))
 '(rainbow-blocks-depth-9-face ((t (:foreground "orange"))))
 '(rainbow-blocks-unmatched-face ((t (:background "red" :foreground "white")))))
