;; NB: If you can't get this .emacs file to load successfully, you
;; should try M-x package-list-packages, then U, then x.

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))


(setq inhibit-splash-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore)

(tool-bar-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; I don't care that we're redefining tramp-read-passwd
(setq ad-redefinition-action 'accept)

(show-paren-mode 1)
(setq show-paren-delay 0)

(if (file-exists-p "/usr/local/lib/ciao/ciao-mode-init.el")
    (load-file "/usr/local/lib/ciao/ciao-mode-init.el"))

(package-initialize)

(defun install-if-missing (pkgs)
  (when pkgs
    (unless (package-installed-p (car pkgs) nil)
      (package-install (car pkgs)))
    (install-if-missing (cdr pkgs))))

(unless package-archive-contents
   (package-refresh-contents))

(install-if-missing
 '(ace-jump-mode async auto-complete auto-complete-pcmp auto-package-update
   ac-math autopair avy calfw calfw-gcal color-theme cyberpunk-theme dash
   elscreen elscreen-separate-buffer-list exec-path-from-shell faceup 
   flyspell-lazy hc-zenburn-theme helm helm-idris helm-j-cheatsheet 
   highlight idris-mode j-mode log4e magit magit-filenotify org-ac
   org-beautify-theme org-gcal paredit prop-menu popup racket-mode s
   sml-mode sml-modeline yaxception))

(let ((gnu-ls-path (executable-find "gls"))) 
 (when gnu-ls-path 
   (setq insert-directory-program gnu-ls-path)))

(setq flyspell-issue-welcome-flag nil);; easy spell check setup.
(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
(global-set-key (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)

(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
    (setq-default ispell-program-name "/usr/bin/aspell")) ;; What about Windows?

(setq-default ispell-list-command "list")

(require 'helm)
(require 'helm-config)
(require 'midnight)

;; Execute racket in emacs setup to install 

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

(add-hook 'racket-mode-hook (lambda () (define-key racket-mode-map (kbd "C-c r") 'racket-run)))
(setq tab-always-indent 'complete)

;; (global-linum-mode t)

;; These don't work. 
;; (add-hook 'auto-package-update-minor-mode-hook #'package-menu-mark-obsolete-for-deletion)
;; (add-hook 'auto-package-update-minor-mode-hook #'package-menu-execute)
(auto-package-update-maybe)

;; JBH
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

(require 'savehist)
(savehist-mode t)

(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(global-set-key (kbd "{") 'paredit-open-curly)
(add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
(add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
(add-hook 'emacs-lisp-mode-hook                    #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook   #'enable-paredit-mode)
(add-hook 'ielm-mode-hook                          #'enable-paredit-mode) ;; inferior-emacs-lisp-mode
(add-hook 'lisp-mode-hook                          #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook              #'enable-paredit-mode)
(add-hook 'scheme-mode-hook                        #'enable-paredit-mode)
(add-hook 'racket-mode-hook                        #'enable-paredit-mode)
(add-hook 'racket-repl-mode-hook                   #'enable-paredit-mode)
(add-hook 'idris-mode-hook                         #'enable-paredit-mode)
(add-hook 'idris-repl-mode-hook                    #'enable-paredit-mode)
(add-hook 'agda2-mode-hook                         #'enable-paredit-mode)
;; (add-hook 'ciao-mode-hook                         #'enable-paredit-mode) ;; not til fix paren space issue.
;; tex-mode has paredit-mode issue too. 
;; (add-hook 'idris-prover-script-mode-hook           #'enable-paredit-mode)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(setq window-themes-list '(wheatgrass manoj-dark cyberpunk tango-dark deeper-blue))

(if window-system
  (load-theme (nth (cl-random (length window-themes-list)) window-themes-list))
  (load-theme 'wombat t))

(mapc (lambda (pr) (put (car pr) 'racket-indent-function (cdr pr)))
      '((conde . 0)
        (fresh . 1)
        (run . 1)
        (run* . 1)
        (run . 2)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-modes
   (quote
    (emacs-lisp-mode lisp-mode lisp-interaction-mode slime-repl-mode c-mode cc-mode c++-mode go-mode java-mode malabar-mode clojure-mode clojurescript-mode scala-mode scheme-mode ocaml-mode tuareg-mode coq-mode agda-mode agda2-mode haskell-mode perl-mode cperl-mode python-mode ruby-mode lua-mode tcl-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode less-css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode web-mode ts-mode sclang-mode verilog-mode qml-mode racket-mode Racket-mode idris-mode racket-repl-mode idris-repl-mode ciao-mode)))
 '(custom-safe-themes
   (quote
    ("c86f868347919095aa44d2a6129dd714cbcf8feaa88ba954f636295b14ceff8f" "8fed5e4b89cf69107d524c4b91b4a4c35bcf1b3563d5f306608f0c48f580fdf8" "83e584d74b0faea99a414a06dae12f11cd3176fdd4eba6674422539951bcfaa8" "90edd91338ebfdfcd52ecd4025f1c7f731aced4c9c49ed28cfbebb3a3654840b" "f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" "a507b9ca4a605d5256716da70961741b9ef9ec3246041a4eb776102e8df18418" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(helm-M-x-fuzzy-match (quote (quote t)))
 '(mac-option-modifier (quote (:ordinary meta :mouse alt)))
 '(racket-racket-program "racket")
 '(safe-local-variable-values
   (quote
    ((eval progn
	   (let
	       ((tt-root-directory
		 (when buffer-file-name
		   (locate-dominating-file buffer-file-name ".dir-locals.el")))
		(tt-project-find-file
		 (and
		  (boundp
		   (quote tt-project-find-file))
		  tt-project-find-file)))
	     (setq tags-file-name
		   (concat tt-root-directory "TAGS"))
	     (unless tt-project-find-file
	       (setq compile-command
		     (concat "make -C " tt-root-directory)))
	     (setq default-directory tt-root-directory))))))
 '(scheme-program-name "/usr/bin/petite")
 '(vc-follow-symlinks (quote t)))

;; For 311, to make continuations RI.
;; Assumes k has some formal parameters
;; Leave mark at end of last match line in apply-k.

(fset 'make-k-ri
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217749 134217749 134217749 134217734 134217732 134217732 134217732 134217749 201326624 134217847 134217749 134217730 134217734 25 134217730 134217730 201326624 134217847 134217732 25 32 134217749 201326624 134217765 32 return 32 44 return 33 134217749 96 2 201326624 23 134217732 134217734 134217734 return 25 134217732 25 201326624 201326624 23 134217749 134217730 134217734 201326624 23 134217749 134217749 201326624 tab 134217730 134217734 134217748 2 2 2 134217730 134217730 134217734 25 134217749 201326624 tab 134217734 134217730 134217734 2 134217730 134217730 134217734] 0 "%d")) arg)))

;; Indent regions C-x <tab> left or right. Mix with C-u `num` for multi
;; M-x set-input-method RETURN TeX RETURN write unicode chars
;; in Racket M-\ to change input mode.

;; Right now, this is busted in the agda-mode repository. 13/12/15
(when (eq system-type 'darwin)
  (load-file 
    (let ((coding-system-for-read 'utf-8))
      (shell-command-to-string "agda-mode locate"))))

;; Emacs desiderata
;; Setup emacs calendar to sync with google calendar
;; Have width of linum buffer scale as font-size increases.
;; Set a higher default font size (point size).
;; Make it Windows 7/8/10 appropriate. --- see Google Keep
;; Automatically remove obsolete packages
;; Get code to color parens again. 
;; Setup package-pinned-packages, so as to draw from the correct package repo. 
;; Spacing with parens in various non-lisp modes that you use w/paredit mode.

