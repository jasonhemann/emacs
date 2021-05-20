;;; package --- Summary

;;; This is Jason Hemann's .emacs setup, intended for OSX and some
;;; linux machines. It is currently in an unstable state, and the
;;; non-.emacs dependencies are not listed. Several aspects of this
;;; rely on packages from homebrew, and a number of other downloaded
;;; files and hard-coded directories.

;;; Commentary:

;;; To anyone else trying to run or to load this file, you should
;;; consider the below.
;;; NB: If you can't get this .emacs file to load successfully, you
;;; should try M-x package-list-packages, then U, then x.

;;; Code:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")             t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")              t)

(setq debug-on-quit t
      inhibit-splash-screen t
      column-number-mode t     
      initial-scratch-message nil
      ring-bell-function 'ignore)

(tool-bar-mode -1)
(add-hook 'find-file-hook (lambda () (ruler-mode 1)))

(defalias 'yes-or-no-p 'y-or-n-p)

;; (straight-use-package '(flycheck-textlint :type git :host github :repo "kisaragi-hiu/flycheck-textlint"))
(straight-use-package 'ac-math)
(straight-use-package 'academic-phrases)
(straight-use-package 'ace-jump-mode)
(straight-use-package 'apel)
(straight-use-package 'auctex-latexmk)
(straight-use-package 'auto-complete-auctex)
;; (straight-use-package 'auto-complete-config)
(straight-use-package 'auto-package-update)
(straight-use-package 'autopair)
(straight-use-package 'bbdb)
(straight-use-package 'biblio)
(straight-use-package 'bibtex-completion)
(straight-use-package 'bog)
(straight-use-package 'calfw)
(straight-use-package 'calfw-cal)
(straight-use-package 'calfw-gcal)
(straight-use-package 'calfw-ical)
(straight-use-package 'calfw-org)
(straight-use-package 'cdlatex)
(straight-use-package 'clang-format)
(straight-use-package 'color-theme-modern)
(straight-use-package 'company-coq)
(straight-use-package 'company-dict)
(straight-use-package 'company-lean)
(straight-use-package 'company-math)
(straight-use-package 'coq-commenter)
(straight-use-package 'cyberpunk-theme)
(straight-use-package 'dash-functional)
;; (straight-use-package 'diction) Not a package!!!
(straight-use-package 'dictionary)
(straight-use-package 'dr-racket-like-unicode)
(straight-use-package 'easy-jekyll)
(straight-use-package 'eclim)

(straight-use-package 'elscreen-separate-buffer-list)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'f)
(straight-use-package 'flim)
(straight-use-package 'flymake-easy)
(straight-use-package 'flymake-racket)
(straight-use-package 'flymd)
(straight-use-package 'flyspell-lazy)
(straight-use-package 'gh-md)
(straight-use-package 'ghub)
(straight-use-package 'gradle-mode)
(straight-use-package 'graphql)
(straight-use-package 'hc-zenburn-theme)
(straight-use-package 'helm)
(straight-use-package 'helm-dictionary)
(straight-use-package 'helm-idris)
(straight-use-package 'helm-lean)
(straight-use-package 'helm-wordnet)
(straight-use-package 'ht)
(straight-use-package 'j-mode)
(straight-use-package 'jeison)
(straight-use-package 'langtool)
(straight-use-package 'lean-mode)
(straight-use-package 'magit-filenotify)
(straight-use-package 'magit-popup)
(straight-use-package 'markdown-mode+)
(straight-use-package 'markdown-preview-mode)
(straight-use-package 'mc-extras)
(straight-use-package 'meghanada)
(straight-use-package 'midnight)
(straight-use-package 'multiple-cursors)
(straight-use-package 'mustache)
(straight-use-package 'nlinum)
(straight-use-package 'org-ac)
(straight-use-package 'org-doing)
(straight-use-package 'org-dotemacs)
(straight-use-package 'org-roam)
(straight-use-package 'org-rtm)
(straight-use-package 'org-super-agenda)
(straight-use-package 'org-trello)
(straight-use-package 'org2web)
(straight-use-package 'paredit-everywhere)
(straight-use-package 'paredit-menu)
(straight-use-package 'parent-mode)
(straight-use-package 'powerthesaurus)
(straight-use-package 'proof-general)
(straight-use-package 'racket-mode)
(straight-use-package 'reazon)
(straight-use-package 'scheme-complete)
(straight-use-package 'semi)
(straight-use-package 'simple-httpd)
(straight-use-package 'sml-mode)
(straight-use-package 'sml-modeline)
(straight-use-package 'smog)
(straight-use-package 'tabbar)
(straight-use-package 'treepy)
(straight-use-package 'use-package)
(straight-use-package 'w3m)
(straight-use-package 'wanderlust)
(straight-use-package 'wordnut)
(straight-use-package 'wordsmith-mode)
(straight-use-package 'yaml-mode)
(straight-use-package 'zones)
(straight-use-package 'zygospore)

;;;;;;
;; (straight-use-package 'eldoro) maybe not a package
;; (straight-use-package 'helm-config) 

;; (package-install-file "~/Documents/org-inline-pdf.el/org-inline-pdf.el")
;; (require 'org-inline-pdf) ;; Trying, in case it works

;; I don't care that we're redefining tramp-read-passwd
(setq ad-redefinition-action 'accept)

;; (use-package smog
;;  :config (setq smog-command "style -L en"))


(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; (org-agenda-include-diary t)


;; IMAP
(setq elmo-imap4-default-server "imap.gmail.com")
(setq elmo-imap4-default-user "jason.hemann@gmail.com") 
(setq elmo-imap4-default-authenticate-type 'clear) 
(setq elmo-imap4-default-port '993)
(setq elmo-imap4-default-stream-type 'ssl)

(setq elmo-imap4-use-modified-utf7 t) 

;; SMTP
(setq wl-smtp-connection-type 'starttls)
(setq wl-smtp-posting-port 587)
(setq wl-smtp-authenticate-type "plain")
(setq wl-smtp-posting-user "jason.hemann")
(setq wl-smtp-posting-server "smtp.gmail.com")
(setq wl-local-domain "gmail.com")
(setq wl-message-id-domain "smtp.gmail.com")


(setq wl-default-folder "%inbox")
(setq wl-default-spec "%")
(setq wl-draft-folder "%[Gmail]/Drafts") ; Gmail IMAP
(setq wl-trash-folder "%[Gmail]/Trash")
(setq mail-user-agent 'wl-user-agent)
(setq wl-folder-check-async t) 

(setq wl-from "Jason Hemann <jason.hemann@gmail.com>"
      ;; All system folders (draft, trash, spam, etc) are placed in the
      ;; [Gmail]-folder, except inbox. "%" means it's an IMAP-folder
      wl-default-folder "%inbox"
      wl-draft-folder   "%[Gmail]/Drafts"
      wl-trash-folder   "%[Gmail]/Trash"
      ;; The below is not necessary when you send mail through Gmail's SMTP server,
      ;; see https://support.google.com/mail/answer/78892?hl=en&rd=1
      ;; wl-fcc            "%[Gmail]/Sent"

      ;; Mark sent messages as read (sent messages get sent back to you and
      ;; placed in the folder specified by wl-fcc)
      wl-fcc-force-as-read    t

      ;; For auto-completing foldernames
      wl-default-spec "%")

(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

(show-paren-mode 1)
(setq show-paren-delay 0)


(if (file-exists-p "/Users/jhemann/Documents/acl2/scripts-master/.lisp.el")
    (load-file "/Users/jhemann/Documents/acl2/scripts-master/.lisp.el"))

(if (file-exists-p "/usr/local/lib/ciao/ciao-mode-init.el")
    (load-file "/usr/local/lib/ciao/ciao-mode-init.el"))

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")


(add-hook 'java-mode-hook 'eclim-mode)

;; I should want maven, I think, tbqh

(add-hook 'java-mode-hook '(lambda() (gradle-mode 1)))

;; (load "~/Documents/eliemacs/eliemacs")

;; (unless (package-installed-p 'use-package)
;;   (progn
;;     (unless package-archive-contents
;;       (package-refresh-contents))
;;     (package-install 'use-package)))
;; On another yak-shave, use the following:
;; https://www.reddit.com/r/emacs/comments/47aq53/best_way_to_set_up_package_dependencies_in_initel/
;; to simplify the .emacs

(defun install-if-missing (pkgs)
  (when pkgs
    (unless (package-installed-p (car pkgs) nil)
      (package-install (car pkgs)))
    (install-if-missing (cdr pkgs))))

(unless package-archive-contents
   (package-refresh-contents))

(add-hook 'racket-mode-hook #'racket-xp-mode)
(require 'racket-xp) ;; Don't know what this is but I think it's not a package

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key [C-M-tab] 'clang-format-region)


(let ((gnu-ls-path (executable-find "gls")))
  (when gnu-ls-path
    (setq insert-directory-program gnu-ls-path)))

(global-flycheck-mode)

(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
	    (id (one-or-more (not (any " "))))
	    (message) line-end))
  :modes (text-mode markdown-mode gfm-mode))

(add-to-list 'flycheck-checkers 'proselint)

(setq flyspell-issue-welcome-flag nil);; easy spell check setup.
(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
(global-set-key (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)
(mac-auto-operator-composition-mode)
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
    (setq-default ispell-program-name "/usr/bin/aspell")) ;; What about Windows?

(setq-default ispell-list-command "list")

(setq langtool-language-tool-jar "~/LanguageTool-3.4/languagetool-commandline.jar")
(global-set-key "\C-x4w" 'langtool-check)
(global-set-key "\C-x4W" 'langtool-check-done)
(global-set-key "\C-x4l" 'langtool-switch-default-language)
(global-set-key "\C-x44" 'langtool-show-message-at-point)
(global-set-key "\C-x4c" 'langtool-correct-buffer)

(setq langtool-default-language "en-US")
(setq langtool-mother-tongue "en")

(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

(defun langtool-autoshow-detail-popup (overlays)
  (when (require 'popup nil t)
    ;; Do not interrupt current popup
    (unless (or popup-instances
                ;; suppress popup after type `C-g` .
                (memq last-command '(keyboard-quit)))
      (let ((msg (langtool-details-error-message overlays)))
        (popup-tip msg)))))

(setq langtool-autoshow-message-function 'langtool-autoshow-detail-popup)

;; Execute racket in emacs setup to install 

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; (when (executable-find "curl")
;;   (setq helm-google-suggest-use-curl-p t))

;; (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
;;       helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
;;       helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
;;       helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
;;       helm-ff-file-name-history-use-recentf t)

;; (helm-mode 1)

(add-hook 'racket-mode-hook (lambda () (define-key racket-mode-map (kbd "C-c r") 'racket-run)))
(setq tab-always-indent 'complete)

(global-nlinum-mode t)

;; These don't work. 
;; (add-hook 'auto-package-update-minor-mode-hook 'package-menu-mark-obsolete-for-deletion)
;; (add-hook 'auto-package-update-minor-mode-hook 'package-menu-execute)
;; (auto-package-update-maybe)

;; JBH
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

(global-set-key (kbd "C-S-s") (lambda () (interactive (insert "§"))))
(global-set-key (kbd "C-c (") (lambda () (interactive) (insert "ಠ_ಠ")))
(global-set-key (kbd "C-c )") (lambda () (interactive) (insert "¯\\_(ツ)_/¯")))
(global-set-key (kbd "C-c C-x (") (lambda () (interactive) (insert "ᕕ( ᐛ )ᕗ")))
(global-set-key (kbd "C-c C-x )") (lambda () (interactive) (insert "(－‸ლ)")))
;; !(•̀ᴗ•́)و ̑̑

;; ""
;; ebib mode for latex
(global-set-key "\C-ce" 'ebib)

(add-hook 'text-mode-hook 'writegood-mode)
(add-hook 'text-mode-hook 'artbollocks-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'tex-mode-hook (function (lambda () (setq ispell-parser 'tex))))
(add-hook 'tex-mode-hook 'auxtex-mode)

(add-hook 'tex-mode-hook (lambda () (define-key tex-mode-map (kbd "C-c C-k") 'compile)))
(add-hook 'tex-mode-hook (lambda () (define-key tex-mode-map (kbd "C-c |") 'align-current)))

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(add-hook 'scheme-mode-hook                        'flylisp-mode)
(add-hook 'inferior-scheme-mode-hook               'flylisp-mode)
(add-hook 'scheme-mode-hook                        'multiple-cursors-mode)
(add-hook 'inferior-scheme-mode-hook               'multiple-cursors-mode)

(require 'savehist)
(savehist-mode t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil) ;; (setq-default TeX-master "master") ; set a master for in the future.
;; (setq reftex-label-alist '(AMSTeX)) ;; For if I'm getting parens around my references
(setq reftex-plug-into-AUCTeX t)
(setq reftex-extra-bindings t)
(setq bib-cite-use-reftex-view-crossref t)

;; I grabbed this code off of the internet. The reftex-ref-style-alist
;; variables already had some of these cleveref options, so not sure
;; if I needed all of this. 
(eval-after-load
    "latex"
  '(TeX-add-style-hook
    "cleveref"
    (lambda ()
      (if (boundp 'reftex-ref-style-alist)
	  (add-to-list
	   'reftex-ref-style-alist
	   '("Cleveref" "cleveref"
             (("\\cref" ?c) ("\\Cref" ?C) ("\\cpageref" ?d) ("\\Cpageref" ?D)))))
      (reftex-ref-style-activate "Cleveref")
      (TeX-add-symbols
       '("cref" TeX-arg-ref)
       '("Cref" TeX-arg-ref)
       '("cpageref" TeX-arg-ref)
       '("Cpageref" TeX-arg-ref)))))
;; Also not sure what else I needed to do to make subsec: available by default

;; TeX-latex-mode, LaTeX-mode, TeX-mode, tex-mode, latex-mode, auxtex-mode

(add-hook 'latex-mode-hook 'turn-on-cdlatex)   ; with Emacs latex mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

(add-hook 'TeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'TeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX mode
(add-hook 'TeX-mode-hook 'visual-line-mode)
(add-hook 'TeX-mode-hook 'flyspell-preprocess-buffer) ;; somehow void
(add-hook 'TeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'TeX-mode-hook 'TeX-PDF-mode)
(add-hook 'TeX-mode-hook 'TeX-fold-mode)
(add-hook 'TeX-mode-hook (lambda () (TeX-fold-mode 1)))
(add-hook 'TeX-mode-hook #'TeX-fold-mode) ;; Automatically activate TeX-fold-mode.
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)

;; Only sometimes works!
;; must be after font locking is set up for the buffer on!
;; (add-hook 'find-file-hook 'TeX-fold-buffer t)

;; (autoload 'scheme-smart-complete "scheme-complete" nil t)

;; (eval-after-load 'scheme
;;   '(define-key scheme-mode-map "\t" 'scheme-complete-or-indent))

;; (autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
;; (add-hook 'scheme-mode-hook
;;   (lambda ()
;;     (make-local-variable 'eldoc-documentation-function)
;;     (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
;;     (eldoc-mode)))

;; (setq lisp-indent-function 'scheme-smart-indent-function)

;; (define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct)

;; You can also enable flyspell-popup-auto-correct-mode to popup that
;; Popup Menu automatically with a delay (default 1.6 seconds):

(add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)

(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(global-set-key (kbd "{") 'paredit-open-curly)
(add-hook 'racket-mode-hook      #'flylisp-mode)
(add-hook 'racket-repl-mode-hook #'flylisp-mode)
;; (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
;; (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
(add-hook 'emacs-lisp-mode-hook                    #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook   #'enable-paredit-mode)
(add-hook 'ielm-mode-hook                          #'enable-paredit-mode) ;; inferior-emacs-lisp-mode
(add-hook 'lisp-mode-hook                          #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook              #'enable-paredit-mode)
(add-hook 'scheme-mode-hook                        #'enable-paredit-mode)
(add-hook 'inferior-scheme-mode-hook               #'enable-paredit-mode)
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
        (run . 2)
	(letrec . 0)))

(setq-default major-mode 'text-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-auto-untabify t)
 '(TeX-engine 'xetex)
 '(ac-modes
   '(emacs-lisp-mode lisp-mode lisp-interaction-mode slime-repl-mode c-mode cc-mode c++-mode go-mode java-mode malabar-mode clojure-mode clojurescript-mode scala-mode scheme-mode ocaml-mode tuareg-mode coq-mode agda-mode agda2-mode haskell-mode perl-mode cperl-mode python-mode ruby-mode lua-mode tcl-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode less-css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode web-mode ts-mode sclang-mode verilog-mode qml-mode racket-mode Racket-mode racket-repl-mode idris-mode idris-repl-mode ciao-mode))
 '(bibtex-maintain-sorted-entries 'plain)
 '(custom-safe-themes
   '("b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" "2d835b43e2614762893dc40cbf220482d617d3d4e2c35f7100ca697f1a388a0e" "6bc387a588201caf31151205e4e468f382ecc0b888bac98b2b525006f7cb3307" "59e82a683db7129c0142b4b5a35dbbeaf8e01a4b81588f8c163bd255b76f4d21" "d1cc05d755d5a21a31bced25bed40f85d8677e69c73ca365628ce8024827c9e3" "834cbeacb6837f3ddca4a1a7b19b1af3834f36a701e8b15b628cad3d85c970ff" "923ee73494ea3611d2a1ff9f31bbf8d71b0b0cc2aeb4a6e0944ec6c83bc0ac23" "9fe1540491fcf692b8c639a3abacd32b29233bc4cb834a12a0fd1e01cbd0a128" "d6922c974e8a78378eacb01414183ce32bc8dbf2de78aabcc6ad8172547cb074" "235dc2dd925f492667232ead701c450d5c6fce978d5676e54ef9ca6dd37f6ceb" "38e64ea9b3a5e512ae9547063ee491c20bd717fe59d9c12219a0b1050b439cdd" "e64111716b1c8c82638796667c2c03466fde37e69cada5f6b640c16f1f4e97df" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" "c86f868347919095aa44d2a6129dd714cbcf8feaa88ba954f636295b14ceff8f" "8fed5e4b89cf69107d524c4b91b4a4c35bcf1b3563d5f306608f0c48f580fdf8" "83e584d74b0faea99a414a06dae12f11cd3176fdd4eba6674422539951bcfaa8" "90edd91338ebfdfcd52ecd4025f1c7f731aced4c9c49ed28cfbebb3a3654840b" "f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" "a507b9ca4a605d5256716da70961741b9ef9ec3246041a4eb776102e8df18418" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default))
 '(default-input-method "TeX")
 '(ebib-bibtex-dialect 'biblatex)
 '(eclim-eclipse-dirs
   '("/Applications/eclipse" "/usr/lib/eclipse" "/usr/local/lib/eclipse" "/usr/share/eclipse" "/Applications/Eclipse.app/Contents/Eclipse/" "/Applications/Eclipse Java.app/Contents/Eclipse/"))
 '(mac-option-modifier '(:ordinary meta :mouse alt))
 '(org-babel-load-languages '((scheme . t)))
 '(org-export-backends '(ascii html icalendar latex md org))
 '(org-src-tab-acts-natively t)
 '(org-support-shift-select t)
;;  '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(org-use-speed-commands t)
 '(package-selected-packages
   '(flycheck-gradle flymake-gradle gradle-mode company-emacs-eclim ac-emacs-eclim eclim prescient bind-key font-utils fontawesome flylisp flyspell-correct-popup flyspell-popup flycheck writegood-mode ediprolog ebib el-get el-init el-init-viewer el-mock el-patch el2org pdf-tools latex-unicode-math-mode htmlize auctex artbollocks-mode www-synonyms x-dict cyberpunk-theme langtool racket-mode flymake-racket wordsmith-mode tabbar dr-racket-like-unicode biblio org-doing org-dotemacs org-rtm paredit-menu paredit-everywhere org-ac magit-filenotify hc-zenburn-theme elscreen-separate-buffer-list dictionary color-theme calfw-gcal autopair ace-jump-mode ac-math helm-flyspell helm-wordnet helm-idris helm-dictionary)) ;; eldorado
 '(preview-auto-cache-preamble t)
 '(racket-program "racket")
 '(reftex-cite-format 'biblatex)
 '(safe-local-variable-values
   '((TeX-command-extra-options . "-shell-escape")
     (eval progn
	   (let
	       ((tt-root-directory
		 (when buffer-file-name
		   (locate-dominating-file buffer-file-name ".dir-locals.el")))
		(tt-project-find-file
		 (and
		  (boundp 'tt-project-find-file)
		  tt-project-find-file)))
	     (setq tags-file-name
		   (concat tt-root-directory "TAGS"))
	     (unless tt-project-find-file
	       (setq compile-command
		     (concat "make -C " tt-root-directory)))
	     (setq default-directory tt-root-directory)))))
 '(scheme-program-name "scheme")
 '(straight-use-package-by-default t)
 '(vc-follow-symlinks 't))

;; For 311, to make continuations RI.
;; Assumes k has some formal parameters
;; Leave mark at end of last match line in apply-k.

(fset 'make-k-ri
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217749 134217749 134217749 134217734 134217732 134217732 134217732 134217749 201326624 134217847 134217749 134217730 134217734 25 134217730 134217730 201326624 134217847 134217732 25 32 134217749 201326624 134217765 32 return 32 44 return 33 134217749 96 2 201326624 23 134217732 134217734 134217734 return 25 134217732 25 201326624 201326624 23 134217749 134217730 134217734 201326624 23 134217749 134217749 201326624 tab 134217730 134217734 134217748 2 2 2 134217730 134217730 134217734 25 134217749 201326624 tab 134217734 134217730 134217734 2 134217730 134217730 134217734] 0 "%d")) arg)))

;; I need to write a keyboard macro for going from let* to begin/set!

;; Indent regions C-x <tab> left or right. Mix with C-u `num` for multi
;; M-x set-input-method RETURN TeX RETURN write unicode chars
;; in Racket M-\ to change input mode.
;; C-u M-x shell -- get multiple shells!
;; point-to-register C-x r SPC
;; jump-to-register C-x r j 
;; M-x LaTeX-math-cal Ret <the letter>

;; Right now, this is busted in the agda-mode repository. 13/12/15
;; (when (eq system-type 'darwin)
;;   (load-file 
;;     (let ((coding-system-for-read 'utf-8))
;;       (shell-command-to-string "agda-mode locate"))))

(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))

(add-hook 'eval-expression-minibuffer-setup-hook 'my-minibuffer-setup)
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
(defun my-minibuffer-setup ()
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 5.0))))

;; Emacs desiderata
;; Setup emacs calendar to sync with google calendar
;; Set up paradox
;; Set up wordsmith-mode

;; Have width of nlinum buffer scale as font-size increases.
;; Set a higher default font size (point size).
;; Make it Windows 7/8/10 appropriate. --- see Google Keep
;; Automatically remove obsolete packages
;; Get code to color parens again for latex files. 
;; Setup package-pinned-packages, so as to draw from the correct package repo. 
;; Spacing with parens in various non-lisp modes that you use w/paredit mode.
;; Turn off C-z behavior that hides window
;; add diction file as a submodule, and suggest that if it's not found, that the directory in my .emacs module be symlinked to the correct location.
;; use David's .emacs as a sample, to set things up properly.
;; Add a separate file with my private information like git stuff etc, that folk can setup and add. 
;; Set things up so langtool will either be automatically downloaded or suggest that it be downloaded.
;; Perhaps instead set these things up as an emacs 24 package. 

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
