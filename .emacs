;;; Package --- Summary

;;; Commentary:

;;; This is Jason Hemann's .emacs setup, intended for OSX and some linux machines. It is currently in an
;;; unstable state, and the dependencies outside my .emacs are not listed. Several aspects of this rely on
;;; packages from homebrew, and a number of other downloaded files and hard-coded directories.

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
 
(straight-pull-recipe-repositories '(org-elpa melpa gnu-elpa-mirror el-get emacsmirror-mirror))

(add-hook 'find-file-hook (lambda () (ruler-mode 1)))

(defalias 'yes-or-no-p 'y-or-n-p)

(straight-use-package 'ac-math)
(straight-use-package 'academic-phrases)
(straight-use-package 'artbollocks-mode)
(straight-use-package 'ace-jump-mode)
(straight-use-package 'agda2-mode)
(straight-use-package 'anzu) ;; displays current match and total matches information
(straight-use-package 'apel)
(straight-use-package 'auctex-latexmk)
(straight-use-package 'auto-compile) ;; Automatically compile Emacs Lisp libraries
;; Auto complete is for most things strictly worse than company-mode
;; (straight-use-package 'auto-complete) ;; Dunno, but I had it before
;; (straight-use-package 'auto-complete-auctex)
(straight-use-package 'auto-package-update)
(straight-use-package 'autopair)
(straight-use-package 'bbdb) ;; Emacs address book
(straight-use-package 'biblio)
(straight-use-package 'bibtex-completion)
(straight-use-package 'bog)
(straight-use-package 'buffer-move) ;; used for rotating buffers. buf-move-left
(straight-use-package 'calfw)
(straight-use-package 'calfw-cal)
(straight-use-package 'calfw-gcal)
(straight-use-package 'calfw-ical)
(straight-use-package 'calfw-org)
;; (straight-use-package 'calfw-howm)
(straight-use-package 'cbm) ;; cycle by major mode
(straight-use-package 'cdlatex)
(straight-use-package 'cl-lib) ;; Properly prefixed CL functions and macros
(straight-use-package 'clang-format)
(straight-use-package 'clean-aindent-mode) ;; Emacs extension for simple indent and unindent
(straight-use-package 'color-theme-modern)
(straight-use-package 'comment-dwim-2) ;; A replacement for the emacs' built-in command comment-dwim
(straight-use-package 'company) ;; Complete anything ;-)
(straight-use-package 'company-coq)
(straight-use-package 'company-dict)
(straight-use-package 'company-lean)
(straight-use-package 'company-math)
(straight-use-package 'company-org-roam)
(straight-use-package 'company-auctex)
(straight-use-package 'company-bibtex)
(straight-use-package 'company-try-hard)
(straight-use-package 'company-fuzzy)
(straight-use-package 'coq-commenter)
(straight-use-package 'crux) ;; collection of emacs extensions
(straight-use-package 'cyberpunk-theme)
(straight-use-package 'dash) ;; A modern list library for Emacs
(straight-use-package 'dash-functional)
(straight-use-package 'dictionary)
(straight-use-package 'dired+)
(straight-use-package 'discover) ;; discover more of Emacs
(straight-use-package 'discover-my-major) ;; Discover key bindings and their meaning for the current Emacs major mode
(straight-use-package 'dr-racket-like-unicode)
(straight-use-package 'dtrt-indent) ;; A minor mode that guesses the indentation offset originally used for creating source code
(straight-use-package 'duplicate-thing) ;; duplicate current line
(straight-use-package 'easy-jekyll)
(straight-use-package 'eclim)
(straight-use-package 'ediprolog)
(straight-use-package 'eldoc) ;; the argument list of the function call you are currently writing
;; The following package requires some set-up to work with org-mode or w/e.
(straight-use-package 'elmacro) ;; https://github.com/Silex/elmacro#elmacro-processors
(straight-use-package 'elscreen-separate-buffer-list)
(straight-use-package 'epkg) ;; epkg-describe-package should show the dependencies
(straight-use-package 'exec-path-from-shell) ;; Make Emacs use the $PATH set up by the user's shell
(straight-use-package 'expand-region) ;; Increase selected region by semantic units
(straight-use-package 'f) ;; Modern API for working with files and directories in Emacs
(straight-use-package 'flim)
(straight-use-package '(flycheck-textlint :type git :host github :repo "kisaragi-hiu/flycheck-textlint" :fork nil))
;; Straight can't find the package
;; (straight-use-package 'flylisp-mode) ;; Add highlighting to mismatched parentheses, so you can see the mistake
(straight-use-package 'flymake-easy)
(straight-use-package 'flymake-racket)
(straight-use-package 'flymd)
(straight-use-package 'flyspell-lazy)
(straight-use-package '(flyspell-popup :type git :host github :repo "xuchunyang/flyspell-popup" :fork nil))
(straight-use-package 'fullframe) ;; Advice commands to execute fullscreen, restoring the window setup when exiting.
(straight-use-package 'gh-md)
(straight-use-package 'ghub)
(straight-use-package 'git-timemachine) ;; Walk through git revisions of a file
(straight-use-package 'gotham-theme)
(straight-use-package 'goto-chg) ;; Goto last change in current buffer
(straight-use-package 'gradle-mode)
(straight-use-package 'graphql)
(straight-use-package 'green-phosphor-theme)
(straight-use-package 'hc-zenburn-theme)

;; helm-browse-project: handles project files and buffers; defaults to current directory; works with helm-find-files; recommended with helm-ls-git, helm-ls-hg and helm-ls-svn for a better handling of version control files. Each time a project under version control is visited it is added to helm-browse-project-history and can be visted with helm-projects-history.
;; helm-dabbrev: enhanced dabbrev implementation with helm completion; does not use emacs code.
;; helm-imenu and helm-imenu-in-all-buffers: provide imenus for current or all buffers.
;; helm-etags-select: enhanced etags with helm-completion; usable everywhere with helm-find-files.

;; Grep: launch from any helm file commands; supports back-ends grep, ack-grep, git-grep, ag and custom implementation of pt.

;; helm-gid: Helm interface for gid from id-utils.

;; helm-show-kill-ring: A helm browser for kill ring.
;; helm-all-mark-rings: A helm browser for mark ring; retrieves last positions in buffers.
;; helm-filtered-bookmarks: enhanced browser for bookmarks.
;; helm-list-elisp-packages: enhanced browser for elisp package management.

(straight-use-package 'helm)
(straight-use-package 'helm-addressbook)
(straight-use-package 'helm-bibtex)
(straight-use-package 'helm-chrome)
(straight-use-package 'helm-company)
(straight-use-package 'helm-descbinds)
(straight-use-package 'helm-dictionary)
(straight-use-package 'helm-dirset)
(straight-use-package 'helm-emms)
(straight-use-package 'helm-eww)
(straight-use-package 'helm-firefox)
(straight-use-package 'helm-fuzzy)
(straight-use-package 'helm-google)
(straight-use-package 'helm-idris)
(straight-use-package 'helm-lean)
(straight-use-package 'helm-ls-git)
(straight-use-package 'helm-mu)
(straight-use-package 'helm-shell)
(straight-use-package 'helm-system-packages)
(straight-use-package 'helm-tramp)
(straight-use-package 'helm-wordnet)
(straight-use-package 'helpful)
(straight-use-package 'ht)
;; https://github.com/coldnew/coldnew-emacs#hydra
(straight-use-package 'hydra) ;; tie related commands into a family of short bindings w/a common prefix.
(straight-use-package 'ibuffer-vc) ;; Let Emacs' ibuffer-mode group files by git project etc., and show file state
;; (straight-use-package 'ido-vertical-mode) ;; makes ido-mode display vertically
(straight-use-package 'iedit) ;; Emacs minor mode and allows you to edit one occurrence of some text in a buffer
(straight-use-package 'j-mode)
(straight-use-package 'jeison)
(straight-use-package 'jump) ;; build functions which contextually jump between files
(straight-use-package 'langtool)
(straight-use-package 'lean-mode)
(straight-use-package 'magit-filenotify)
(straight-use-package 'magit-gerrit) ;; gerrit mode for emacs
(straight-use-package 'magit-popup)
(straight-use-package 'markdown-mode+)
(straight-use-package 'markdown-preview-mode)
(straight-use-package 'mc-extras)
(straight-use-package 'meghanada)
(straight-use-package 'midnight)
(straight-use-package 'multiple-cursors)
(straight-use-package 'mustache)
(straight-use-package 'neotree) ;; A emacs tree plugin like NerdTree for Vim.
;; (straight-use-package 'nlinum) %% with emacs 26 built-in line numbering, not needed
(straight-use-package 'ob-browser)
(straight-use-package 'org-ac)
(straight-use-package 'org-bullets)
(straight-use-package 'org-dropbox)
(straight-use-package 'org-doing)
(straight-use-package 'org-dotemacs)
(straight-use-package 'org-inline-pdf)
(straight-use-package 'org-jekyll)
(straight-use-package 'org-journal)
(straight-use-package 'org-ql)
(straight-use-package 'org-ref)
(straight-use-package 'org-roam)
(straight-use-package 'org-roam-bibtex)
;;(straight-use-package 'org-roam-server)
(straight-use-package 'org-rtm)
(straight-use-package 'org-sidebar)
(straight-use-package 'org-super-agenda)
(straight-use-package 'org-trello)
(straight-use-package 'org2web)
(straight-use-package 'ox-jekyll-md)
(straight-use-package 'ox-pandoc)
(straight-use-package 'paradox)
(straight-use-package 'paredit-everywhere)
(straight-use-package 'paredit-menu)
(straight-use-package 'parent-mode)
(straight-use-package 'pdf-tools)
(straight-use-package 'popup) ;; Visual Popup Interface Library for Emacs
(straight-use-package 'powerthesaurus)
(straight-use-package 'projectile) ;; Project Interaction Library for Emacs http://projectile.readthedocs.io
(straight-use-package 'proof-general)
(straight-use-package 'racket-mode)
(straight-use-package 'reazon)
(straight-use-package 'refine)
(straight-use-package 's) ;; The long lost Emacs string manipulation library.
(straight-use-package 'savehist)
(straight-use-package 'scheme-complete)
(straight-use-package 'semi)
(straight-use-package 'sh-script) ;; The major mode for editing Unix and GNU/Linux shell script code
(straight-use-package '(simple-httpd :type git :host github :repo "skeeto/emacs-web-server" :local-repo "simple-httpd"))
(straight-use-package 'smartscan) ;; Quickly jumps between other symbols found at point in Emacs
(straight-use-package 'sml-mode)
(straight-use-package 'sml-modeline)
(straight-use-package 'smog)
(straight-use-package 'solarized-emacs)
(straight-use-package 'sourcemap) ;;  Sourmap parser in Emacs Lisp
(straight-use-package 'sx) ;; Stackoverflow mode ;-)
(straight-use-package 'svg-tag-mode)
(straight-use-package 'tabbar)
(straight-use-package 'treepy)
(straight-use-package 'ts) ;; A bunch of nice utilities for time and date parsing, better than the built-ins
(straight-use-package 'undo-tree) ;; Treat undo history as a tree
(straight-use-package 'use-package)
(straight-use-package 'visual-regexp) ;; A regexp/replace command for Emacs with interactive visual feedback
(straight-use-package 'visual-regexp-steroids) ;; Extends visual-regexp to support other regexp engines
(straight-use-package 'volatile-highlights) ;; Minor mode for visual feedback on some operations.
(straight-use-package 'w3m)
(straight-use-package 'wanderlust)
(straight-use-package 'wordnut)
(straight-use-package 'wordsmith-mode)
(straight-use-package 'wrap-region) ;; Emacs minor mode to wrap region with tag or punctuations
(straight-use-package 'writegood-mode)
(straight-use-package 'ws-butler) ;; Unobtrusively trim extraneous white-space *ONLY* in lines edited.
(straight-use-package 'yafolding) ;; Yet another folding extension for Emacs
(straight-use-package 'yaml-mode) ;; The emacs major mode for editing files in the YAML data serialization format.
(straight-use-package 'zones)
(straight-use-package 'zygospore)

(use-package org-roam
      :ensure t
      :hook (after-init . org-roam-mode)
      :custom
      (org-roam-directory (file-truename "~/.org/"))
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))



(require 'ediprolog)
(global-set-key [f10] 'ediprolog-dwim)

(add-hook 'after-init-hook 'org-roam-mode)

(global-set-key (kbd "M-;") 'comment-dwim-2)
(global-set-key (kbd "C-z") #'company-try-hard)

;; A minor mode for Emacs that deals with parens pairs and tries to be smart about it.
;; I think paredit probably does everything I need
;; (straight-use-package  'smartparens)

;; (straight-use-package '(eldoro "pjones/eldoro")

;; Wanderlust doesn't seem to work w/Google 2FA.
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

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
      wl-fcc-force-as-read t

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

(if (file-exists-p "/Users/jhemann/Documents/acl2/scripts-master/.lisp.el")
    (load-file "/Users/jhemann/Documents/acl2/scripts-master/.lisp.el"))

;; https://cliplab.org/~clip/Software/Ciao/ciao-1.15.0.html/CiaoMode.html#Installation%20of%20the%20Ciao%20emacs%20interface
;; https://github.com/ciao-lang/ciao_emacs
;; (if (file-exists-p "/usr/local/lib/ciao/ciao-mode-init.el")
;;     (load-file "/usr/local/lib/ciao/ciao-mode-init.el"))

;; I should want maven, I think, tbqh
(add-hook 'java-mode-hook '(lambda() (gradle-mode 1)))
(add-hook 'java-mode-hook 'eclim-mode)

;; (load "~/Documents/eliemacs/eliemacs")

;; Probably package.el related doohickuses
;; (package-initialize)
;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (unless package-archive-contents
;;    (package-refresh-contents))

(require 'racket-xp) ;; Don't know what this is but I think it's not a package
(add-hook 'racket-mode-hook #'racket-xp-mode)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key [C-M-tab] 'clang-format-region)

(let ((gnu-ls-path (executable-find "gls")))
  (when gnu-ls-path
    (setq insert-directory-program gnu-ls-path)))

(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

;; (ac-mode 1)
;; (require 'auto-complete-config)

(helm-mode 1)


;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(require 'paredit)
(require 'calfw)
(require 'calfw-cal)
(require 'helm-config)

(global-set-key (kbd "C-x b") 'helm-buffers-list) ;; helm-buffers-list: provides enhanced buffers listing.
(global-set-key (kbd "C-h a") 'helm-apropos) ;; enhanced apropos for functions and variables that C-h commands provide.
(global-set-key (kbd "C-c h o") 'helm-occur) ;; helm-occur: enhanced occur for one or more buffers; launch from helm-buffers-list or current-buffer.
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files) ;; helm-find-files: one command that handles all the files related commands
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(add-hook 'racket-mode-hook (lambda () (define-key racket-mode-map (kbd "C-c r") 'racket-run)))

;; These don't work.
;; (add-hook 'auto-package-update-minor-mode-hook 'package-menu-mark-obsolete-for-deletion)
;; (add-hook 'auto-package-update-minor-mode-hook 'package-menu-execute)
;; (auto-package-update-maybe)

;; JBH
;; (ac-config-default)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

(global-set-key (kbd "C-S-s") (lambda () (interactive (insert "§"))))
(global-set-key (kbd "C-c (") (lambda () (interactive) (insert "ಠ_ಠ")))
(global-set-key (kbd "C-c )") (lambda () (interactive) (insert "¯\\_(ツ)_/¯")))
(global-set-key (kbd "C-c C-x (") (lambda () (interactive) (insert "ᕕ( ᐛ )ᕗ")))
(global-set-key (kbd "C-c C-x )") (lambda () (interactive) (insert "(－‸ლ)")))
(global-set-key (kbd "C-c C-x x") (lambda () (interactive) (insert "!(•̀ᴗ•́)و ̑̑")))


;; ebib mode for latex
(global-set-key "\C-ce" 'ebib)

(if (eq system-type 'darwin)
    (add-hook 'text-mode-hook 'wordsmith-mode)) ;; Because this depends on OSX tooling specifically

;; (add-hook 'text-mode-hook 'writegood-mode)
(add-hook 'text-mode-hook 'artbollocks-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'tex-mode-hook (function (lambda () (setq ispell-parser 'tex))))
(add-hook 'tex-mode-hook 'auxtex-mode)

(add-hook 'tex-mode-hook (lambda () (define-key tex-mode-map (kbd "C-c C-k") 'compile)))
(add-hook 'tex-mode-hook (lambda () (define-key tex-mode-map (kbd "C-c |") 'align-current)))

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; (add-hook 'scheme-mode-hook                        'flylisp-mode)
;; (add-hook 'inferior-scheme-mode-hook               'flylisp-mode)
(add-hook 'scheme-mode-hook                        'multiple-cursors-mode)
(add-hook 'inferior-scheme-mode-hook               'multiple-cursors-mode)


(global-flycheck-mode)
(global-company-mode)
(global-anzu-mode +1) ;; global search count mode

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
  "Custom function to spell check next highlighted word."
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)

;; Only in Emacs mac-port
(mac-auto-operator-composition-mode t)

(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (if (eq system-type 'linux)
      (setq-default ispell-program-name "/usr/bin/aspell")
    (setq-default ispell-program-name (executable-find "asell")))) ;; What should we do about Windows?

(setq-default ispell-list-command "list")

(global-set-key "\C-x4w" 'langtool-check)
(global-set-key "\C-x4W" 'langtool-check-done)
(global-set-key "\C-x4l" 'langtool-switch-default-language)
(global-set-key "\C-x44" 'langtool-show-message-at-point)
(global-set-key "\C-x4c" 'langtool-correct-buffer)

(defun langtool-autoshow-detail-popup (overlays)
  ""
  (when (require 'popup nil t)
    ;; Do not interrupt current popup
    (unless (or popup-instances
                ;; suppress popup after type `C-g` .
                (memq last-command '(keyboard-quit)))
      (let ((msg (langtool-details-error-message overlays)))
        (popup-tip msg)))))

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

(add-hook 'latex-mode-hook 'turn-on-cdlatex)  ; with Emacs latex mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

(add-hook 'TeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'TeX-mode-hook 'turn-on-cdlatex)  ; with AUCTeX LaTeX mode
(add-hook 'TeX-mode-hook 'visual-line-mode)
(add-hook 'TeX-mode-hook 'flyspell-preprocess-buffer) ;; somehow void
(add-hook 'TeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'TeX-mode-hook 'TeX-PDF-mode)
(add-hook 'TeX-mode-hook 'TeX-fold-mode)
(add-hook 'TeX-mode-hook (lambda () (TeX-fold-mode 1)))
(add-hook 'TeX-mode-hook #'TeX-fold-mode) ;; Automatically activate TeX-fold-mode.
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

(global-set-key (kbd "C-c C-d") #'helpful-at-point)

(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)

;; Only sometimes works!
;; must be after font locking is set up for the buffer on!
;; (add-hook 'find-file-hook 'TeX-fold-buffer t)


;; (setq lisp-indent-function 'scheme-smart-indent-function)
;; (autoload 'scheme-smart-complete "scheme-complete" nil t)

;; (eval-after-load 'scheme
;;   '(define-key scheme-mode-map "\t" 'scheme-complete-or-indent))

;; (autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
;; (add-hook 'scheme-mode-hook
;;   (lambda ()
;;     (make-local-variable 'eldoc-documentation-function)
;;     (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
;;     (eldoc-mode)))

;; (define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct)
;; You can also enable flyspell-popup-auto-correct-mode to popup that
;; Popup Menu automatically with a delay (default 1.6 seconds):
;; (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)

(add-to-list 'auto-mode-alist '("\\.v$" . coq-mode))
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
(add-to-list 'auto-mode-alist '("\\.agda\\'" . agda2-mode))
(add-to-list 'auto-mode-alist '("\\.lagda.md\\'" . agda2-mode))

(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

(add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
(add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)

;; Paredit-everywhere-mode is a liar.
;; It turns on *some* of the paredit keybindings but not all, and it doesn't let you choose
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)
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
;; (add-hook 'ciao-mode-hook                          #'enable-paredit-mode) ;; not til fix paren space issue.
;; tex-mode has paredit-mode issue too.
(add-hook 'idris-prover-script-mode-hook           #'enable-paredit-mode)

(global-set-key (kbd "{") 'paredit-open-curly)
;; (add-hook 'racket-mode-hook      #'flylisp-mode)
;; (add-hook 'racket-repl-mode-hook #'flylisp-mode)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; (setq window-themes-list '(wheatgrass manoj-dark cyberpunk tango-dark deeper-blue green-phosphor gotham solarized))

(mapc (lambda (pr) (put (car pr) 'racket-indent-function (cdr pr)))
      '((conde . 0)
        (fresh . 1)
        (run . 1)
        (run* . 1)
        (run . 2)
	(letrec . 0)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-auto-save t t)
 '(TeX-auto-untabify t)
 '(TeX-engine 'xetex)
 '(TeX-parse-self t t)
 '(ac-modes
   '(emacs-lisp-mode lisp-mode lisp-interaction-mode slime-repl-mode c-mode cc-mode c++-mode go-mode java-mode malabar-mode clojure-mode clojurescript-mode scala-mode scheme-mode ocaml-mode tuareg-mode coq-mode haskell-mode perl-mode cperl-mode python-mode ruby-mode lua-mode tcl-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode less-css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode web-mode ts-mode sclang-mode verilog-mode qml-mode racket-mode Racket-mode racket-repl-mode idris-mode idris-repl-mode )) ;; ciao-mode
 '(ad-redefinition-action 'accept)
 '(auto-save-interval 75)
 '(auto-save-timeout 10)
 '(bib-cite-use-reftex-view-crossref t t)
 '(bibtex-maintain-sorted-entries 'plain)
 '(blink-cursor-mode nil)
 '(bookmark-save-flag 0)
 '(calendar-mark-diary-entries-flag t)
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(custom-safe-themes t)
 '(debug-on-quit t)
 '(default-input-method "TeX")
 '(dired-listing-switches "-al --group-directories-first --time-style=long-iso")
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(ebib-bibtex-dialect 'biblatex)
 '(eclim-eclipse-dirs
   '("/Applications/eclipse" "/usr/lib/eclipse" "/usr/local/lib/eclipse" "/usr/share/eclipse" "/Applications/Eclipse.app/Contents/Eclipse/" "/Applications/Eclipse Java.app/Contents/Eclipse/"))
 '(flyspell-issue-welcome-flag nil)
 '(fringe-mode 2 nil (fringe))
 '(global-display-line-numbers-mode t)
 '(global-flycheck-mode t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-scroll-amount 8)
 '(helm-split-window-inside-p t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-highlight-face 'highlight)
 '(ispell-highlight-p t)
 '(ispell-program-name "aspell")
 '(langtool-autoshow-message-function 'langtool-autoshow-detail-popup)
 '(langtool-default-language "en-US")
 '(langtool-language-tool-jar "~/LanguageTool-3.4/languagetool-commandline.jar")
 '(langtool-mother-tongue "en")
 '(load-home-init-file t t)
 '(ls-lisp-dirs-first t)
 '(make-backup-files nil)
 '(ns-alternate-modifier '(:ordinary meta :mouse alt))
 '(org-agenda-files '("tasks.org"))
 '(org-agenda-include-diary t)
 '(org-agenda-start-with-log-mode 'only)
 '(org-babel-load-languages '((scheme . t)))
 '(org-directory "~/.org")
 '(org-export-backends '(ascii html icalendar latex md org))
 '(org-log-done 'time)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail org-tempo ol-w3m org-mac-iCal org-mac-link ol-wl))
 '(org-roam-directory "~/org-roam/" nil nil "Customized with use-package org-roam")
 '(org-src-tab-acts-natively t)
 '(org-support-shift-select t)
 '(org-time-stamp-custom-formats '("<%m/%d/%y %a>" . "<%a %_B %_d, %H:%M>"))
 '(org-trello-current-prefix-keybinding "C-c o")
 '(org-use-speed-commands t)
 '(package-selected-packages
   '(flycheck-gradle flymake-gradle gradle-mode company-emacs-eclim ac-emacs-eclim eclim prescient bind-key font-utils fontawesome flyspell-correct-popup flyspell-popup flycheck writegood-mode ediprolog ebib el-get el-init el-init-viewer el-mock el-patch el2org pdf-tools latex-unicode-math-mode htmlize auctex artbollocks-mode www-synonyms x-dict cyberpunk-theme langtool racket-mode flymake-racket wordsmith-mode tabbar dr-racket-like-unicode biblio org-doing org-dotemacs org-rtm paredit-menu paredit-everywhere org-ac magit-filenotify hc-zenburn-theme elscreen-separate-buffer-list dictionary color-theme calfw-gcal autopair ace-jump-mode ac-math helm-flyspell helm-wordnet helm-idris helm-dictionary))
 '(preview-auto-cache-preamble t)
 '(racket-program "racket")
 '(reftex-cite-format 'biblatex)
 '(reftex-extra-bindings t t)
 '(reftex-plug-into-AUCTeX t t)
 '(ring-bell-function 'ignore)
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
 '(savehist-mode t)
 '(scheme-program-name "scheme")
 '(scroll-bar-mode 'right)
 '(select-enable-clipboard t)
 '(sentence-end-double-space nil)
 '(show-paren-delay 0)
 '(show-paren-mode 1)
 '(sort-fold-case t t)
 '(straight-use-package-by-default t)
 '(tab-always-indent 'complete)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(vc-follow-symlinks 't)
 '(vc-make-backup-files t)
 '(version-control t)
 '(visible-bell t))

(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
	    (id (one-or-more (not (any " "))))
	    (message) line-end))
  :modes (text-mode markdown-mode gfm-mode))

(add-to-list 'flycheck-checkers 'proselint)

(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
(global-set-key (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word)

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word."
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)

;; Only in Emacs mac-port
(mac-auto-operator-composition-mode t)

(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (if (eq system-type 'linux)
      (setq-default ispell-program-name "/usr/bin/aspell")
    (setq-default ispell-program-name ""))) ;; What should we do about Windows?

(setq-default major-mode 'text-mode)
(setq-default ispell-list-command "list")

(global-set-key "\C-x4w" 'langtool-check)
(global-set-key "\C-x4W" 'langtool-check-done)
(global-set-key "\C-x4l" 'langtool-switch-default-language)
(global-set-key "\C-x44" 'langtool-show-message-at-point)
(global-set-key "\C-x4c" 'langtool-correct-buffer)

(defun langtool-autoshow-detail-popup (overlays)
  ""
  (when (require 'popup nil t)
    ;; Do not interrupt current popup
    (unless (or popup-instances
                ;; suppress popup after type `C-g` .
                (memq last-command '(keyboard-quit)))
      (let ((msg (langtool-details-error-message overlays)))
        (popup-tip msg)))))

(setq-default TeX-master nil)
;; (setq-default TeX-master "master") ; set a master for in the future.
;;'(reftex-label-alist '(AMSTeX)) ;; For if I'm getting parens around my references

(if window-system
  (load-theme (nth (cl-random (length (custom-available-themes))) (custom-available-themes)) t) ;; To have it always remember this is safe
  (load-theme 'wombat t))

 ;; Add opam emacs directory to the load-path
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;; Pre-load Andromeda
(require 'andromeda-autoloads)

;; For 311, to make continuations RI.
;; Assumes k has some formal parameters
;; Leave mark at end of last match line in apply-k.

;; (fset 'make-k-ri
;;       (lambda (&optional arg) "Keyboard macro." (interactive "p")
;;         (kmacro-exec-ring-item (quote ([134217749 134217749 134217749 134217734 134217732 134217732 134217732 134217749 201326624 134217847 134217749 134217730 134217734 25 134217730 134217730 201326624 134217847 134217732 25 32 134217749 201326624 134217765 32 return 32 44 return 33 134217749 96 2 201326624 23 134217732 134217734 134217734 return 25 134217732 25 201326624 201326624 23 134217749 134217730 134217734 201326624 23 134217749 134217749 201326624 tab 134217730 134217734 134217748 2 2 2 134217730 134217730 134217734 25 134217749 201326624 tab 134217734 134217730 134217734 2 134217730 134217730 134217734] 0 "%d")) arg)))

;; I need to write a keyboard macro for going from let* to begin/set!

;; Indent regions C-x <tab> left or right. Mix with C-u `num` for multi
;; M-x set-input-method RETURN TeX RETURN write unicode chars
;; in Racket M-\ to change input mode.
;; C-u M-x shell -- get multiple shells!
;; point-to-register C-x r SPC
;; jump-to-register C-x r j
;; M-x LaTeX-math-cal Ret <the letter>
;; M-x smog-check-region
;; C-h a does apropos
;; M-v custom-enabled-themes tells you what themes are in force.
;; In org-mode C-' on a table.el table lets you edit it nicely, like that.
;; C-h r for the manual, then g (for "goto node").
;; M-x table-capture https://www.gnu.org/software/emacs/manual/html_node/emacs/Table-Conversion.html
;; M-x list-processes
;; 

;; Right now, this is busted in the agda-mode repository. 13/12/15
;; (when (eq system-type 'darwin)
;;   (load-file
;;     (let ((coding-system-for-read 'utf-8))
;;       (shell-command-to-string "agda-mode locate"))))

;; This was some setup that I did to the minibuffer; not sure it was wise
;; (add-hook 'eval-expression-minibuffer-setup-hook 'my-minibuffer-setup)
;; (add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
;; (defun my-minibuffer-setup ()
;;   (set (make-local-variable 'face-remapping-alist)
;;        '((default :height 5.0))))

;; Emacs desiderata
;; Setup emacs calendar to sync with google calendar

;; Set a higher default font size (point size).
;; Make it Windows 7/8/10 appropriate. --- see Google Keep

;; Get code to color parens again for latex files.
;; Setup package-pinned-packages, so as to draw from the correct package repo.
;; Set up paradox -- if that's still a good idea. Cf straight-use-package, etc. etc.
;; Automatically remove obsolete packages

;; (unless (package-installed-p 'use-package)
;;   (progn
;;     (unless package-archive-contents
;;       (package-refresh-contents))
;;     (package-install 'use-package)))
;; On another yak-shave, use the following:
;; https://www.reddit.com/r/emacs/comments/47aq53/best_way_to_set_up_package_dependencies_in_initel/
;; to simplify the .emacs

;; Spacing with parens in various non-lisp modes that you use w/paredit mode.

;; Turn off C-z behavior that hides window

;; use David's .emacs as a sample, to set things up properly.
;; Add a separate file with my private information like git stuff etc, that folk can setup and add.
;; Set things up so langtool will either be automatically downloaded or suggest that it be downloaded.
;; Perhaps instead set these things up as an emacs 24 package.

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diary ((t (:foreground "dark red")))))
(require 'company-try-hard)
(require 'comment-dwim-2)

(define-key org-mode-map (kbd "M-;") 'org-comment-dwim-2)
(define-key company-active-map (kbd "C-z") #'company-try-hard)
(setq org-roam-graph-executable "/usr/local/bin/dot")


;; Org-ref
;; Set up bibliography
;; (setq org-ref-default-bibliography '("~/iCloudDrive/bibliography/myBibliography.bib"))
;; (setq bibtex-completion-bibliography "~/iCloudDrive/bibliography/myBibliography.bib")
(global-set-key (kbd "<f6>") #'org-ref-helm-insert-cite-link)

;; Org-roam-bibtex
(require `org-roam-bibtex)
(add-hook 'after-init-hook #'org-roam-bibtex-mode)
(define-key org-roam-bibtex-mode-map (kbd "C-c n a") #'orb-note-actions)
;; This one does not seem to be needed unless You use reftex.
;; (setq reftex-default-bibliography '("~/iCloudDrive/bibliography/myBibliography.bib"))

(pdf-tools-install) ;; if this slows things down try (pdf-loader-install)

(require 'org-protocol)
(require 'org-roam-protocol)

;; (require 'org-roam-server)
;; (setq org-roam-server-host "127.0.0.1"
;;       org-roam-server-port 8080
;;       org-roam-server-export-inline-images t
;;       org-roam-server-authenticate nil
;;       org-roam-server-network-poll t
;;       org-roam-server-network-arrows nil
;;       org-roam-server-network-label-truncate t
;;       org-roam-server-network-label-truncate-length 60
;;       org-roam-server-network-label-wrap-length 20)

;; default to mononoki
(set-face-attribute 'default nil
                    :family "mononoki"
                    :height 120
                    :weight 'normal
                    :width  'normal)

(provide '.emacs)
;;; .emacs ends here
