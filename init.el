
;;; Commentary:

;;; This is Jason Hemann's .emacs setup, intended for OSX and some linux machines. It is currently in an
;;; unstable state, and the dependencies outside my .emacs are not listed. Several aspects of this rely on
;;; packages from homebrew, and a number of other downloaded files and hard-coded directories.

;;; Code:

;; Need to be set before we load straight.el, to correct a flycheck incompatibility.
(setq straight-fix-flycheck t)

;; Configuration for how straight.el should load.
(setq load-prefer-newer t)

;; The straight.el bootstrap code.
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

(require 'straight-x) ;; Adds the straight-x commands to clean up straight install

(defvar curr-f-list features)

;; This is probably not good, b/c what if we are not online
(straight-pull-recipe-repositories '(melpa org-elpa gnu-elpa-mirror el-get emacsmirror-mirror))

(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; JBH 4/6/22 disabling because it was seeming slow
;; (add-hook 'find-file-hook (lambda () (ruler-mode 1)))

;; From my package.el days
;;  htmlize seems unnecessary. Org and markdown are all I would use it for and those are already supported elsewhere.
;;  x-dict emacs attic, so no need.
;;  dictionary is also a emacs 21 era thing, so no need.

(straight-use-package 'use-package)

;; So that I can publicly VC my config w/o leaking secret keys &c.
(straight-use-package '(use-package-secret :host github :repo "emacswatcher/use-package-secret"))

(use-package org
  :straight t
  :config ;; Org mode defaults
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture))

;; Do I want this hook under agda2-mode or paredit-mode
(use-package agda2-mode
  :straight '(:includes (eri annotation))
  :hook (agda2-mode . enable-paredit-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.agda\\'" . agda2-mode))
  (add-to-list 'auto-mode-alist '("\\.lagda.md\\'" . agda2-mode)))

(straight-use-package '(simple-httpd :includes web-server :files ("*.el")))
(straight-use-package 'impatient-mode) ;; replacement for flymd
(straight-use-package '(faceup :type built-in)) ;; b/c this is newer than the one from straight, lexical binding
(straight-use-package '(let-alist :type built-in))
(straight-use-package '(which-key :custom (which-key-mode)))
(straight-use-package '(helm :files ("*.el" "emacs-helm.sh" (:exclude "helm-lib.el" "helm-source.el" "helm-multi-match.el" "helm-core.el" "helm-core-pkg.el") "helm-pkg.el")))

(straight-use-package 'unicode-fonts)
(straight-use-package 'font-utils) ;; Apparently nice for working w/fonts in emacs.

;; A preferred synonyms package, but check use-cases.
(straight-use-package 'powerthesaurus)

;; In order to search for synonyms.
(use-package www-synonyms
  :straight t
  :secret api-key
  :config
  (setq www-synonyms-key api-key))

(use-package org-roam
  :demand t
  :config (org-roam-db-autosync-mode)
  :straight t
  :custom
  (setq org-roam-graph-executable "/usr/local/bin/dot"
		org-roam-directory (file-truename "~/.org/"))
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

(add-to-list 'display-buffer-alist
	     '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

(use-package org-roam-ui
    :straight (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out"))
    :after org-roam
    :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(straight-use-package 'academic-phrases)

(use-package artbollocks-mode
  :straight t
  :hook (text-mode . artbollocks-mode))

(straight-use-package 'ace-jump-mode)
;; displays current match and total matches information
;; global search count mode
(use-package anzu
  :straight t
  :config
  (global-anzu-mode +1))
(straight-use-package 'apel) ;; portable emacs extensions; unclear how relevant
(straight-use-package 'auctex) ;; Not sure if I need w/dependency but trying just in case
(straight-use-package 'auctex-latexmk)
(straight-use-package 'auto-compile) ;; Automatically compile Emacs Lisp libraries

;; Auto complete is for most things strictly worse than company-mode
;; (straight-use-package 'auto-complete)
;; (require 'auto-complete-config)
;; (ac-mode 1)
;; (ac-config-default)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (straight-use-package 'ac-math) commented in favor of company-math
;; (straight-use-package 'auto-complete-auctex)

;; (straight-use-package 'auto-package-update) ;; straight has this feature already

(straight-use-package 'autopair)
(straight-use-package 'bbdb) ;; Emacs address book
(straight-use-package 'biblio)
(straight-use-package 'bibtex-completion)

;; Org-ref
;; Set up bibliography
(setq bibtex-completion-bibliography "~/old-microKanrenbib.bib")

;; ~bind-key~ adds a keybinding
;; ~bind-key*~ overrides minor-mode
;; ~unbind-key~ removes
;; M-x describe-key-bindings gives the full list
(use-package bind-key
  :straight t
  :bind ("C-h B" . describe-personal-keybindings))
(straight-use-package 'bog) ;; for taking research notes w/org. Cf the more general org-ref that does both notes and writing.
(straight-use-package 'buffer-move) ;; used for rotating buffers. buf-move-left

(straight-use-package 'calfw)
(straight-use-package 'calfw-cal)
(straight-use-package 'calfw-gcal)
(straight-use-package 'calfw-ical)
(straight-use-package 'calfw-org)
;; No need for howm-mode; org-mode + roam for me
;; (straight-use-package 'calfw-howm)
(straight-use-package 'cbm) ;; cycle by major mode

(use-package cdlatex
  :straight t
  :hook
  (latex-mode . turn-on-cdlatex)  ; with Emacs latex mode
  (TeX-mode . turn-on-cdlatex))  ; with AUCTeX LaTeX mode

(straight-use-package 'cl-lib) ;; Properly prefixed CL functions and macros

(use-package clang-format
  :straight t
  :config (global-set-key [C-M-tab] 'clang-format-region))

(use-package comment-dwim-2 ;; A replacement for the emacs' built-in command comment-dwim
  :straight t
  :bind (:map org-mode-map
			  ("M-;" . 'org-comment-dwim-2))
  :config
  (global-set-key (kbd "M-;") 'comment-dwim-2))

(use-package company ;; Complete anything ;-)
  :straight t
  :config (global-company-mode))

(straight-use-package 'company-coq)
(straight-use-package 'company-dict)

(straight-use-package 'lean-mode)
(use-package company-lean
  :straight t
  :config (global-set-key (kbd "S-SPC") #'company-complete)) ;; Trigger completion on Shift-Space

(straight-use-package 'company-math)
(straight-use-package 'company-org-roam)
(straight-use-package 'company-auctex)
(straight-use-package 'company-bibtex)
(straight-use-package 'company-try-hard)
(straight-use-package 'company-fuzzy)
(straight-use-package 'consult) ;; the counsel equivalent for selectrum
(straight-use-package 'coq-commenter)
(straight-use-package 'crux) ;; collection of emacs extensions

(straight-use-package 'dash) ;; A modern list library for Emacs
(straight-use-package 'dash-functional)

(straight-use-package '(dired-hacks-utils :host github :repo "Fuco1/dired-hacks" :fork (:host github :repo "jasonhemann/dired-hacks")))

(use-package dired-collapse
  :straight '(:host github :repo "Fuco1/dired-hacks"
		    :fork (:host github :repo "jasonhemann/dired-hacks")) ;; This is now correct
  :hook (dired-mode . dired-collapse-mode))

(straight-use-package 'dired+)
;; (straight-use-package 'discover) ;; discover more of Emacs. Sadly, moribund

(straight-use-package 'discover-my-major) ;; Discover key bindings and their meaning for the current Emacs major mode

(use-package dr-racket-like-unicode
  :straight t
  :hook
  (racket-mode . racket-unicode-input-method-enable)
  (racket-repl . racket-unicode-input-method-enable))

(straight-use-package 'clean-aindent-mode) ;; Emacs extension for simple indent and unindent
(straight-use-package 'dtrt-indent) ;; A minor mode that guesses the indentation offset originally used for creating source code

(straight-use-package 'duplicate-thing) ;; duplicate current line
(straight-use-package 'easy-jekyll)

(use-package ebib
  :straight t
  :config
  (setq ebib-bibtex-dialect 'biblatex)
  (global-set-key "\C-ce" 'ebib)) ;; ebib mode, for latex

(use-package ediprolog
  :straight t
  :config
  (setq ediprolog-program "scryer-prolog")
  (global-set-key [f10] 'ediprolog-dwim))

(straight-use-package 'el2org)

;; these provide ways to cleanly split an init file up
(straight-use-package 'el-init)
(straight-use-package 'el-init-viewer)

;;  el-mock Maybe I need it, but I don't think so.
(straight-use-package 'el-patch)
(straight-use-package 'eldoc) ;; the argument list of the function call you are currently writing
;; The following package requires some set-up to work with org-mode or w/e.
(straight-use-package 'elmacro) ;; https://github.com/Silex/elmacro#elmacro-processors

;; s-u-p elscreen ?
(straight-use-package 'elscreen-separate-buffer-list)

(when (executable-find "mpv") ;; empv relies on the mpv executable.
  (straight-use-package '(empv :type git :host github :repo "isamert/empv.el")))

(straight-use-package 'epkg) ;; epkg-describe-package should show the dependencies
(straight-use-package 'exec-path-from-shell) ;; Make Emacs use the $PATH set up by the user's shell
(straight-use-package 'expand-region) ;; Increase selected region by semantic units
(straight-use-package 'f) ;; Modern API for working with files and directories in Emacs
;; fontawesome is abandonware

(use-package flycheck
  :straight t
  :config (setq global-flycheck-mode t))

(straight-use-package '(flycheck-textlint :type git :host github :repo "kisaragi-hiu/flycheck-textlint" :fork nil))

(use-package gradle-mode ;; I should want maven, I think, tbqh
  :straight t
  :hook (java-mode . gradle-mode))

(straight-use-package 'flycheck-gradle)

;; Another java mode
(straight-use-package 'meghanada)

;; All eclim abandoned and better use java-lsp.
;; no company-emacs-eclim ac-emacs-eclim either

;; Take a look at what he has here
;; (load "~/Documents/eliemacs/eliemacs")

;; No need to use these, as flycheck is better
;; (straight-use-package 'flylisp) ;; Add highlighting to mismatched parentheses, so you can see the mistake
;; (straight-use-package 'flymake-easy)
;; (straight-use-package 'flymake-gradle)
;; (straight-use-package 'flymake-racket)
;; (straight-use-package 'flymd) No longer works w/FF >= 68

(straight-use-package 'flyspell-lazy)
(straight-use-package '(flyspell-popup :type git :host github :repo "xuchunyang/flyspell-popup"))
(straight-use-package 'fullframe) ;; Advice commands to execute fullscreen, restoring the window setup when exiting.
(straight-use-package 'gh-md)

(straight-use-package 'ghub)
(straight-use-package 'ghub+)

(straight-use-package 'git-timemachine) ;; Walk through git revisions of a file

(straight-use-package 'goto-chg) ;; Goto last change in current buffer
(straight-use-package 'graphql)

;; Themes from packages
(straight-use-package 'color-theme-modern)
(straight-use-package 'cyberpunk-theme)
(straight-use-package 'gotham-theme)
(straight-use-package 'green-phosphor-theme)
(straight-use-package 'hc-zenburn-theme)

;; These helm commands I commented because they seemed annoying when I used them.
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
;; These helm commands I commented because I wanted to try without helm, and try selectrum instead.
;; (straight-use-package 'helm)
;; (straight-use-package 'helm-addressbook)
;; (straight-use-package 'helm-bibtex)
;; (straight-use-package 'helm-chrome)
;; (straight-use-package 'helm-company)
;; (straight-use-package 'helm-descbinds)
;; (straight-use-package 'helm-dictionary)
;; (straight-use-package 'helm-dirset)
;; (straight-use-package 'helm-emms)
;; (straight-use-package 'helm-eww)
;; (straight-use-package 'helm-firefox)
;; (straight-use-package 'helm-fuzzy)
;; (straight-use-package 'helm-google)
;; (straight-use-package 'helm-idris)
;; (straight-use-package 'helm-lean)
;; (straight-use-package 'helm-ls-git)
;; (straight-use-package 'helm-mu)
;; (straight-use-package 'helm-shell)
;; (straight-use-package 'helm-system-packages)
;; (straight-use-package 'helm-tramp)
;; (straight-use-package 'helm-wordnet)

(use-package helpful
  :straight t
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command))

(straight-use-package 'ht)
;; https://github.com/coldnew/coldnew-emacs#hydra
(straight-use-package 'hydra) ;; tie related commands into a family of short bindings w/a common prefix.

(global-set-key (kbd "C-x C-b") 'ibuffer)
(straight-use-package 'all-the-icons-ibuffer)

(straight-use-package 'ibuffer-vc) ;; Let Emacs' ibuffer-mode group files by git project etc., and show file state

;; Not needed, I use helm.
;; (straight-use-package 'ido-vertical-mode) ;; makes ido-mode display vertically
(straight-use-package 'iedit) ;; Emacs minor mode and allows you to edit one occurrence of some text in a buffer
(straight-use-package 'info+) ;; Package that enhances some info menus

;; (straight-use-package 'init-loader) ;; No, b/c el-init is better for split init files

(use-package j-mode
  :straight t
  :config (add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . j-mode)))

;; Unneeded
;; (straight-use-package 'jeison)

(straight-use-package 'jump) ;; build functions which contextually jump between files

(straight-use-package 'latex-unicode-math-mode)

;; https://www.emacswiki.org/emacs/LibraryDependencies
(straight-use-package 'loadhist)
;; straight-use-package lib-requires, elisp-depend, exl

(use-package magit
  :straight t
  :config (global-set-key (kbd "C-x g") 'magit-status))

;; Buggy.
;; This looks like what I want, but when I load the hook there's a bug w/it.
;; (use-package magit-filenotify ;; if magit feels slow, disable this.
;;   :straight t
;;   :hook
;;   (magit-status-mode magit-filenotify-mode))

(straight-use-package 'magit-gerrit) ;; gerrit mode for emacs w/magit attachment
(straight-use-package 'magit-popup)

;; (straight-use-package 'markdown-mode+) ;; attic'd, defunct

;; Unclear if I want this, when I have impatient-mode.
(straight-use-package 'markdown-preview-mode)

;; Perform an action every day at "midnight"--e.g. daily calendar
(use-package midnight
  :straight t
  :config
  (setq midnight-hook '(calendar)))

(use-package multiple-cursors
  :straight t
  :hook
  (scheme-mode . multiple-cursors-mode)
  (inferior-scheme-mode . multiple-cursors-mode)
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(straight-use-package 'mc-extras)

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

(use-package org-ref ;; Org-ref
  :straight t
  :config ;; Set up bibliography
  (setq org-ref-default-bibliography '("~/iCloudDrive/bibliography/myBibliography.bib")))

(use-package org-roam-bibtex ;; Org-roam-bibtex
  :straight t
  :config
  (org-roam-bibtex-mode)
  (define-key org-roam-bibtex-mode-map (kbd "C-c n a") #'orb-note-actions))

;; (straight-use-package 'org-roam-server) defunct, org-roam-ui is the good one
(straight-use-package 'org-rtm)
(straight-use-package 'org-sidebar)
(straight-use-package 'org-super-agenda)

(use-package org-trello
  :straight t
  :config
  (setq org-trello-current-prefix-keybinding "C-c o"))

(straight-use-package 'org2web)
(straight-use-package 'ox-gfm) ;; for el2org, instead of ox-md fallback cf https://github.com/tumashu/el2org
(straight-use-package 'ox-jekyll-md)
(straight-use-package 'ox-pandoc)
(straight-use-package 'paradox)
(straight-use-package 'paredit)

(use-package paredit-everywhere ;; Paredit-everywhere-mode is a liar.
  :straight t ;; It turns on *some* of the paredit keybindings but not all, and it doesn't let you choose
  :hook (prog-mode . paredit-everywhere-mode))

(straight-use-package 'paredit-menu)
(straight-use-package 'parent-mode)

;;(straight-use-package 'pdf-tools)
(use-package pdf-tools
  :straight t
;;  :after (pdf-annot fullframe)
;;  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("h"   . 'pdf-annot-add-highlight-markup-annotation)
              ("t"   . 'pdf-annot-add-text-annotation)
              ("D"   . 'pdf-annot-delete)
              ("C-s" . 'isearch-forward)
              ;; ("m"   . 'mrb/mailfile)
              ;; :map pdf-annot-edit-contents-minor-mode-map
              ;; ("<return>"   . 'pdf-annot-edit-contents-commit)
              ;; ("<S-return>" .  'newline)
	      ))

;; Visual Popup Interface Library for Emacs
;; (straight-use-package 'popup)
;; probably useful if I'm developing some GUI packages, but I don't see why I need to manually require it. Straight!

(straight-use-package 'projectile) ;; Project Interaction Library for Emacs http://projectile.readthedocs.io
;; ibuffer-projectile. If I like projectile that is.

(straight-use-package 'proof-general)

;; Not clear: do I want these paredit hooks on racket-mode or paredit mode?
(use-package racket-mode
  :straight t
  :hook
  (racket-mode . enable-paredit-mode) ;; should I need the below?
  (racket-mode . (lambda () (define-key racket-mode-map (kbd "C-c r") 'racket-run)))
  (racket-mode . racket-xp-mode)
  (racket-repl-mode . enable-paredit-mode)
  :config
  (setq racket-program "racket")
  (add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode)))

(straight-use-package 'reazon)
(straight-use-package 'refine)

(straight-use-package 's) ;; The long lost Emacs string manipulation library.
;; (straight-use-package 'scheme-complete) ;; Unclear if I need it — Asked Alex Shinn

(use-package selectrum
  :straight t
  :config
  (selectrum-mode +1)) ;; To turn on selectrum

(use-package selectrum-prescient
  :straight t
  :config
  (selectrum-prescient-mode +1) ;; to make sorting and filtering more intelligent
  (prescient-persist-mode +1)) ;; For selectrum, save your command history on disk, so the sorting gets more intelligent over time

(straight-use-package 'semi)
(straight-use-package 'sh-script) ;; The major mode for editing Unix and GNU/Linux shell script code
(straight-use-package 'smartscan) ;; Quickly jumps between other symbols found at point in Emacs
;; (straight-use-package  'smartparens) ;; A minor mode for parens pairs; paredit probably does all I need
(straight-use-package 'sml-mode)
(straight-use-package 'sml-modeline)
(straight-use-package 'smog)
(straight-use-package 'solarized-emacs)
(straight-use-package 'sourcemap) ;;  Sourmap parser in Emacs Lisp
(straight-use-package 'sx) ;; Stackoverflow mode ;-)
(straight-use-package 'svg-tag-mode)
(straight-use-package 'tabbar)
(straight-use-package 'treepy) ;; tree-walk functionality like a clojure library implementation
(straight-use-package 'ts) ;; A bunch of nice utilities for time and date parsing, better than the built-ins
(straight-use-package 'undo-tree) ;; Treat undo history as a tree
;; vertico ??
(use-package visual-regexp  ;; A regexp/replace command for Emacs with interactive visual feedback
  :straight t
  :config
  (global-set-key (kbd "C-c r") 'vr/replace)
  (global-set-key (kbd "C-c q") 'vr/query-replace))

;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)

(straight-use-package 'visual-regexp-steroids) ;; Extends visual-regexp to support other regexp engines

(straight-use-package 'volatile-highlights) ;; Minor mode for visual feedback on some operations.
(straight-use-package 'w3m)
(straight-use-package 'which-key)

(use-package wordnut
  :straight t
  :config
  (global-set-key [f12] 'wordnut-search)
  (global-set-key [(control f12)] 'wordnut-lookup-current-word))

(use-package wordsmith-mode
  :straight t
  :hook (text-mode . wordsmith-mode))

(straight-use-package 'wrap-region) ;; Emacs minor mode to wrap region with tag or punctuations

(use-package writegood-mode
  :straight t
  :hook (text-mode . writegood-mode))

(use-package ws-butler ;; Unobtrusively trim extraneous white-space *ONLY* in lines edited.
  :straight t
  :config
  (setq ws-butler-global-mode t))

(straight-use-package 'yafolding) ;; Yet another folding extension for Emacs
(straight-use-package 'yaml-mode) ;; The emacs major mode for editing files in the YAML data serialization format.
(straight-use-package 'zones)

(use-package zygospore
  :straight t
  :config (global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows))

(straight-use-package '(all-the-icons :type git :flavor melpa :files (:defaults "data" "all-the-icons-pkg.el") :host github :repo "domtronn/all-the-icons.el"))

;; Also, does this need to be graphics-only, like all-the-icons?
;; https://github.com/domtronn/all-the-icons.el#installation
(use-package all-the-icons–dired
  :straight '(:host github :repo "wyuenho/all-the-icons-dired")
  :hook
  (dired-mode . all-the-icons-dired-mode)
  :config
  (setq all-the-icons-dired-monochrome nil))



(global-set-key (kbd "C-z") #'company-try-hard)
;; global-set-key is a shortcut here for:
;; (define-key (current-global-map) (kbd "C-z") #'company-try-hard)

;; UTF-8 as default encoding
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "utf-8")

;;  Don't bind this globally quoth docs
;; (setq coding-system-for-read 'utf-8)
;; (setq coding-system-for-write 'utf-8)

(straight-use-package '(eldoro :host github :repo "pjones/eldoro"))

;; Disabling because I don't have mail handling in emacs right now
;; (straight-use-package 'wanderlust)
;; Wanderlust doesn't seem to work w/Google 2FA.
;; (autoload 'wl "wl" "Wanderlust" t)
;; (autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
;; (autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; ;; IMAP
;; (setq elmo-imap4-default-server "imap.gmail.com")
;; (setq elmo-imap4-default-user "jason.hemann@gmail.com")
;; (setq elmo-imap4-default-authenticate-type 'clear)
;; (setq elmo-imap4-default-port '993)
;; (setq elmo-imap4-default-stream-type 'ssl)

;; (setq elmo-imap4-use-modified-utf7 t)

;; ;; SMTP
;; (setq wl-smtp-connection-type 'starttls)
;; (setq wl-smtp-posting-port 587)
;; (setq wl-smtp-authenticate-type "plain")
;; (setq wl-smtp-posting-user "jason.hemann")
;; (setq wl-smtp-posting-server "smtp.gmail.com")
;; (setq wl-local-domain "gmail.com")
;; (setq wl-message-id-domain "smtp.gmail.com")

;; (setq wl-default-folder "%inbox")
;; (setq wl-default-spec "%")
;; (setq wl-draft-folder "%[Gmail]/Drafts") ; Gmail IMAP
;; (setq wl-trash-folder "%[Gmail]/Trash")
;; (setq mail-user-agent 'wl-user-agent)
;; (setq wl-folder-check-async t)

;; (setq wl-from "Jason Hemann <jason.hemann@gmail.com>"
;;       ;; All system folders (draft, trash, spam, etc) are placed in the
;;       ;; [Gmail]-folder, except inbox. "%" means it's an IMAP-folder
;;       wl-default-folder "%inbox"
;;       wl-draft-folder   "%[Gmail]/Drafts"
;;       wl-trash-folder   "%[Gmail]/Trash"
;;       ;; The below is not necessary when you send mail through Gmail's SMTP server,
;;       ;; see https://support.google.com/mail/answer/78892?hl=en&rd=1
;;       ;; wl-fcc            "%[Gmail]/Sent"

;;       ;; Mark sent messages as read (sent messages get sent back to you and
;;       ;; placed in the folder specified by wl-fcc)
;;       wl-fcc-force-as-read t

;;       ;; For auto-completing foldernames
;;       wl-default-spec "%")

;; (autoload 'wl-user-agent-compose "wl-draft" nil t)
;; (if (boundp 'mail-user-agent)
;;     (setq mail-user-agent 'wl-user-agent))
;; (if (fboundp 'define-mail-user-agent)
;;     (define-mail-user-agent
;;       'wl-user-agent
;;       'wl-user-agent-compose
;;       'wl-draft-send
;;       'wl-draft-kill
;;       'mail-send-hook))

;; Flim is a help package for dev w/wanderlust
;; (straight-use-package 'flim)

(if (file-exists-p "/Users/jhemann/Documents/acl2/scripts-master/.lisp.el")
    (load-file "/Users/jhemann/Documents/acl2/scripts-master/.lisp.el"))

(let ((gnu-ls-path (executable-find "gls")))
  (when gnu-ls-path
    (setq insert-directory-program gnu-ls-path)))

;; (helm-mode 1)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
;; (global-unset-key (kbd "C-x c"))
;; (require 'helm-config)

;; (global-set-key (kbd "C-x b") 'helm-buffers-list) ;; helm-buffers-list: provides enhanced buffers listing.
;; (global-set-key (kbd "C-h a") 'helm-apropos) ;; enhanced apropos for functions and variables that C-h commands provide.
;; (global-set-key (kbd "C-c h o") 'helm-occur) ;; helm-occur: enhanced occur for one or more buffers; launch from helm-buffers-list or current-buffer.
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files) ;; helm-find-files: one command that handles all the files related commands
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)

;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z

;; (when (executable-find "curl")
;;   (setq helm-google-suggest-use-curl-p t))
;; (global-set-key (kbd "<f6>") #'org-ref-helm-insert-cite-link)

(global-set-key (kbd "C-S-s") (lambda () (interactive (insert "§"))))
(global-set-key (kbd "C-c (") (lambda () (interactive) (insert "ಠ_ಠ")))
(global-set-key (kbd "C-c )") (lambda () (interactive) (insert "¯\\_(ツ)_/¯")))
(global-set-key (kbd "C-c C-x (") (lambda () (interactive) (insert "ᕕ( ᐛ )ᕗ")))
(global-set-key (kbd "C-c C-x )") (lambda () (interactive) (insert "(－‸ლ)")))
(global-set-key (kbd "C-c C-x x") (lambda () (interactive) (insert "!(•̀ᴗ•́)و ̑̑")))

(defun prime-it ()
  "A function to add a prime character."
  (interactive (insert "′")))


;; Because this depends on OSX tooling specifically
(when (and (eq system-type 'darwin) (executable-find "syn"))
  (add-hook 'text-mode-hook 'wordsmith-mode))

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
(when (eq system-type 'darwin)
  (mac-auto-operator-composition-mode t))

(setq-default ispell-program-name (executable-find "aspell"))
(setq-default ispell-list-command "list")

(use-package langtool
  :straight t
  :config
  (global-set-key "\C-x4w" 'langtool-check)
  (global-set-key "\C-x4W" 'langtool-check-done)
  (global-set-key "\C-x4l" 'langtool-switch-default-language)
  (global-set-key "\C-x44" 'langtool-show-message-at-point)
  (global-set-key "\C-x4c" 'langtool-correct-buffer)
  (setq langtool-autoshow-message-function 'langtool-autoshow-detail-popup
	langtool-bin "/usr/local/bin/languagetool"
	langtool-default-language "en-US"
	langtool-mother-tongue "en"))

(defun langtool-autoshow-detail-popup (overlays)
  ". OVERLAYS."
  (when (require 'popup nil t)
    ;; Do not interrupt current popup
    (unless (or popup-instances
                ;; suppress popup after type `C-g` .
                (memq last-command '(keyboard-quit)))
      (let ((msg (langtool-details-error-message overlays)))
        (popup-tip msg)))))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
;; (setq-default TeX-master "master") ; set a master for in the future.
(setq-default TeX-master nil)

(setq reftex-default-bibliography '("~/old-microKanren.bib"))
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


(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
(add-hook 'text-mode-hook 'visual-line-mode)

(add-hook 'TeX-mode-hook (function (lambda () (setq ispell-parser 'tex))))
(add-hook 'tex-mode-hook 'auxtex-mode)

(add-hook 'tex-mode-hook (lambda () (define-key tex-mode-map (kbd "C-c C-k") 'compile)))
(add-hook 'tex-mode-hook (lambda () (define-key tex-mode-map (kbd "C-c |") 'align-current)))

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)

(add-hook 'TeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'TeX-mode-hook 'visual-line-mode)

(add-hook 'TeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'TeX-mode-hook 'TeX-PDF-mode)
(add-hook 'TeX-mode-hook 'TeX-fold-mode)
(add-hook 'TeX-mode-hook (lambda () (TeX-fold-mode 1)))
(add-hook 'TeX-mode-hook #'TeX-fold-mode) ;; Automatically activate TeX-fold-mode.
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)


;; Only sometimes works!
;; must be after font locking is set up for the buffer on!
;; ... whatever that means
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

(add-to-list 'auto-mode-alist '("\\.v$" . coq-mode))

(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

(add-hook 'emacs-lisp-mode-hook                    #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook   #'enable-paredit-mode)
(add-hook 'ielm-mode-hook                          #'enable-paredit-mode) ;; inferior-emacs-lisp-mode
(add-hook 'lisp-mode-hook                          #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook              #'enable-paredit-mode)
(add-hook 'scheme-mode-hook                        #'enable-paredit-mode)
(add-hook 'inferior-scheme-mode-hook               #'enable-paredit-mode)


(add-hook 'idris-mode-hook                         #'enable-paredit-mode)
(add-hook 'idris-repl-mode-hook                    #'enable-paredit-mode)
;; (add-hook 'ciao-mode-hook                          #'enable-paredit-mode) ;; not til fix paren space issue.
;; tex-mode has paredit-mode issue too.
(add-hook 'idris-prover-script-mode-hook           #'enable-paredit-mode)

(global-set-key (kbd "{") 'paredit-open-curly)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; So that I can find scryer-prolog
(add-to-list 'exec-path (expand-file-name (substitute-in-file-name "$HOME/.cargo/bin")))

(mapc (lambda (pr) (put (car pr) 'racket-indent-function (cdr pr)))
      '((conde . 0)
        (fresh . 1)
        (run . 1)
        (run* . 1)
        (run . 2)
	(letrec . 0)))

;; From https://stackoverflow.com/questions/36183071/how-can-i-preview-markdown-in-emacs-in-real-time/36189456
;; For use with impatient-mode
(defun markdown-html (buffer)
  "BUFFER that we should use w/impatient mode to render md in weblike-form."
  (princ (with-current-buffer buffer
    (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
  (current-buffer)))

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
   '(emacs-lisp-mode lisp-mode lisp-interaction-mode slime-repl-mode c-mode cc-mode c++-mode go-mode java-mode malabar-mode clojure-mode clojurescript-mode scala-mode scheme-mode ocaml-mode tuareg-mode coq-mode haskell-mode perl-mode cperl-mode python-mode ruby-mode lua-mode tcl-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode less-css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode web-mode ts-mode sclang-mode verilog-mode qml-mode racket-mode Racket-mode racket-repl-mode idris-mode idris-repl-mode))
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
 '(default-input-method "TeX")
 '(dired-listing-switches "-alGh1v --group-directories-first --time-style=long-iso" nil nil "long format, w/hidden files, w/o group information, w/good numeric sorting human-readable sizes, and w/directories first")
 '(dired-recursive-copies 'always nil nil "I shouldn't be prompted to recursively copy dirs")
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(find-file-visit-truename t)
 '(fringe-mode 2 nil (fringe))
 '(global-auto-revert-non-file-buffers t)
 '(global-display-line-numbers-mode t)
 '(history-length 50)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-highlight-face 'highlight)
 '(ispell-highlight-p t)
 '(load-home-init-file t t)
 '(ls-lisp-dirs-first t)
 '(mac-system-move-file-to-trash-use-finder t)
 '(make-backup-files nil)
 '(ns-alternate-modifier '(:ordinary meta :mouse alt))
 '(org-agenda-files '("tasks.org"))
 '(org-agenda-include-diary t)
 '(org-agenda-start-with-log-mode 'only)
 '(org-babel-load-languages '((scheme . t)))
 '(org-directory "~/.org")
 '(org-export-backends '(ascii html icalendar latex md org))
 '(org-fold-catch-invisible-edits 'smart)
 '(org-list-allow-alphabetical t)
 '(org-log-done 'time)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-w3m))
 '(org-src-tab-acts-natively t)
 '(org-support-shift-select t)
 '(org-time-stamp-custom-formats '("<%m/%d/%y %a>" . "<%a %_B %_d, %H:%M>"))
 '(org-use-speed-commands t)
 '(preview-auto-cache-preamble t)
 '(reftex-cite-format 'biblatex)
 '(reftex-extra-bindings t t)
 '(reftex-plug-into-AUCTeX t t)
 '(require-final-newline t nil nil "Add an EOL to files when I save them.")
 '(revert-without-query '("'(\".*\")"))
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
 '(save-place-mode t)
 '(savehist-mode t)
 '(scheme-program-name "scheme" nil nil "scheme defaults to chez scheme on my system.")
 '(scroll-bar-mode 'right)
 '(select-enable-clipboard t)
 '(sentence-end-double-space nil)
 '(show-paren-delay 0)
 '(show-paren-mode 1)
 '(show-trailing-whitespace t)
 '(sort-fold-case t t)
 '(straight-host-usernames
   '((gitlab . "jasonhemann")
	 (github . "jasonhemann")
	 (bitbucket . "jhemann")))
 '(straight-use-package-by-default t)
 '(tab-always-indent 'complete)
 '(tab-width 4 nil nil "Switching to a 4-space tab")
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(vc-follow-symlinks t)
 '(vc-make-backup-files t)
 '(version-control t)
 '(view-read-only t)
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

;; Don't know why this isn't working Pu
;; (define-key flyspell-mode-map (kbd "C-;") 'flyspell-popup-correct)
;; You can also enable flyspell-popup-auto-correct-mode to popup that
;; Popup Menu automatically with a delay (default 1.6 seconds):
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)
;; (add-hook 'TeX-mode-hook 'flyspell-preprocess-buffer) ;; somehow void

(setq flyspell-issue-message-flag nil)
(setq flyspell-issue-welcome-flag nil)
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

(setq-default major-mode 'text-mode)
(setq-default ispell-list-command "list")

(global-set-key "\C-x4w" 'langtool-check)
(global-set-key "\C-x4W" 'langtool-check-done)
(global-set-key "\C-x4l" 'langtool-switch-default-language)
(global-set-key "\C-x44" 'langtool-show-message-at-point)
(global-set-key "\C-x4c" 'langtool-correct-buffer)

(defun langtool-autoshow-detail-popup (overlays)
  "OVERLAYS."
  (when (require 'popup nil t)
    ;; Do not interrupt current popup
    (unless (or popup-instances
                ;; suppress popup after type `C-g` .
                (memq last-command '(keyboard-quit)))
      (let ((msg (langtool-details-error-message overlays)))
        (popup-tip msg)))))

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
;; M-x smog-check, smog-check-region I thought used to exist but must not be autoloaded
;; C-h a does apropos
;; M-v custom-enabled-themes tells you what themes are in force.
;; In org-mode C-' on a table.el table lets you edit it nicely, like that.
;; C-h r for the manual, then g (for "goto node").
;; M-x table-capture https://www.gnu.org/software/emacs/manual/html_node/emacs/Table-Conversion.html
;; M-x list-processes
;; C-s search C-q C-i a literal tab character
;; custom-file is the variable to set location of customizations
;; M-x custom-enabled-themes to describe the current theme(s) loaded
;; M-x describe-theme gives the deets on whatever theme is running
;; M-x straight-normalize-all
;; M-x find-library to a bunch of libraries in their locations
;; C-x = to get a whole bunch of char info incl. overlays
;; M-x proced is the emacs ps replacement
;; M-x find-grep is an improved way to use find and grep together
;; M-x find-grep-dired is like that but it opens in a dired buffer
;; Diminish mode will help me clean up my modeline
;; straight--build-cache has the dependencies listed
;; C-h o ⇒ What's this thing?
;; C-h e ⇒ What'd /Emacs/ do?
;; C-h l ⇒ What'd /I/ do?
;; C-h ? ⇒ What're the help topics? —gives possible completions to “C-h ⋯”.
;; “I accidentally hit a key, which one and what did it do!?” ⇒ C-h e and C-h l, then use C-h o to get more details on the action. ;-)
;; Finally, C-h d asks nicely what 'd'ocumentation you're interested in. After providing a few keywords, the apropos tool yields possible functions and variables that may accomplish my goal.

;; This was some setup that I did to the minibuffer; not sure it was wise
;; (add-hook 'eval-expression-minibuffer-setup-hook 'my-minibuffer-setup)
;; (add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
;; (defun my-minibuffer-setup ()
;;   (set (make-local-variable 'face-remapping-alist)
;;        '((default :height 5.0))))

;; Emacs desiderata

;; Get code to color parens again for latex files.
;; Setup emacs calendar to sync with google calendar
;; Spacing with parens in various non-lisp modes that you use w/paredit mode.
;; use David's .emacs as a sample, to set things up properly.

;; From https://github.com/Vidianos-Giannitsis/Dotfiles/tree/master/emacs/.emacs.d
;; I believe, how to do things in GUI and non-GUI mode
;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;; 		(lambda (frame)
;; 		  (setq doom-modeline-icon t)
;; 		  (with-selected-frame frame
;; 		    (set-font-faces))))
;;   (set-font-faces))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(j-verb-face ((t (:foreground "Red"))))
 '(j-adverb-face ((t (:foreground "Green"))))
 '(j-conjunction-face ((t (:foreground "Blue"))))
 '(j-other-face ((t (:foreground "Black"))))
 '(diary ((t (:foreground "dark red")))))

(define-key org-roam-mode-map [mouse-1] #'org-roam-visit-thing)

(with-eval-after-load 'ox-latex
   (add-to-list 'org-latex-classes
                '("letter"
                  "\\documentclass{report}"
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(pdf-tools-install) ;; if this slows things down try (pdf-loader-install)

(require 'org-protocol)
(require 'org-roam-protocol)

;; Draw tabs with the same color as trailing whitespace
;; (add-hook 'font-lock-mode-hook
;;   '(lambda ()
;;      (font-lock-add-keywords
;;        nil
;;         '(("\t" 0 'trailing-whitespace prepend))
;;      )
;;    )
;; )

(use-package ob-racket
  :after org
  :hook (ob-racket-pre-runtime-library-load . ob-racket-raco-make-runtime-library)
  :straight (:type git :host github :repo "togakangaroo/ob-racket"
		   :files ("*.el" "*.rkt")))

;; Pick a random theme.
(load-theme (nth (cl-random (length (custom-available-themes))) (custom-available-themes)) t)    ;; To have it always remember this is safe
(fset 'yes-or-no-p 'y-or-n-p)
;; default to mononoki
(set-face-attribute 'default nil
                    :family "mononoki"
                    :height 120
                    :weight 'normal
                    :width  'normal)

;; (file-dependents (feature-file 'cl))

; @begin(39781165)@ - Do not edit these lines - added automatically!
(if (file-exists-p "/Users/jhemann/Documents/ciao/ciao_emacs/elisp/ciao-site-file.el")
  (load-file "/Users/jhemann/Documents/ciao/ciao_emacs/elisp/ciao-site-file.el"))
; @end(39781165)@ - End of automatically added lines.

;; https://cliplab.org/~clip/Software/Ciao/ciao-1.15.0.html/CiaoMode.html#Installation%20of%20the%20Ciao%20emacs%20interface
;; https://github.com/ciao-lang/ciao_emacs
;; (if (file-exists-p "/usr/local/lib/ciao/ciao-mode-init.el")
;;     (load-file "/usr/local/lib/ciao/ciao-mode-init.el"))

;; Start the emacs server, so that I can use emacsclient to connect to the existing emacs instance
;; I need another way to do this. To instead have an Emacs.app -like thing do it
;; (server-start)


(provide 'init.el)
;;; init.el ends here
