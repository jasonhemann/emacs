
;;; Commentary:

;;; This is Jason Hemann's .emacs setup, intended for OSX and some
;;; linux machines. It is currently in an unstable state, and the
;;; dependencies outside my .emacs are not listed. Several aspects of
;;; this rely on packages from homebrew, and a number of other
;;; downloaded files and hard-coded directories.

;;; Code:

;; Need to be set before we load straight.el, to correct a flycheck
;; incompatibility.
(setq straight-fix-flycheck t
	  use-package-compute-statistics t
;; Configuration for how straight.el should load.
	  load-prefer-newer t)

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

;; What if we are not online? We ignore that problem here.
(straight-pull-recipe-repositories)

(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; JBH 4/6/22 disabling because it was seeming slow
;; (ruler-mode 1)

;; From my package.el days
;; Don’t need htmlize. I’d only use it w/.org and .md; those are supported elsewhere.
;; x-dict is emacs attic, so no need.
;; dictionary is also a emacs 21 era thing, so no need.

(straight-use-package 'delight)
(straight-use-package 'use-package)
(straight-use-package 'use-package-ensure-system-package)

;; So that I can publicly VC my config w/o leaking secret keys &c.
(straight-use-package
 '(use-package-secret :host github
					  :repo "emacswatcher/use-package-secret"
					  :fork (:host github :repo "jasonhemann/use-package-secret")))

(use-package exec-path-from-shell ;; Make Emacs use the $PATH set up by the user's shell
  :if (memq window-system '(mac ns))
  :straight t
  :config (exec-path-from-shell-initialize))

(use-package org
  :straight t
  :config
  (add-to-list 'org-latex-classes
               '("letter"
                 "\\documentclass{report}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  :bind (("C-c l" . org-store-link)
		 ("C-c a" . org-agenda)
		 ("C-c c" . org-capture)))

(use-package ob-prolog
  :after org
  :straight t
  :demand t
  :config (add-to-list 'org-babel-load-languages '(prolog . t)))

;; display/update images in the buffer after evaluation
;; This has 'org-display-inline-images already installed, so just the append.
;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(use-package agda2-mode
  :straight (:includes (eri annotation))
  :mode (("\\.agda\\'" . agda2-mode)
		 ("\\.lagda.md\\'" . agda2-mode)))

(straight-use-package '(simple-httpd :includes web-server :files ("*.el")))

(use-package impatient-mode ;; replacement for flymd
  :straight t
  :hook markdown-mode)

(straight-use-package '(faceup :type built-in)) ;; b/c this is newer than the one from straight, lexical binding
(straight-use-package '(let-alist :type built-in))
(straight-use-package '(which-key :custom (which-key-mode)))
(straight-use-package '(helm :files ("*.el" "emacs-helm.sh" (:exclude "helm-lib.el" "helm-source.el" "helm-multi-match.el" "helm-core.el" "helm-core-pkg.el") "helm-pkg.el")))

(straight-use-package '(rg :ensure-system-package rg))

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
		  (add-to-list
		   'display-buffer-alist
		   '("\\*org-roam\\*"
			 (display-buffer-in-direction)
			 (direction . right)
			 (window-width . 0.33)
			 (window-height . fit-window-to-buffer)))
  :straight t
  :custom
  (org-roam-directory (file-truename "~/.org/"))
  :bind (:map org-roam-mode-map
		 ("C-c n l" . org-roam)
		 ("C-c n f" . org-roam-find-file)
		 ("C-c n g" . org-roam-graph)
		 ([mouse-1] . org-roam-visit-thing)
		 :map org-mode-map
		 ("C-c n i" . org-roam-insert)
		 ("C-c n I" . org-roam-insert-immediate)))

;; I don't know when I need an ~:after~ flag
(use-package org-roam-ui
    :straight (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out"))
    :after org-roam
    :hook (after-init . org-roam-ui-mode)
    :custom
	(org-roam-ui-sync-theme t)
	(org-roam-ui-follow t)
	(org-roam-ui-update-on-save t)
	(org-roam-ui-open-on-start t))

(straight-use-package 'academic-phrases)

(use-package artbollocks-mode
  :straight t
  :delight
  :hook text-mode)

(straight-use-package 'ace-jump-mode)

;; displays current match and total matches information
;; global search count mode
(use-package anzu
  :straight t
  :delight
  :config
  (global-anzu-mode +1))

(straight-use-package 'apel) ;; portable emacs extensions; unclear how relevant

;; Need to think about how to set up hooks w/auctex.
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

(straight-use-package 'bug-hunter) ;; how to fix bugs in an init file, by auto-bisect

(straight-use-package 'buffer-move) ;; used for rotating buffers. buf-move-left

;; https://github.com/kiwanami/emacs-calfw
;; To use, must configure
;; TODO when time.
(use-package calfw
  :straight t
  :custom
  (cfw:fchar-junction ?╋)
  (cfw:fchar-vertical-line ?┃)
  (cfw:fchar-horizontal-line ?━)
  (cfw:fchar-left-junction ?┣)
  (cfw:fchar-right-junction ?┫)
  (cfw:fchar-top-junction ?┯)
  (cfw:fchar-top-left-corner ?┏)
  (cfw:fchar-top-right-corner ?┓))

(straight-use-package 'calfw-cal)
(straight-use-package 'calfw-gcal)
(straight-use-package 'calfw-ical)
(straight-use-package 'calfw-org)
;; No need for howm-mode; org-mode + roam for me
;; (straight-use-package 'calfw-howm)

(straight-use-package 'cbm) ;; cycle by major mode

(use-package cdlatex
  :straight t
  :delight
  :hook
  ;; with Emacs latex mode, then with AUCTeX LaTeX mode
  ((latex-mode TeX-mode) . turn-on-cdlatex))

(straight-use-package 'cl-lib) ;; Properly prefixed CL functions and macros

(use-package clang-format
  :straight t
  :bind ([C-M-tab] . clang-format-region))

(use-package comment-dwim-2 ;; A replacement for the emacs' built-in command comment-dwim
  :straight t
  :bind
  (("M-;" . comment-dwim-2)
   :map org-mode-map
   ("M-;" . 'org-comment-dwim-2)))

(use-package company ;; Complete anything ;-)
  :straight t
  :delight " 🏭"
  :config (global-company-mode))

(straight-use-package 'company-coq)
(straight-use-package 'company-dict)
(straight-use-package 'lean-mode)

(use-package company-lean
  :straight t
  ;; Trigger completion on Shift-Space
  ;; Was ~company-complete~, but company-try-hard does more
  :bind ("S-SPC" . company-try-hard))

(straight-use-package 'company-math)
(straight-use-package 'company-org-roam)
(straight-use-package 'company-auctex)
(straight-use-package 'company-bibtex)

(use-package company-try-hard
  :straight t
  :bind (("C-z" . company-try-hard)
		 :map company-active-map
		 ("C-z" . company-try-hard)))

(straight-use-package 'company-fuzzy)
(straight-use-package 'consult) ;; the counsel equivalent for selectrum
(straight-use-package 'coq-commenter)
(straight-use-package 'crux) ;; collection of emacs extensions

(straight-use-package 'dash) ;; A modern list library for Emacs
(straight-use-package 'dash-functional)

(straight-use-package '(dired-hacks-utils :host github :repo "Fuco1/dired-hacks" :fork (:host github :repo "jasonhemann/dired-hacks")))

(use-package dired-collapse
  :straight (:host github :repo "Fuco1/dired-hacks"
		    :fork (:host github :repo "jasonhemann/dired-hacks")) ;; This is now correct
  :hook dired-mode)

(use-package dired+
  :straight t
  :custom (diredp-hide-details-initially-flag nil))

;; (straight-use-package 'discover) ;; discover more of Emacs. Sadly, moribund

(straight-use-package 'discover-my-major) ;; Discover key bindings and their meaning for the current Emacs major mode

(use-package dr-racket-like-unicode
  :straight t
  :hook
  ((racket-mode racket-repl) . racket-unicode-input-method-enable))

(straight-use-package 'clean-aindent-mode) ;; Emacs extension for simple indent and unindent
(straight-use-package 'dtrt-indent) ;; A minor mode that guesses the indentation offset originally used for creating source code

(straight-use-package 'duplicate-thing) ;; duplicate current line
(straight-use-package 'easy-jekyll)

(use-package ebib
  :straight t
  :bind ("\C-ce" . ebib)
  :custom
   (ebib-bibtex-dialect 'biblatex)) ;; ebib mode, for latex

;; TODO change from scryer-prolog to configurable
(use-package ediprolog
  :straight t
  :bind ([f10] . ediprolog-dwim))

(straight-use-package 'edit-indirect)
(straight-use-package 'org-edit-indirect)

(straight-use-package 'el2org)

;; these provide ways to cleanly split an init file up
(straight-use-package 'el-init)
(straight-use-package 'el-init-viewer)

;;  el-mock Maybe I need it, but I don't think so.
(straight-use-package 'el-patch)

(use-package eldoc ;; the argument list of the function call you are currently writing
  :straight t
  :delight)

;; The following package requires some set-up to work with org-mode or w/e.
(straight-use-package 'elmacro) ;; https://github.com/Silex/elmacro#elmacro-processors

;; s-u-p elscreen ?
(straight-use-package 'elscreen-separate-buffer-list)

;; Emacs start up profiler isn't working the way it should
(use-package esup
  ;; :custom `(esup-user-init-file ,(file-truename "~/init.el")) I thought this would help, but alas
  :straight t)

(straight-use-package '(empv :host github :repo "isamert/empv.el" :ensure-system-package mpv))

;; Needed b/c closql wasn't working?
(straight-use-package 'closql)

(use-package epkg ;; epkg-describe-package should show the dependencies
  :straight t
  :after closql)

(straight-use-package 'expand-region) ;; Increase selected region by semantic units
(straight-use-package 'f) ;; Modern API for working with files and directories in Emacs
;; fontawesome is abandonware

(use-package flycheck
  :straight t
  ;; Commented because this way drops important data.
  ;; :delight " F✓"
  :config (global-flycheck-mode +1)
  ;;  Consider as a fix to flycheck-mode, see https://github.com/flycheck/flycheck/issues/153#issuecomment-19450255
  :custom (flycheck-highlighting-mode 'lines))

(straight-use-package '(flycheck-textlint :type git :host github :repo "kisaragi-hiu/flycheck-textlint" :fork nil))

(use-package gradle-mode ;; I should want maven, I think, tbqh
  :straight t
  :hook java-mode)

(straight-use-package 'flycheck-gradle)

;; Another java mode
(straight-use-package 'meghanada)

;; All eclim abandoned and better use java-lsp.
;; Skip company-emacs-eclim ac-emacs-eclim too.

;; Take a look at what he has here
;; (load "~/Documents/eliemacs/eliemacs")

;; Logtalk mode is not found right now. BUG.
(autoload 'logtalk-mode "logtalk" "Major mode for editing Logtalk programs." t)
(add-to-list 'auto-mode-alist '("\\.lgt\\'" . logtalk-mode))
(add-to-list 'auto-mode-alist '("\\.logtalk\\'" . logtalk-mode))

(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))


;; No need to use these, as flycheck is better
;; (straight-use-package 'flylisp) ;; Add highlighting to mismatched parentheses, so you can see the mistake
;; (straight-use-package 'flymake-easy)
;; (straight-use-package 'flymake-gradle)
;; (straight-use-package 'flymake-racket)
;; (straight-use-package 'flymd) No longer works w/FF >= 68

;; Intended to make flyspell zippier
(use-package flyspell-lazy
  :straight t
  :config (flyspell-lazy-mode +1))

(use-package flyspell-popup
  :after flyspell
  :straight (:host github :repo "xuchunyang/flyspell-popup")
  :bind (:map flyspell-mode-map
		 ("C-;" . flyspell-popup-correct))
  :hook (flyspell-mode . flyspell-popup-auto-correct-mode))

(straight-use-package 'fullframe) ;; Advise commands to execute fullscreen, restoring the window setup when exiting.
(straight-use-package 'gh-md)

(straight-use-package 'ghub)
(straight-use-package 'ghub+)

(straight-use-package 'git-timemachine) ;; Walk through git revisions of a file

(straight-use-package 'goto-chg) ;; Goto last change in current buffer. Needed?
(straight-use-package 'graphql)

;; Themes from packages. I have enough; don't think I need these.
;; (straight-use-package 'color-theme-modern)
;; (straight-use-package 'cyberpunk-theme)
;; (straight-use-package 'gotham-theme)
;; (straight-use-package 'green-phosphor-theme)
;; (straight-use-package 'hc-zenburn-theme)
;; (straight-use-package 'solarized-emacs)

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
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-c C-d" . helpful-at-point)
   ("C-h F" . helpful-function)
   ("C-h C" . helpful-command)))

(use-package highlight-indent-guides
  :straight t
  :hook prog-mode-hook)

(straight-use-package 'ht)
;; https://github.com/coldnew/coldnew-emacs#hydra
(straight-use-package 'hydra) ;; tie related commands into a family of short bindings w/a common prefix.

;; Adds GUI-based stuff that augments the text-based info flow
(use-package hyperbole
  :straight t
  :custom (hyperbole-mode))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(straight-use-package 'all-the-icons-ibuffer)

(straight-use-package 'ibuffer-vc) ;; Let Emacs' ibuffer-mode group files by git project etc., and show file state

;; Not needed, b/c I was using helm. Now I'm using selectrum instead.
;; (straight-use-package 'ido-vertical-mode) ;; makes ido-mode display vertically
(straight-use-package 'iedit) ;; Emacs minor mode and allows you to edit one occurrence of some text in a buffer
(straight-use-package 'info+) ;; Package that enhances some info menus

;; (straight-use-package 'init-loader) ;; No, b/c el-init is better for split init files

(use-package j-mode
  :straight t
  :mode ("\\.ij[rstp]$" . j-mode))

;; This fork was more up-to-date than the o.g. janet-mode repo.
(straight-use-package '(janet-mode :fork (:host github :repo "pierre-rouleau/janet-mode")))

;; Unneeded
;; (straight-use-package 'jeison)

(straight-use-package 'jump) ;; build functions which contextually jump between files

(use-package latex-unicode-math-mode
  :straight t
  :hook LaTeX-mode)

;; https://www.emacswiki.org/emacs/LibraryDependencies
(straight-use-package 'loadhist)
;; straight-use-package lib-requires, elisp-depend, exl

(use-package magit
  :straight t
  :bind ("C-x g" . magit-status))

;; Buggy. This looks like what I want, but when magit-status-mode-hook
;; runs, I get an error that magit-filenotify.el does not actually
;; define the magit-filenotify that it claims to provide at eof.
;;
;; I have tried to :declare magit-filenotify, since I think maybe
;; that's needed. I have also tried to add explicitly the name of the
;; mode ~(magit-status-mode . magit-filenotify-mode)~ in case that's
;; it.
;;
;; (use-package magit-filenotify ;; if magit feels slow, disable this.
;;   :straight t
;;   :hook magit-status-mode)

;; gerrit mode for emacs w/magit attachment
;; (straight-use-package 'magit-gerrit)
(straight-use-package 'magit-popup)

;; (straight-use-package 'markdown-mode+) ;; attic'd, defunct

;; Unclear if I want this, when I have impatient-mode.
(straight-use-package 'markdown-preview-mode)

;; Perform an action every day at "midnight"--e.g. daily calendar
(use-package midnight
  :straight t
  :custom (midnight-hook '(calendar)))

(use-package multiple-cursors
  :straight t
  :config
  (multiple-cursors-mode +1)
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

(straight-use-package 'mc-extras)

(straight-use-package 'mustache)
(straight-use-package 'neotree) ;; A emacs tree plugin like NerdTree for Vim.
;; (straight-use-package 'nlinum) %% with emacs 26 built-in line numbering, not wanted
(straight-use-package '(org-ac :hook org-mode))

(use-package org-bullets
  :straight t
  :hook ((org-mode org-roam-mode) . org-bullets-mode))

(straight-use-package 'org-dropbox)
(straight-use-package 'org-doing)
(straight-use-package 'org-dotemacs)
(straight-use-package 'org-inline-pdf)
(straight-use-package 'org-jekyll)
(straight-use-package 'org-journal)
(straight-use-package 'org-ql)

;; See (org-ref-manual) for some documentation
;; Unclear if I actually _want_ this system, or if I'll prefer the built-in org-cite behavior.
(use-package org-ref ;; Org-ref
  :straight t
  :config
  (setq org-ref-default-bibliography '("~/old-microKanrenbib.bib")))
;; Set up bibliography

(use-package org-roam-bibtex ;; Org-roam-bibtex
  :straight t
  :config (org-roam-bibtex-mode +1)
  :bind (:map org-roam-bibtex-mode-map
	     ("C-c n a" . orb-note-actions)))

;; (straight-use-package 'org-roam-server) defunct, org-roam-ui is the good one
(straight-use-package 'org-rtm)
(straight-use-package 'org-sidebar)
(straight-use-package 'org-super-agenda)

;; TODO: org-trello-mode, when I load it, seems to change how the
;; org-mode files indent and breaks tab-through org-mode behavior.
(use-package org-trello
  :straight t
  :custom (org-trello-current-prefix-keybinding "C-c o"))

(straight-use-package 'org2web)
(straight-use-package 'ox-gfm) ;; for el2org, instead of ox-md fallback cf https://github.com/tumashu/el2org
(straight-use-package 'ox-jekyll-md)
(straight-use-package 'ox-pandoc)
(straight-use-package 'paradox)


;; A minor mode for parens pairs; commenting paredit in favor of smartparens
;; (use-package paredit
;;   :straight t
;;   :bind (:map paredit-mode-map
;; 		 ("{"   . paredit-open-curly)
;; 		 ("}"   . paredit-close-curly))
;;   :hook
;;   ((agda2-mode
;; 	emacs-lisp-mode
;; 	eval-expression-minibuffer-setup
;; 	ielm-mode ;; inferior-emacs-lisp-mode
;; 	lisp-mode
;; 	lisp-interaction-mode
;; 	scheme-mode-hook
;; 	racket-mode
;; 	racket-repl-mode
;; 	idris-mode
;; 	idris-repl-mode
;; 	idris-prover-script-mode
;; 	inferior-scheme-mode) . enable-paredit-mode)
;;   :config
;;   ;; terminal emacs seems to use some of these sequences for moving
;;   ;; around, both directly as in to switch between open windows in the
;;   ;; terminal application, and also perhaps as parts of a sequences
;;   ;; when using the GUI window stuff in terminal.
;;   ;;
;;   (unless terminal-frame
;; 	(bind-keys :map paredit-mode-map
;; 			   ("M-[" . paredit-wrap-square)
;; 			   ("M-{" . paredit-wrap-curly))))

;; ;; Paredit-everywhere-mode is a liar. It turns on *some* of the
;; ;; paredit keybindings but not all, and it doesn't let you choose
;; (use-package paredit-everywhere
;;   :straight t
;;   :bind ("{" . 'paredit-open-curly)
;;   :hook
;;   (prog-mode . paredit-everywhere-mode))

;; (straight-use-package 'paredit-menu)

(straight-use-package 'parent-mode)

(use-package pdf-tools
  :straight t
;;  :after (pdf-annot fullframe)
  :magic ("%PDF" . pdf-view-mode)
  :config (pdf-tools-install :no-query) ;; if this slows things down try (pdf-loader-install)
  :bind (:map pdf-view-mode-map
		 ("h"   . 'pdf-annot-add-highlight-markup-annotation)
		 ("t"   . 'pdf-annot-add-text-annotation)
		 ("D"   . 'pdf-annot-delete)
		 ("C-s" . 'isearch-forward))
              ;; ("m"   . 'mrb/mailfile)
              ;; :map pdf-annot-edit-contents-minor-mode-map
              ;; ("<return>"   . 'pdf-annot-edit-contents-commit)
              ;; ("<S-return>" .  'newline)
			  )

;; Visual Popup Interface Library for Emacs
;; (straight-use-package 'popup)
;; probably useful if I'm developing some GUI packages, but I don't see why I need to manually require it. Straight!

;projectile add .projectile to any file
(use-package projectile ;; Project Interaction Library for Emacs
  :straight t
  :custom
  (projectile-sort-order recently-active)
  (projectile-completion-system selectrum)
  (projectile-indexing-method hybrid) ; 'alien 'native
  (projectile-enable-caching t)
  :config
  (projectile-mode +1)
  (setq projectile-mode-line-function '(lambda () (format " Projectile[%s]" (projectile-project-name))))
  ;; (setq projectile-switch-project-action 'projectile-dired)
  ;; (setq projectile-switch-project-action 'helm-projectile)

  (with-eval-after-load 'projectile
	(defun projectile-find-file-other-window (&optional invalidate-cache)
	  "Jump to a project's file using completion and show it in another window. With a prefix arg INVALIDATE-CACHE invalidates the cache first."
	  (interactive "P")
	  (progn
		(setq split-window-preferred-function 'split-window-sensibly
			  split-window-preferred-function nil)
		(projectile--find-file invalidate-cache #'find-file-other-window))))
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-mode-map
		 ("C-c p" . projectile-command-map)))

;; http://projectile.readthedocs.io
;; ibuffer-projectile. If I like projectile that is.

;; Broken
;; No coq-mode package
;; (use-package proof-general
;;   :defines (coq-mode-map
;;             proof-prog-name-ask
;;             proof-follow-mode
;;             proof-sticky-errors
;;             proof-splash-seen)
;;   :straight t
;;   :mode ("\\.v$" . coq-mode)
;;   :init (push ".v.d" completion-ignored-extensions)
;;   :custom
;;   (proof-prog-name-ask t)
;;   (proof-follow-mode 'followdown)
;;   (proof-sticky-errors t)
;;   (proof-splash-seen t)
;;   (coq-accept-proof-using-suggestion 'never)
;;   :config (flycheck-mode 0)
;;   :bind (:map coq-mode-map
;; 			  (("s-n" . proof-assert-next-command-interactive)
;; 			   ("s-<down>" . proof-assert-next-command-interactive)
;; 			   ("s-<right>" . proof-goto-point)
;; 			   ("s-<up>" . proof-undo-last-successful-command)
;; 			   ("s-<left>" . proof-goto-end-of-locked)
;; 			   ("s-<end>" . proof-process-buffer))))

(use-package racket-mode
  :straight t
  :bind (:map racket-mode-map ("C-c r" . racket-run))
  :hook
  (racket-mode . racket-xp-mode)
  (racket-mode . racket-smart-open-bracket-mode)
  (racket-mode . (lambda () (flycheck-mode -1))) ;; disable flycheck in racket b/c Rk✓
  (racket-repl-mode . racket-smart-open-bracket-mode)
  :custom (racket-program "racket")
  :mode ("\\.rkt\\'" . racket-mode))

(straight-use-package 'scribble-mode)

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

(use-package smex
  :straight t
  :config (smex-initialize) ; Can be omitted. This might cause a (minimal) delay when Smex is auto-initialized on its first run.
  :bind (("M-x" . smex)
		 ("M-X" . smex-major-mode-commands)
		 ("C-c C-c M-x" . execute-extended-command))) ;; This is your old M-x.

(straight-use-package 'semi)
(straight-use-package 'sh-script) ;; The major mode for editing Unix and GNU/Linux shell script code
(straight-use-package 'smartscan) ;; Quickly jumps between other symbols found at point in Emacs

(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config)
  (sp-use-paredit-bindings +1)
  :hook ((prog-mode text-mode) . turn-on-smartparens-strict-mode))

(straight-use-package 'sml-mode)
(straight-use-package 'sml-modeline)
(straight-use-package 'smog)
(straight-use-package 'sourcemap) ;;  Sourmap parser in Emacs Lisp
(straight-use-package 'sx) ;; Stackoverflow mode ;-)
(straight-use-package 'svg-tag-mode)
(straight-use-package 'tabbar)
(straight-use-package 'treepy) ;; tree-walk functionality like a clojure library implementation
(straight-use-package 'ts) ;; A bunch of nice utilities for time and date parsing, better than the built-ins

(use-package vterm
  :straight t
  :bind (("C-c t" . vterm))
  :custom (vterm-always-compile-module t))

(use-package eshell-vterm
  :straight t
;;  Not clear that I should need these configuration options, b/c dependencies.
;;  :demand t
;;  :after eshell
  :config
  (eshell-vterm-mode)
  (defalias 'eshell/v 'eshell-exec-visual))

(use-package volatile-highlights ;; Minor mode for visual feedback on some operations.
  :straight t
  :delight
  :config (volatile-highlights-mode +1))

;; I don't think I like undo-tree. Weird undo structure.
;; (use-package undo-tree ;; Treat undo history as a tree
;;   :straight t
;;   :config
;;   (global-undo-tree-mode)
;;   BTW Config section assumes volatile highlights
;;   (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
;;   (vhl/install-extension 'undo-tree))

;; vertico ??

(use-package visual-regexp  ;; A regexp/replace command for Emacs with interactive visual feedback
  :straight t
  :bind (("C-c r" . vr/replace)
         ("C-c q". vr/query-replace)
		 ;; if you use multiple-cursors, this is for you:
		 ("C-c m" . 'vr/mc-mark)))

(straight-use-package 'visual-regexp-steroids) ;; Extends visual-regexp to support other regexp engines

(use-package wc-mode
  :straight t
  :hook text-mode
  :bind ("\C-cw" . wc-mode))

(use-package w3m
  :straight t
   :custom (w3m-use-tab-line nil))

(use-package which-key
  :straight t
  :delight
  :custom (which-key-mode t))

(use-package wordnut
  :straight t
  :delight
  :bind (([f12] . wordnut-search)
		 ([(control f12)] . wordnut-lookup-current-word)))

(use-package wordsmith-mode
  :if (eq system-type 'darwin) ;; Because this depends on OSX tooling specifically
  :ensure-system-package syn ;; I need to tell it how to install syn if missing.
  :delight " ✒"
  :straight t
  :hook text-mode)

;; I think this would be useful for block comments, but I am not sure
;; if this is what this is supposed to be for.
;;
(use-package wrap-region ;; Emacs minor mode to wrap region with tag or punctuations
  :straight t
  :delight
  :custom
  (wrap-region-global-mode t))

;; ⮐

(use-package writegood-mode
  :straight t
  :delight " 💯"
  :hook (org-mode text-mode)
  :bind (("C-c g"     . writegood-mode)
		 ("C-c C-g g" . writegood-grade-level)
		 ("C-c C-g e" . writegood-reading-ease)))

(use-package ws-butler ;; Unobtrusively trim extraneous white-space *ONLY* in lines edited.
  :straight t
  :config
  (setq ws-butler-global-mode t))

(straight-use-package 'xr) ;; The reverse regex library (regex->Human)
(straight-use-package 'yafolding) ;; Yet another folding extension for Emacs
(straight-use-package 'yaml-mode) ;; The emacs major mode for editing files in the YAML data serialization format.
(straight-use-package 'zones)

(use-package zygospore
  :straight t
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))

(use-package all-the-icons
  :straight t
  :config
  (ignore-error end-of-file
	(all-the-icons-install-fonts t)))

;; Also, does this need to be graphics-only, like all-the-icons?
;; https://github.com/domtronn/all-the-icons.el#installation
(use-package all-the-icons-dired
  :straight (:host github :repo "wyuenho/all-the-icons-dired")
  :custom (all-the-icons-dired-monochrome nil)
  :delight
  :hook (dired-mode . all-the-icons-dired-mode))

;; global-set-key is a shortcut here for:
;; (define-key (current-global-map) (kbd "C-z") #'company-try-hard)

;; UTF-8 as default encoding
;; (set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "utf-8")
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

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

(if (file-exists-p "~/Documents/acl2/scripts-master/.lisp.el")
    (load-file "~/Documents/acl2/scripts-master/.lisp.el"))

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

;; C-x 8 S (interactive (insert "§"))
(global-set-key (kbd "C-c (") (lambda () (interactive (insert "ಠ_ಠ"))))
(global-set-key (kbd "C-c )") (lambda () (interactive (insert "¯\\_(ツ)_/¯"))))
(global-set-key (kbd "C-c C-x (") (lambda () (interactive (insert "ᕕ( ᐛ )ᕗ"))))
(global-set-key (kbd "C-c C-x )") (lambda () (interactive (insert "(－‸ლ)"))))
(global-set-key (kbd "C-c C-x x") (lambda () (interactive (insert "!(•̀ᴗ•́)و ̑̑"))))

(defun prime-it ()
  "A function to add a prime character."
  (interactive (insert "′")))

(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
	    (id (one-or-more (not (any " "))))
	    (message) line-end))
  :modes (text-mode markdown-mode gfm-mode))

(add-to-list 'flycheck-checkers 'proselint)

;; Only in Emacs mac-port
(when (eq system-type 'darwin)
  (setq mac-auto-operator-composition-mode t
		mac-system-move-file-to-trash-use-finder t))

(setq-default ispell-program-name (executable-find "aspell"))
(setq-default ispell-list-command "--list")

(if ispell-program-name
	(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US") ;; we want this just when the ispell program is aspell
		  ispell-highlight-face 'highlight
		  ispell-highlight-p t)
  (t (message "No aspell found!")))

(global-set-key (kbd "<f8>") 'ispell-word)
(bind-key "H-$" 'ispell-word) ;; Hyper, one presumes.

(use-package langtool
  :straight t
  ;; I don't like that I have the same string in two places
  :ensure-system-package languagetool
  :bind (("\C-x4w" . langtool-check)
		 ("\C-x4W" . langtool-check-done)
		 ("\C-x4l" . langtool-switch-default-language)
		 ("\C-x44" . langtool-show-message-at-point)
		 ("\C-x4c" . langtool-correct-buffer))
  :custom
  (langtool-bin (executable-find "languagetool"))
  (langtool-mother-tongue "en")
  (langtool-autoshow-message-function 'langtool-autoshow-detail-popup)
  (langtool-default-language "en-US")
  :config
  (with-eval-after-load 'langtool
	(defun langtool-autoshow-detail-popup (overlays)
	  "OVERLAYS."
	  (when (require 'popup nil t)
		;; Do not interrupt current popup
		(unless (or popup-instances
					;; suppress popup after type `C-g` .
					(memq last-command '(keyboard-quit)))
		  (let ((msg (langtool-details-error-message overlays)))
			(popup-tip msg)))))))

;; (setq-default TeX-master "master") ; set a master for in the future.
(setq-default TeX-master nil)

(setq reftex-default-bibliography '("~/old-microKanren.bib"))

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

(add-hook 'text-mode-hook 'flyspell-mode)

;; See the following for a related issue and workaround. Gets complicated.
;; https://github.com/Fuco1/smartparens/issues/854
(add-hook 'text-mode-hook 'electric-quote-mode)

;; TeX-latex-mode, LaTeX-mode, TeX-mode, tex-mode, latex-mode, auxtex-mode
(add-hook 'TeX-mode-hook (function (lambda () (setq ispell-parser 'tex))))
(add-hook 'tex-mode-hook (lambda () (define-key tex-mode-map (kbd "C-c C-k") 'compile)))
(add-hook 'tex-mode-hook (lambda () (define-key tex-mode-map (kbd "C-c |") 'align-current)))

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'TeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'TeX-mode-hook 'TeX-PDF-mode)
(add-hook 'TeX-mode-hook 'TeX-fold-mode) ;; Automatically activate TeX-fold-mode.

;; (enabled-minor-modes
;;  (auto-composition-mode)
;;  (auto-compression-mode)
;;  (auto-encryption-mode)
;;  (auto-fill-mode)
;;  (auto-save-mode)
;;  (company-tng-mode)
;;  (counsel-mode)
;;  (display-line-numbers-mode)
;;  (electric-indent-mode)
;;  (emojify-mode)
;;  (file-name-shadow-mode)
;;  (fira-code-mode)
;;  (font-lock-mode)
;;  (global-eldoc-mode)
;;  (global-emojify-mode)
;;  (global-font-lock-mode)
;;  (ivy-mode)
;;  (ivy-prescient-mode)
;;  (ivy-rich-mode)
;;  (line-number-mode)
;;  (mouse-wheel-mode)
;;  (override-global-mode)
;;  (paredit-mode)
;;  (prescient-persist-mode)
;;  (prettify-symbols-mode)
;;  (semantic-minor-modes-format)
;;  (shell-dirtrack-mode)
;;  (transient-mark-mode))

;; (disabled-minor-modes
;;  (abbrev-mode)
;;  (archive-subfile-mode)
;;  (auto-complete-mode)
;;  (auto-fill-function)
;;  (auto-package-update-minor-mode)
;;  (auto-save-visited-mode)
;;  (avy-linum-mode)
;;  (buffer-face-mode)
;;  (buffer-read-only)
;;  (cl-old-struct-compat-mode)
;;  (company-mode)
;;  (company-search-mode)
;;  (compilation-minor-mode)
;;  (compilation-shell-minor-mode)
;;  (completion-in-region-mode)
;;  (dash-fontify-mode)
;;  (defining-kbd-macro)
;;  (delete-selection-mode)
;;  (diff-auto-refine-mode)
;;  (diff-minor-mode)
;;  (dired-hide-details-mode)
;;  (eldoc-mode)
;;  (electric-layout-mode)
;;  (electric-quote-mode)
;;  (emojify-debug-mode)
;;  (emojify-mode-line-mode)
;;  (evil-smartparens-mode)
;;  (flyspell-mode)
;;  (general-override-local-mode)
;;  (general-override-mode)
;;  (global-auto-complete-mode)
;;  (global-company-mode)
;;  (global-dash-fontify-mode)
;;  (global-emojify-mode-line-mode)
;;  (global-fira-code-mode)
;;  (global-ligature-mode)
;;  (global-prettify-symbols-mode)
;;  (global-reveal-mode)
;;  (global-semantic-highlight-edits-mode)
;;  (global-semantic-highlight-func-mode)
;;  (global-semantic-show-parser-state-mode)
;;  (global-semantic-show-unmatched-syntax-mode)
;;  (global-semantic-stickyfunc-mode)
;;  (global-visual-line-mode)
;;  (horizontal-scroll-bar-mode)
;;  (hs-minor-mode)
;;  (ido-everywhere)
;;  (image-minor-mode)
;;  (isearch-mode)
;;  (ispell-minor-mode)
;;  (ivy-rich-project-root-cache-mode)
;;  (jit-lock-debug-mode)
;;  (ligature-mode)
;;  (menu-bar-mode)
;;  (next-error-follow-minor-mode)
;;  (org-cdlatex-mode)
;;  (org-list-checkbox-radio-mode)
;;  (org-src-mode)
;;  (org-table-follow-field-mode)
;;  (org-table-header-line-mode)
;;  (orgtbl-mode)
;;  (outline-minor-mode)
;;  (overwrite-mode)
;;  (paragraph-indent-minor-mode)
;;  (racket-smart-open-bracket-mode)
;;  (racket-xp-mode)
;;  (rectangle-mark-mode)
;;  (reveal-mode)
;;  (semantic-highlight-edits-mode)
;;  (semantic-highlight-func-mode)
;;  (semantic-mode)
;;  (semantic-show-parser-state-mode)
;;  (semantic-show-unmatched-syntax-mode)
;;  (semantic-stickyfunc-mode)
;;  (sh-electric-here-document-mode)
;;  (show-smartparens-global-mode)
;;  (show-smartparens-mode)
;;  (size-indication-mode)
;;  (slime-edit-value-mode)
;;  (slime-editing-mode)
;;  (slime-macroexpansion-minor-mode)
;;  (slime-mode)
;;  (slime-popup-buffer-mode)
;;  (smartparens-global-mode)
;;  (smartparens-global-strict-mode)
;;  (smartparens-mode)
;;  (smartparens-strict-mode)
;;  (tab-bar-history-mode)
;;  (tab-bar-mode)
;;  (tar-subfile-mode)
;;  (temp-buffer-resize-mode)
;;  (text-scale-mode)
;;  (tool-bar-mode)
;;  (tooltip-mode)
;;  (unify-8859-on-decoding-mode)
;;  (unify-8859-on-encoding-mode)
;;  (url-handler-mode)
;;  (use-hard-newlines)
;;  (vc-parent-buffer)
;;  (view-mode)
;;  (visible-mode)
;;  (visual-line-mode)
;;  (window-divider-mode)
;;  (xref-etags-mode))

;; (defmacro def-pairs (pairs)
;;   "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
;; conses, where NAME is the function name that will be created and
;; STRING is a single-character string that marks the opening character.

;;   (def-pairs ((paren . \"(\")
;;               (bracket . \"[\"))

;; defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
;; respectively."
;;   `(progn
;;      ,@(loop for (key . val) in pairs
;;              collect
;;              `(defun ,(read (concat
;;                              "wrap-with-"
;;                              (prin1-to-string key)
;;                              "s"))
;;                   (&optional arg)
;;                 (interactive "p")
;;                 (sp-wrap-with-pair ,val)))))

;; (def-pairs ((paren . "(")
;;             (bracket . "[")
;;             (brace . "{")
;;             (single-quote . "'")
;;             (double-quote . "\"")
;;             (back-quote . "`")))

;; (bind-keys
;;  :map smartparens-mode-map
;;  ("C-M-a" . sp-beginning-of-sexp)
;;  ("C-M-e" . sp-end-of-sexp)

;;  ("C-<down>" . sp-down-sexp)
;;  ("C-<up>"   . sp-up-sexp)
;;  ("M-<down>" . sp-backward-down-sexp)
;;  ("M-<up>"   . sp-backward-up-sexp)

;;  ("C-M-f" . sp-forward-sexp)
;;  ("C-M-b" . sp-backward-sexp)

;;  ("C-M-n" . sp-next-sexp)
;;  ("C-M-p" . sp-previous-sexp)

;;  ("C-S-f" . sp-forward-symbol)
;;  ("C-S-b" . sp-backward-symbol)

;;  ("C-<right>" . sp-forward-slurp-sexp)
;;  ("M-<right>" . sp-forward-barf-sexp)
;;  ("C-<left>"  . sp-backward-slurp-sexp)
;;  ("M-<left>"  . sp-backward-barf-sexp)

;;  ("C-M-t" . sp-transpose-sexp)
;;  ("C-M-k" . sp-kill-sexp)
;;  ("C-k"   . sp-kill-hybrid-sexp)
;;  ("M-k"   . sp-backward-kill-sexp)
;;  ("C-M-w" . sp-copy-sexp)
;;  ("C-M-d" . delete-sexp)

;;  ("M-<backspace>" . backward-kill-word)
;;  ("C-<backspace>" . sp-backward-kill-word)
;;  ([remap sp-backward-kill-word] . backward-kill-word)

;;  ("M-[" . sp-backward-unwrap-sexp)
;;  ("M-]" . sp-unwrap-sexp)

;;  ("C-x C-t" . sp-transpose-hybrid-sexp)

;;  ("C-c ("  . wrap-with-parens)
;;  ("C-c ["  . wrap-with-brackets)
;;  ("C-c {"  . wrap-with-braces)
;;  ("C-c '"  . wrap-with-single-quotes)
;;  ("C-c \"" . wrap-with-double-quotes)
;;  ("C-c _"  . wrap-with-underscores)
;;  ("C-c `"  . wrap-with-back-quotes))

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

;; I don't know if this is what I want here.

(straight-use-package 'idris-mode)

;; (add-hook 'ciao-mode-hook #'enable-paredit-mode) ;; not til fix paren space issue.
;; tex-mode has paredit-mode issue too.

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
 '(TeX-auto-save t)
 '(TeX-auto-untabify t)
 '(TeX-engine 'xetex)
 '(TeX-master 'dwim t)
 '(TeX-parse-self t)
 '(ac-modes
   '(emacs-lisp-mode lisp-mode lisp-interaction-mode slime-repl-mode c-mode cc-mode c++-mode go-mode java-mode malabar-mode clojure-mode clojurescript-mode scala-mode scheme-mode ocaml-mode tuareg-mode coq-mode haskell-mode perl-mode cperl-mode python-mode ruby-mode lua-mode tcl-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode less-css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode web-mode ts-mode sclang-mode verilog-mode qml-mode racket-mode Racket-mode racket-repl-mode idris-mode idris-repl-mode))
 '(ad-redefinition-action 'accept)
 '(apropos-sort-by-scores t)
 '(auto-save-interval 75)
 '(auto-save-timeout 75)
 '(bib-cite-use-reftex-view-crossref t t)
 '(bibtex-maintain-sorted-entries 'plain)
 '(blink-cursor-mode nil)
 '(bookmark-save-flag 0)
 '(calendar-mark-diary-entries-flag t)
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(custom-safe-themes t)
 '(debug-on-error t)
 '(default-input-method "TeX")
 '(describe-bindings-outline t)
 '(dired-dwim-target 'dired-dwim-target-recent nil nil "Not obvious which option is *best*, but this at least works when I put two direds side-by-side.")
 '(dired-listing-switches "-alGh1v --group-directories-first --time-style=long-iso" nil nil "long format, w/hidden files, w/o group information, w/good numeric sorting human-readable sizes, and w/directories first")
 '(dired-recursive-copies 'always nil nil "I shouldn't be prompted to recursively copy dirs")
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 ;; I don’t think I want this; when I set it and created a new email
 ;; in emacs, it signalled an error wrt this setting.
 ;;
 ;; '(display-time-use-mail-icon t)
 '(find-file-visit-truename t)
 '(flycheck-check-syntax-automatically '(save idle-change mode-enabled) nil nil "flycheck was a time-hog w/Racket mode, so I disabled newline check & delayed to 4sec")
 '(flycheck-idle-change-delay 4)
 '(flyspell-issue-welcome-flag nil)
 '(fringe-mode 2 nil (fringe))
 '(global-auto-revert-non-file-buffers t)
 '(global-display-line-numbers-mode t)
 '(global-visual-line-mode t)
 '(history-length 50)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-highlight-face 'highlight)
 '(ispell-highlight-p t)
 '(ispell-program-name "aspell")
 '(load-home-init-file t t)
 '(ls-lisp-dirs-first t)
 '(make-backup-files nil)
 '(ns-alternate-modifier '(:ordinary meta :mouse alt))
 '(org-agenda-files '("tasks.org"))
 '(org-agenda-include-diary t)
 '(org-agenda-start-with-log-mode 'only)
 '(org-confirm-babel-evaluate nil)
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
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello) "Customized with use-package org-trello")
 '(org-use-speed-commands t)
 '(preview-auto-cache-preamble t)
 '(prolog-compile-string
   '((eclipse "[%f].")
	 (mercury "mmake ")
	 (sicstus
	  (eval
	   (if
		   (prolog-atleast-version
			'(3 . 7))
		   "prolog:zap_file(%m,%b,compile,%l)." "prolog:zap_file(%m,%b,compile).")))
	 (scryer "%f")
	 (swi "[%f].")
	 (t "compile(%f).")))
 '(prolog-consult-string
   '((eclipse "[%f].")
	 (mercury nil)
	 (sicstus
	  (eval
	   (if
		   (prolog-atleast-version
			'(3 . 7))
		   "prolog:zap_file(%m,%b,consult,%l)." "prolog:zap_file(%m,%b,consult).")))
	 (swi "[%f].")
	 (scryer "consult(%f).")
	 (gnu "[%f].")
	 (t "reconsult(%f).")))
 '(prolog-program-name
   '(((getenv "EPROLOG")
	  (eval
	   (getenv "EPROLOG")))
	 (eclipse "eclipse")
	 (mercury nil)
	 (sicstus "sicstus")
	 (swi "swipl")
	 (scryer "scryer-prolog")
	 (gnu "gprolog")
	 (t "prolog")))
 '(prolog-system 'scryer)
 '(prolog-system-version
   '((sicstus
	  (3 . 6))
	 (swi
	  (0 . 0))
	 (mercury
	  (0 . 0))
	 (eclipse
	  (3 . 7))
	 (gnu
	  (0 . 0))
	 (scryer
	  (0 . 9))
	 (azprolog
	  (9 . 63))))
 '(reftex-cite-format 'biblatex)
 '(reftex-extra-bindings t)
 '(reftex-plug-into-AUCTeX t)
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
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(sort-fold-case t nil nil "Make buffer-sort functions case-insensitive")
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
 '(visible-bell t)
 '(window-combination-resize t))

;; IIRC I didn't want to use ~with-eval-after-load~
(with-eval-after-load "flycheck-mode"
  (flycheck-define-checker proselint
    "A linter for prose"
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end))
    :modes (text-mode markdown-mode gfm-mode org-mode))
  (add-to-list 'flycheck-checkers 'proselint))

(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
(global-set-key (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word)

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word."
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)

;; Add opam emacs directory to the load-path This whole substring
;; thing just strips the newline. Seems weird way to do it. Check 's'
;; package for possibly better function?
;;
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

;; Pre-load Andromeda
(require 'andromeda-autoloads)

(fset 'make-k-ri
      (lambda (&optional arg)
		"For 311, to make continuations RI."
		"Assumes k has some formal parameters"
		"To start, leave mark at end of last match line in apply-k."
		(interactive "p")
        (kmacro-exec-ring-item
		 (quote ([134217749 134217749 134217749 134217734 134217732 134217732 134217732 134217749 201326624 134217847 134217749 134217730 134217734 25 134217730 134217730 201326624 134217847 134217732 25 32 134217749 201326624 134217765 32 return 32 44 return 33 134217749 96 2 201326624 23 134217732 134217734 134217734 return 25 134217732 25 201326624 201326624 23 134217749 134217730 134217734 201326624 23 134217749 134217749 201326624 tab 134217730 134217734 134217748 2 2 2 134217730 134217730 134217734 25 134217749 201326624 tab 134217734 134217730 134217734 2 134217730 134217730 134217734] 0 "%d")) arg)))

;; Emacs desiderata
;; I need to write a keyboard macro for going from let* to begin/set!
;; Setup emacs calendar to sync with google calendar
;; Spacing with parens in various non-lisp modes that you use w/paredit mode.
;; use David Christiansen's .emacs as a sample, to set things up properly.
;; Cleanup the mode settings sitting here commented out.

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
 '(diary ((t (:foreground "dark red"))))
 '(font-lock-function-name-face ((t (:foreground "#385e6b" :weight bold)))))



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

;; relies on phantomjs, which is discontinued upstream
;; (straight-use-package 'ob-browser)

(use-package org-babel-eval-in-repl
  :straight t
  :bind (:map org-mode-map
		 ("C-<return>" . 'ober-eval-in-repl)
		 ("M-<return>" . 'ober-eval-block-in-repl)))

(use-package ob-racket
  :after org
  :straight (ob-racket :type git :host github :repo "hasu/emacs-ob-racket" :files ("*.el" "*.rkt"))
  :after org
  :hook (ob-racket-pre-runtime-library-load . ob-racket-raco-make-runtime-library)
  :config (add-to-list 'org-babel-load-languages '(racket . t)))

;; (use-package ob-racket
;;   :after org
;;   :hook (ob-racket-pre-runtime-library-load . ob-racket-raco-make-runtime-library)
;;   :config
;;   (setq org-babel-command:racket "/usr/local/bin/racket")
;;   (add-to-list 'org-babel-load-languages '(racket . t))
;;   :straight (:type git :host github :repo "togakangaroo/ob-racket" :files ("*.el" "*.rkt")))

(setq-default major-mode 'text-mode)
;; Pick a random theme.
(load-theme (nth (cl-random (length (custom-available-themes))) (custom-available-themes)) t) ;; To have it always remember this is safe
(fset 'yes-or-no-p 'y-or-n-p)

;; default to mononoki, 22pt font
(set-face-attribute 'default nil
                    :family "mononoki"
                    :height 220
                    :weight 'normal
                    :width  'normal)

;; (file-dependents (feature-file 'cl))

;; Commenting because my Ciao mode install sucks and I need prolog mode.
; @begin(39781165)@ - Do not edit these lines - added automatically!
;; (if (file-exists-p "~/Documents/ciao/ciao_emacs/elisp/ciao-site-file.el")
;;   (load-file "~/Documents/ciao/ciao_emacs/elisp/ciao-site-file.el"))
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
