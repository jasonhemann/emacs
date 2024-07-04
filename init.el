
;;; Commentary:

;;; This is Jason Hemann's .emacs setup, intended for OSX and some
;;; linux machines. Several aspects of this rely on packages from
;;; homebrew.

;;; Code:

;; Need to be set before we load straight.el, to correct a flycheck
;; incompatibility.
(defvar straight-fix-flycheck)
(defvar use-package-compute-statistics)
(defvar straight-host-usernames)
(setq straight-fix-flycheck t
      use-package-compute-statistics t
;; Configuration for how straight.el should load.
      load-prefer-newer t
      straight-host-usernames '((gitlab . "jasonhemann")
								(github . "jasonhemann")
								(bitbucket . "jhemann")))

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


(use-package benchmark-init
  :straight t
  :config
  (benchmark-init/activate)
  ;; To stop benchmarking after init
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(defvar straight-check-for-modifications)
(setq straight-fix-flycheck t
      straight-check-for-modifications '(check-on-save find-when-checking)
      straight-host-usernames '((gitlab . "jasonhemann")
                                (github . "jasonhemann")
                                (bitbucket . "jhemann")))

(use-package straight-x ;; Adds the straight-x commands to clean up straight install
  :defer t)

;; What if we are not online? We ignore that problem here.
;; (straight-pull-recipe-repositories)
;; Should also disable copilot-mode auto if we don’t have that locally available

;; JBH 4/6/22 disabling because it was seeming slow
;; (ruler-mode 1)

;; From my package.el days
;; Don’t need htmlize. I’d only use it w/.org and .md; those are supported elsewhere.
;; x-dict is emacs attic, so no need.
;; dictionary is also a emacs 21 era thing, so no need.

(require 'server)
(unless (server-running-p)
  (server-start))

(straight-use-package 'delight)
(straight-use-package 'djvu)
(straight-use-package '(use-package :custom (use-package-compute-statistics t)))
(straight-use-package 'use-package-ensure-system-package)

;; So that I can publicly VC my config w/o leaking secret keys &c.
(straight-use-package
 '(use-package-secret :host github :repo "emacswatcher/use-package-secret" :fork t :branch "patch-1"))

(use-package emacs
  :delight
  (auto-revert-mode)
  (auto-fill-function " AF")
  ;; (visual-line-mode)
)

;; seems to duplicate the inclusion of my .bash_profile behavior, disabled b/c I don't want to duplicate path
;;
;; (use-package exec-path-from-shell ;; Make Emacs use the $PATH set up by the user's shell
;;   :if (not (eq system-type 'windows-nt))
;;   :straight t
;;   :config (exec-path-from-shell-initialize))

;(use-package fontaine) ;; I want to eventually add to be config w/Prot.


(use-package orderless
  :straight t
  :custom
  (completion-category-defaults nil)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion orderless)))))

;; (org-mode . org-indent-mode) annoying, see emacs-deficiencies
(use-package org
  :straight t
  :after wordsmith-mode
  :init
  (defun org--disable-wordsmith-mode ()
    "Disable wordsmith mode."
    (wordsmith-mode -1))
  :config (setq org-effort-property "EFFORT") ;; This causes EFFORT to look like all the others
  :bind (:map org-mode-map
		 ("C-c l" . org-store-link)
		 ("C-c a" . org-agenda)
		 ("C-c c" . org-capture)
		 ("C-c b" . org-switchb)
		 ("C-c C-x C-x" . org-clock-in-last)
		 ;; Because smartparens shadows these, we rebind them otherwise.
		 ("M-<up>" . nil)
		 ("M-<down>" . nil)
		 ("C-c M-<up>" . org-metaup)
		 ("C-c M-<down>" . org-metadown))

 ;; Then enable org-cdlatex-mode
  :custom (org-agenda-files '("tasks.org" "/Users/jhemann/class/2023/Summer/tfp/tfp-to-do.org"))
		  (org-agenda-start-with-log-mode '(closed))
		  (org-confirm-babel-evaluate nil)
		  (org-directory "~/.org")
		  (org-export-backends '(ascii html icalendar latex odt md org))
		  (org-export-with-smart-quotes t)
		  (org-export-allow-bind-keywords t)
		  (org-fast-tag-selection-include-todo t)
		  (org-fold-catch-invisible-edits 'smart)
		  (org-list-allow-alphabetical t)
		  (org-log-done 'time)
		  (org-modules '(org-tempo ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail)) ;; ol-w3m outdated
		  (org-refile-targets '((org-agenda-files . (:maxlevel . 2))
								(nil . (:maxlevel . 2))))
		  (org-src-tab-acts-natively t)
		  (org-support-shift-select t)
		  (org-tags-column 0)
		  (org-time-stamp-custom-formats '("<%m/%d/%y %a>" . "<%a %_B %_d, %H:%M>"))
		  (org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELED(c)")))
		  (org-todo-keyword-faces '(("TODO" . "red")
									("IN-PROGRESS" . "dark goldenrod")
									("WAITING" . "orange")
									("SOMEDAY" . "dodger blue")
									("DONE" . "lime green")
									("CANCELED" . "purple")))
		  (org-use-speed-commands t)
		  (org-use-sub-superscripts '{})
		  (org-use-tag-inheritance nil)
		  :hook (org-mode . org--disable-wordsmith-mode))

(defun my-ignore-delete-windows (&rest args)
  "Run original function with all ARGS.
For the scope of this function, make `delet-other-windows' the same as `ignore'."
  (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
    (apply args)))

;; (advice-add 'org-fast-todo-selection :around 'my-ignore-delete-windows)


;;   :config
;; ORG-CDLATEX-KEYBINDINGS SHADOW ORG-CYCLE
;; (org-mode . org-cdlatex-setup)
;; (defun org-cdlatex-setup ()
;;   "Setup org-cdlatex."
;;   (turn-on-cdlatex)   ;; Ensure cdlatex is on
;;   (org-cdlatex-mode))

;; https://superuser.com/a/1106691/963448 b/c ox-latex not loaded w/org.
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("letter"
                 "\\documentclass{letter}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(use-package ob-prolog
  :after (org ediprolog)
  :straight t
  :demand t
  :config (add-to-list 'org-babel-load-languages '(prolog . t)))

;; display/update images in the buffer after evaluation
;; This has 'org-display-inline-images already installed, so just the append.
;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
;;
;; Don’t know where this code came from, possible this should now be
;; org-display-remote-inline-images. Still don’t understand the append.

(use-package agda2-mode
  :straight (:includes (eri annotation))
  :ensure-system-package agda
  :mode (("\\.agda\\'" . agda2-mode)
		 ("\\.lagda.md\\'" . agda2-mode)))

(straight-use-package '(simple-httpd :includes web-server :files ("*.el")))

(use-package impatient-mode ;; replacement for flymd
  :straight t
  :hook markdown-mode)



(straight-use-package '(faceup :type built-in)) ;; b/c this is newer than the one from straight, lexical binding
(straight-use-package '(let-alist :type built-in))
(straight-use-package '(which-key :config (which-key-mode)))
(straight-use-package '(helm :files ("*.el" "emacs-helm.sh" (:exclude "helm-lib.el" "helm-source.el" "helm-multi-match.el" "helm-core.el" "helm-core-pkg.el") "helm-pkg.el")))
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
;; These helm commands I commented because I wanted to try without helm, and try vertico instead.
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

(straight-use-package '(rg :ensure-system-package rg))

(straight-use-package 'unicode-fonts)
(straight-use-package 'font-utils) ;; Apparently nice for working w/fonts in emacs.

;; A preferred synonyms package, but check use-cases.
(straight-use-package 'powerthesaurus)

;; An improvement over the author's multi-term package
(use-package aweshell
  :straight (:host github :repo "manateelazycat/aweshell" :files ("*.el" "out")))

;; In order to search for synonyms.
(use-package www-synonyms
  :straight t
  :secret www-synonyms-key)

(straight-use-package 'emacsql-sqlite-builtin)

(use-package org-roam
  :demand t
  :after (emacsql-sqlite-builtin org)
  :config (add-to-list
		   'display-buffer-alist
		   '("\\*org-roam\\*"
			 (display-buffer-in-direction)
			 (direction . right)
			 (window-width . 0.33)
			 (window-height . fit-window-to-buffer)))
  :straight t
  :custom (org-roam-db-connection-type 'sqlite-builtin)
          (org-roam-db-autosync-mode t)
          (org-roam-directory (file-truename "~/.org/"))
  :bind (:map org-mode-map
		 ("C-c n l" . org-roam)
		 ("C-c n f" . org-roam-find-file)
		 ("C-c n g" . org-roam-graph)
		 ([mouse-1] . org-roam-visit-thing)
		 ("C-c n i" . org-roam-insert)
		 ("C-c n I" . org-roam-insert-immediate)))


(straight-use-package 'editorconfig)
(straight-use-package 'jsonrpc)
(use-package copilot-mode
  :straight (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist"))
  :after (jsonrpc editorconfig) ;; These two were needed for copilot to work
  ;; There should be some other way to do this, but I don't know what it is.
  :hook prog-mode
  :bind (:map prog-mode-map
		 ("C-c C-a" . copilot-accept-completion)
		 ("C-c C-c" . copilot-current-completion)
		 ("C-c C-n" . copilot-next-completion)
		 ("C-c C-p" . copilot-previous-completion)))


(use-package org-roam-ui
	:straight (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out"))
	;; I don't know when I need an ~:after~ flag
	:after org-roam
	:delight (org-roam-ui-mode "ORUI")
	(org-roam-ui-follow-mode " F-")
	;; Cannot use hook here, b/c need to catch possible error.
	;; :hook (after-init . org-roam-ui-mode)
	:custom
	(org-roam-ui-sync-theme t)
	(org-roam-ui-follow t)
	(org-roam-ui-update-on-save t)
	(org-roam-ui-open-on-start t))

(straight-use-package 'academic-phrases)

(use-package artbollocks-mode
  :straight (:host github :repo "sachac/artbollocks-mode" :fork t :files ("*.el" "out"))
  :delight
  :hook text-mode)

(straight-use-package 'ace-jump-mode)

;; displays current match and total matches information
;; global search count mode
(use-package anzu
  :straight t
  :custom (global-anzu-mode t))

(straight-use-package 'apel) ;; portable emacs extensions; unclear how relevant

;; Need to think about how to set up hooks w/auctex.

;;;;;;;;;
(defun my-tex-mode-setup ()
  "My customizations for TeX mode."
  (setq ispell-parser 'tex)
  (define-key LaTeX-mode-map (kbd "C-c C-k") 'compile)
  (define-key LaTeX-mode-map (kbd "C-c |") 'align-current))

(setq TeX-source-correlate-method 'synctex)
(use-package auctex
  :straight t
  :custom
  (TeX-view-program-selection
   '((output-dvi "open")
	 (output-pdf "PDF Tools")
	 (output-html "open")))
  (TeX-view-program-list
      '(("PDF Tools" "TeX-pdf-tools-sync-view")
        ("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  (TeX-auto-TeX-command t)
  (TeX-auto-save t)
  (TeX-auto-untabify t)
  (TeX-engine 'xetex)
  (TeX-master 'dwim)
  (TeX-parse-self t)
  (TeX-source-correlate-start-server t)
  :config
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :hook (;(LaTeX-mode . turn-on-auto-fill)
		 (LaTeX-mode . my-tex-mode-setup)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . LaTeX-preview-setup)
         (LaTeX-mode . turn-on-reftex) ;; with AUCTeX LaTeX mode
         (LaTeX-mode . turn-on-cdlatex)
         (TeX-mode . TeX-source-correlate-mode)
         (TeX-mode . TeX-PDF-mode)
         (TeX-mode . TeX-fold-mode))) ;; Automatically activate TeX-fold-mode.

(straight-use-package 'auctex-latexmk)
(straight-use-package 'auto-compile) ;; Automatically compile Emacs Lisp libraries

;; Auto complete is for most things strictly worse than company-mode
;; (straight-use-package 'auto-complete)
;; (require 'auto-complete-config)
;; (ac-mode 1)
;; (ac-config-default)
;; '(ac-modes
;;   '(emacs-lisp-mode lisp-mode lisp-interaction-mode slime-repl-mode c-mode cc-mode c++-mode go-mode java-mode malabar-mode clojure-mode clojurescript-mode scala-mode scheme-mode ocaml-mode tuareg-mode coq-mode haskell-mode perl-mode cperl-mode python-mode ruby-mode lua-mode tcl-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode less-css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode web-mode ts-mode sclang-mode verilog-mode qml-mode racket-mode Racket-mode racket-repl-mode idris-mode idris-repl-mode))
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

;; (straight-use-package 'ac-math) commented in favor of company-math
;; (straight-use-package 'auto-complete-auctex)

;; (straight-use-package 'auto-package-update) ;; straight has this feature already

;; No longer needed as of 24.X see electric pair mode
;; (straight-use-package 'autopair)
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


(global-set-key (kbd "M-/") 'hippie-expand)
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; How is this different than https://github.com/emacs-straight/uniquify-files/blob/master/uniquify-files.el
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)


;; To use, must configure and debug
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

(defvar calfw-diary-sources nil) ;; Emacs Diary Schedules
(use-package calfw-cal
  :straight t
  :after calfw
  :config
  (setq calf-diary-sources (list (cfw:cal-create-source "Orange")))) ; diary source

(use-package calfw-blocks ;; Amazing for calendar block views
  :straight t
  :ensure t
  :after (calfw calfw-org))

;; (straight-use-package 'calfw-gcal) Not sure what this does that ical doesnt
;; maybe interact w/ and *edit* calendar event

(defvar calfw-ical-sources nil)
(use-package calfw-ical
  :straight t
  :after calfw
  :secret (gcal-open-events gcal-work gcal-michele)
  :config
  (setq calfw-ical-sources
	(list
	  (cfw:ical-create-source "open" gcal-open-events "blue")
	  (cfw:ical-create-source "work" gcal-work "purple")
	  ;(cfw:ical-create-source "mRhee" gcal-michele "gold")
	  ))
  )

(defvar calfw-org-sources nil)

(use-package calfw-org
  :straight (calfw-org :type git :flavor melpa :files ("calfw-org.el" "calfw-org-pkg.el") :host github :repo "kiwanami/emacs-calfw")
  :after calfw
  :config
  (setq calfw-org-sources (list (cfw:org-create-source "Green"))))

;; No need for howm-mode; org-mode + roam for me
;; (straight-use-package 'calfw-howm)

(defun cfw:my-open-calendar ()
  "Display a calfw calendar of my personal calendar."
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (append calfw-ical-sources calfw-org-sources calfw-diary-sources)
   :view 'block-week
   ))

(straight-use-package 'cbm) ;; cycle by major mode

(use-package cdlatex
  :straight t
  :delight
  :hook
  ;; with Emacs latex mode, then with AUCTeX LaTeX mode
  (LaTeX-mode . turn-on-cdlatex))

(straight-use-package 'cl-lib) ;; Properly prefixed CL functions and macros

(use-package clang-format
  :straight t
  :bind ([C-M-tab] . clang-format-region))

(use-package comment-dwim-2 ;; A replacement for the emacs' built-in command comment-dwim
  :straight t
  :bind (("M-;" . comment-dwim-2)))

(straight-use-package 'company-mode)

(use-package company-fuzzy
  :straight t
  :hook company-mode
  ;; :init (setq company-fuzzy-sorting-backend 'flx
  ;; 			  company-fuzzy-prefix-on-top nil
  ;; 			  company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@"))
  :delight " 🏭"
  :custom
  ; (company-fuzzy-sorting-backend 'flx) ; https://github.com/PythonNut/company-flx
  (company-fuzzy-prefix-on-top nil)
  (company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@"))
  (global-company-fuzzy-mode t))

(straight-use-package 'company-coq)
(straight-use-package 'company-dict)

;; TODO Ensure system package lean
(straight-use-package 'lean-mode)

(use-package company-lean
  :after (company lean company-try-hard)
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
  :bind (("C-z" . company-try-hard)))

(straight-use-package 'company-fuzzy)
(straight-use-package 'consult) ;; the counsel equivalent for vertico
(straight-use-package 'coq-commenter)

(use-package crux ;; collection of emacs extensions
  :straight t
  :demand t
  :bind (("C-c C-n C-s" . crux-create-scratch-buffer)))

(straight-use-package 'dash) ;; A modern list library for Emacs

;; Package dash-functional is obsolete; use dash 2.18.0 instead
;; (straight-use-package 'dash-functional)

(straight-use-package 'dirvish)

(straight-use-package '(dired-hacks-utils :host github :repo "Fuco1/dired-hacks" :fork t))

;; Disable dired-collapse, b/c it makes it difficult to	move things around from dired.
;; (use-package dired-collapse
;;   :straight (:host github :repo "Fuco1/dired-hacks" :fork t) ;; This is now correct
;;   :hook dired-mode)

(straight-use-package 'bookmark+)

(use-package dired+
  :straight t
  :custom (diredp-hide-details-initially-flag nil))

;; (straight-use-package 'discover) ;; discover more of Emacs. Sadly, moribund

(straight-use-package 'discover-my-major) ;; Discover key bindings and their meaning for the current Emacs major mode

(use-package dr-racket-like-unicode
  :straight t
  :hook
  ((racket-mode racket-repl) . racket-unicode-input-method-enable)) ;;  scribble-mode deprecated

(straight-use-package 'clean-aindent-mode) ;; Emacs extension for simple indent and unindent
(straight-use-package 'dtrt-indent) ;; A minor mode that guesses the indentation offset originally used for creating source code

(straight-use-package 'duplicate-thing) ;; duplicate current line
(straight-use-package 'easy-jekyll)

(use-package eat
  :straight t)

(use-package ebib
  :straight t
  :bind ("\C-ce" . ebib)
  :custom
   (ebib-bibtex-dialect 'biblatex)) ;; ebib mode, for latex

;; TODO change from scryer-prolog to configurable
;; Make scryer-prolog also a configuration setup
;; "pushd ~/Code/scryer-prolog; git pull --force; cargo build --release"
;; TODO: where  do custom commands get locally executed?
;; So, e.g cargo install such and such for scryer-prolog?
;; What about npl aka nprolog?
;; Yap prolog?
(use-package ediprolog
  :ensure-system-package
    ((gprolog . gnu-prolog)
	 (swipl . swi-prolog))
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

; Too difficult to set up
;; (use-package emacspeak)


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
;;  :ensure-system-package proselint
  :straight t
  ;; Commented because this way drops important data.
  ;; :delight " F✓"
  ;; :config (global-flycheck-mode +1)
  ;;  Consider as a fix to flycheck-mode, see https://github.com/flycheck/flycheck/issues/153#issuecomment-19450255
  :custom (flycheck-highlighting-mode 'lines)
		  (flycheck-check-syntax-automatically '(save idle-change mode-enabled) nil nil "flycheck was a time-hog w/Racket mode, so I disabled newline check & delayed to 4sec")
		  (flycheck-idle-change-delay 4)
		  ;; (flyspell-issue-welcome-flag nil)
		  (global-flycheck-mode t)
  :config
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
	(add-to-list 'flycheck-checkers 'proselint)
	(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
	(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
	(global-set-key (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word)

	(defun flyspell-check-next-highlighted-word ()
	  "Custom function to spell check next highlighted word."
	  (interactive)
	  (flyspell-goto-next-error)
	  (ispell-word))

   (global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)

)

(straight-use-package '(flycheck-textlint :type git :host github :repo "kisaragi-hiu/flycheck-textlint"))

;; (use-package gradle-mode ;; I should want maven, I think, tbqh
;;   :straight t
;;   :hook java-mode)
;;
;; (straight-use-package 'flycheck-gradle)

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
  :custom (flyspell-lazy-mode t))

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
(use-package all-the-icons-ibuffer
  :straight t
  :delight
  :if (display-graphic-p)
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode)
        (ibuffer-mode . ibuffer-auto-mode))

(straight-use-package 'ibuffer-vc) ;; Let Emacs' ibuffer-mode group files by git project etc., and show file state

;; (straight-use-package 'idris-mode) ;; We strictly prefer idris2-mode
(use-package idris2-mode
  :straight (:host github :repo "idris-community/idris2-mode" :files ("*.el" "*.png" "Makefile"))
  :ensure-system-package idris2
  :config (keymap-unset idris2-mode-map "C-c C-c") ;; default idris2-case-dwim clobbers too much
  :hook (idris2-mode . (lambda ()
						 (setq smartparens-global-mode nil)
						 (setq smartparens-mode nil)
						 (setq smartparens-strict-mode nil)
						 (setq wc-mode nil)
						 (setq idris2-load-packages '("prelude" "base" "contrib"))))
  :bind (:map idris2-mode-map
			  ("C-c c"       . idris2-case-dwim)
			  ("C-c C-j"     . idris2-jump-to-def)
			  ("C-c C-c C-a" . copilot-accept-completion)
			  ("C-c C-c C-c" . copilot-current-completion)
			  ("C-c C-c C-n" . copilot-next-completion)
			  ("C-c C-c C-p" . copilot-previous-completion)))

;; Not needed, b/c I was using helm. Now I'm using vertico instead.
;; (straight-use-package 'ido-vertical-mode) ;; makes ido-mode display vertically
(straight-use-package 'iedit) ;; Emacs minor mode and allows you to edit one occurrence of some text in a buffer
(straight-use-package 'info+) ;; Package that enhances some info menus

;; (straight-use-package 'init-loader) ;; No, b/c el-init is better for split init files

;; COMMENTED b/c j incorrectly installed right now. needs fixing
;; (use-package j-mode
;;  :straight t
;;  :mode ("\\.ij[rstp]$" . j-mode))

;; This fork was more up-to-date than the o.g. janet-mode repo.
(straight-use-package '(janet-mode :fork (:host github :repo "pierre-rouleau/janet-mode")))

;; Unneeded
;; (straight-use-package 'jeison)

(straight-use-package 'jump) ;; build functions which contextually jump between files

;; (use-package khoj
;;   :after org
;;   :straight (khoj :type git :host github :repo "khoj-ai/khoj" :files (:defaults "src/interface/emacs/khoj.el"))
;;   :secret khoj-openai-api-key
;;   ;; :bind ("C-c s" . 'khoj) Interferes w/idris2-mode
;;   :config (setq khoj-org-directories '("~/docs/org-roam" "~/docs/notes")
;;                 khoj-org-files '("~/docs/todo.org" "~/docs/work.org")
;;                 khoj-org-agenda-files '("~/docs/todo.org" "~/docs/work.org")))

(use-package latex-unicode-math-mode
  :straight t
  :hook LaTeX-mode)

;; https://www.emacswiki.org/emacs/LibraryDependencies
(straight-use-package 'loadhist)
;; straight-use-package lib-requires, elisp-depend, exl

(use-package magit
  :straight t
  :ensure-system-package
    ((github . github)
	 (git . git)
	 (gitk . git-gui))
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

(straight-use-package 'minimap)

(use-package mindstream
  :after racket-mode
  :straight
  (mindstream :type git :host github :repo "countvajhula/mindstream")
  :config
  (mindstream-initialize))

(use-package multiple-cursors
  :straight t
  :custom (multiple-cursors-mode t)
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

(straight-use-package 'nov.el)

(straight-use-package 'mustache)
(straight-use-package 'neotree) ;; A emacs tree plugin like NerdTree for Vim.
;; (straight-use-package 'nlinum) %% with emacs 26 built-in line numbering, not wanted

;; Actually somewhat annoying in practice, at least in the default configuration.
;; Also IMO, copilot does a better completion job.
;; (use-package org-ac
;;   :straight t
;;   :config (org-ac/config-default))

(use-package org-bullets
  :straight t
  :hook ((org-mode org-roam-mode) . org-bullets-mode))

;; org-dropbox-mode starts up a daemon to sync org-notes via dropbox.
;; Not using, possibly deprecated in favor of other solutions
;; (straight-use-package '(org-dropbox :hook org-mode))

;; Don't want org-doing. An old, one-off

;; Don't want org-dotemacs
;; I instead want to develop a literate org-mode file that I can disentangle to a .el.


(use-package org-inline-pdf
  :after org-mode
  :straight t
  :hook org-mode)

(use-package org-jekyll
  :straight t)

;; :hook org-mode causes an infinite loop.
(use-package org-journal
  :straight t)

;; org-lms, once we have canvas---worth looking into

(use-package org-noter
  :after (pdf-tools nov djvu)
  :straight t)

;; Lets you write queries for org-agenda like views
(straight-use-package 'org-ql)

;; See (org-ref-manual) for some documentation
;; Unclear if I actually _want_ this system, or if I'll prefer the built-in org-cite behavior.
(use-package org-ref ;; Org-ref
  :after (org ox-pandoc)
  :straight t
  :config
  (setq org-ref-default-bibliography '("~/old-microKanrenbib.bib"))
  ;; (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
  )
;; Set up bibliography



(use-package org-roam-bibtex ;; Org-roam-bibtex
  :straight t
  :custom (org-roam-bibtex-mode t)
  :bind (:map org-roam-bibtex-mode-map
			  ("C-c n a" . orb-note-actions))
  :hook org-roam)

;; (straight-use-package 'org-roam-server) defunct, org-roam-ui is the good one
(use-package org-rtm
  :after org
  :straight t)

(use-package org-sidebar
  :after org
  :straight t)

;; Complains about agenda file missing
;; Lets you group items in agenda by tags
(use-package org-super-agenda
  :after org-mode
  :straight t
  :custom
  (org-super-agenda-groups nil)
  :hook org-agenda-mode)

;; TODO: org-trello-mode, when I load it, seems to change how the
;; org-mode files indent and breaks tab-through org-mode behavior.
;; https://github.com/org-trello/org-trello/issues/418#issuecomment-1462483881
;;
;; renatofdds's fork seems to have a fix w/ a PR w/possibly a solution.
;;
;; (use-package org-trello
;;   :straight (:build (:not compile))
;;   :custom (org-trello-current-prefix-keybinding "C-c o")
;;   :hook org-mode)

(use-package org2web
  :after org-mode
  :straight t)

(use-package ox-gfm  ;; for el2org, instead of ox-md fallback cf https://github.com/tumashu/el2org
  :straight t
  :after org-mode)

(use-package ox-jekyll-md
  :after org-mode
  :straight t)

(use-package ox-pandoc
  :after org-mode
  :straight t)

(use-package org-transclusion
  :after org-mode
  :straight t
  :hook org-mode
  :config
  (setq org-transclusion-include-first-section t)
  (setq org-transclusion-include-last-section t)
  (:bind (:map org-mode-map
		 ("<f12>" . org-transclusion-add))))


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

(straight-use-package 'powershell-mode)

(use-package pdf-tools
  :straight t
  :ensure t
;;  :after (pdf-annot fullframe)
  :magic ("%PDF" . pdf-view-mode)
  :config (pdf-tools-install :no-query) ;; if this slows things down try (pdf-loader-install)
  ;; Setup in .bash_profile. Next time I'm on a new machine, I'll try to see what this was checking for and try to auto-install w/brew and add a message to the user instead
  ;; (setenv "PKG_CONFIG_PATH" (concat (getenv "PKG_CONFIG_PATH") ":/opt/homebrew/Cellar/poppler/23.01.0/lib/pkgconfig:/opt/homebrew/lib/pkgconfig:/opt/X11/lib/pkgconfig/:/opt/homebrew/Cellar/poppler/23.01.0/lib/pkgconfig:/opt/X11/share/pkgconfig"))
  (pdf-tools-install)
  :custom (pdf-tools-handle-upgrades t)
  :bind (:map pdf-view-mode-map
		 ("h"   . 'pdf-annot-add-highlight-markup-annotation)
		 ("t"   . 'pdf-annot-add-text-annotation)
		 ("D"   . 'pdf-annot-delete)
		 ("C-s" . 'isearch-forward)
         ;; ("m"   . 'mrb/mailfile)
		 :map pdf-annot-edit-contents-minor-mode-map
         ("<return>"   . 'pdf-annot-edit-contents-commit)
         ("<S-return>" .  'newline)))

;; https://www.emacswiki.org/emacs/LineNumbers#h5o-1
;; Plus the eval-after-load to make sure that it happens in the right sequence.
(eval-after-load "display-line-numbers"
  '(progn
	 (defcustom display-line-numbers-exempt-modes
	   '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode doc-view-mode pdf-view-mode image-mode circe-mode erc-mode compilation-mode dired-mode pdf-annot-list-mode image-dired-mode pdf-outline-buffer-mode occur-mode grep-mode git-rebase-mode magit-mode magit-popup-mode help-mode Info-mode Man-mode)
	   "Major modes on which to disable line numbers."
	   :group 'display-line-numbers
	   :type 'list
	   :version "green")

	 (defun display-line-numbers--turn-on ()
	   "Turn on line numbers except for certain major modes. Exempt major modes are defined in `display-line-numbers-exempt-modes'."
	   (unless (or (minibufferp)
				   (member major-mode display-line-numbers-exempt-modes))
		 (display-line-numbers-mode)))))

;; Visual Popup Interface Library for Emacs
;; (straight-use-package 'popup)
;; probably useful if I'm developing some GUI packages, but I don't see why I need to manually require it. Straight!

;projectile add .projectile to any file
(use-package projectile ;; Project Interaction Library for Emacs
  :straight t
  :custom
  (projectile-sort-order 'recently-active)
  (projectile-completion-system 'auto)
  (projectile-indexing-method 'hybrid) ; 'alien 'native
  (projectile-enable-caching t)
  (projectile-mode t)
  (projectile-mode-line-prefix " Proj")
  (projectile-switch-project-action #'projectile-dired)

  :config
  ;; My own version which ensures we use the split-window-sensibly here,
  ;; no matter what the usual default is.
  (defun projectile-find-file-other-window (&optional invalidate-cache)
	"Jump to a project's file using completion and show it in another window. With a prefix arg INVALIDATE-CACHE invalidates the cache first."
	(interactive "P")
	(let ((split-window-preferred-function-usual split-window-preferred-function))
	  (setq split-window-preferred-function-usual 'split-window-sensibly)
	  (projectile--find-file invalidate-cache #'find-file-other-window)
	  (setq split-window-preferred-function split-window-preferred-function-usual)))
  (projectile-global-mode)
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

(defun racket-insert-prime ()
  "Insert a Unicode prime character."
  (interactive)
  (insert "′"))

(use-package racket-mode
  :straight t
  :ensure-system-package racket
  :bind (:map racket-mode-map ("C-c r" . racket-run))
  :hook
  (racket-mode . racket-xp-mode)
  (racket-mode . racket-smart-open-bracket-mode)
  (racket-mode . (lambda () (define-key racket-mode-map (kbd "C-c C-x C-p") 'racket-insert-prime)))
  (racket-mode . (lambda () (flycheck-mode -1))) ;; disable flycheck in racket b/c Rk✓
  (racket-repl-mode . racket-smart-open-bracket-mode)
  :custom (racket-program "racket")
  (mapc (lambda (pr) (put (car pr) 'racket-indent-function (cdr pr)))
      '((conde . 0)
        (fresh . 1)
        (run . 1)
        (run* . 1)
        (run . 2)
		(letrec . 0)))
  :delight (racket-smart-open-bracket-mode)
           (racket-xp-mode " ✗")
		   (racket-mode " Rkt")
  :mode ("\\.rkt\\'" . racket-mode))

;; Deprecated, b/c Racket mode w/scribble files is better
;;
;; FYI, also alt scribble-mode at
;; https://www.neilvandyke.org/scribble-emacs/scribble.el
;;
;; Moreover, racket now has a hashlang mode.
;;
;; (straight-use-package 'scribble-mode)

(straight-use-package 'reazon)
(straight-use-package 'refine)

(straight-use-package 's) ;; The long lost Emacs string manipulation library.
;; (straight-use-package 'scheme-complete) ;; Unclear if I need it — Asked Alex Shinn

;; SMEX was causing a major slowdown w/my config.
;;
;; (use-package smex
;;   :straight t
;;   :config (smex-initialize) ; Can be omitted. This might cause a (minimal) delay when Smex is auto-initialized on its first run.
;;   :bind (("M-x" . smex)
;; 		 ("M-X" . smex-major-mode-commands)
;; 		 ("C-c C-c M-x" . execute-extended-command)))
;; This is your old M-x.

(straight-use-package 'semi)
(straight-use-package 'sh-script) ;; The major mode for editing Unix and GNU/Linux shell script code
(straight-use-package 'smartscan) ;; Quickly jumps between other symbols found at point in Emacs

(use-package smartparens
  :straight t
  :init ;; https://github.com/Fuco1/smartparens/issues/1088#issuecomment-854714652
  ;; Also mentioned in racket-mode info pages.
  (require 'smartparens-config)
  :config
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  (sp-use-paredit-bindings)
  :hook ((prog-mode text-mode) . turn-on-smartparens-strict-mode)
        ((prog-mode text-mode) . show-smartparens-mode)
  :bind (:map smartparens-mode-map
		 ;; Shadows lisp.el beginning and end of defun; seem useful
		 ;; ("C-M-a" . sp-beginning-of-sexp)
		 ;; ("C-M-e" . sp-end-of-sexp)
		 ;; Need new bindings for these, these conflict/taken.
		 ;;  ("????C-M-n" . sp-next-sexp)
		 ;;  ("????C-M-p" . sp-previous-sexp)
		 ;;  ("????C-M-w" . sp-copy-sexp)---I can live without
		 ("C-S-f" . sp-forward-symbol)
		 ("C-S-b" . sp-backward-symbol)
		 ("C-<left>" . nil)
		 ("C-<right>" . nil)
		 ("C-M-<left>" . nil) ;; These can be used, just should decide what to do
		 ("C-M-<right>" . nil) ;; These can be used, just should decide what to do
		 ([remap kill-sexp] . sp-kill-sexp)
		 ([remap backward-kill-sexp] . sp-backward-kill-sexp)
		 ([remap transpose-lines] . sp-transpose-hybrid-sexp)
		 ([remap forward-sexp] . sp-forward-sexp)
		 ([remap backward-sexp] . sp-backward-sexp)
		 ("M-[" . sp-backward-unwrap-sexp)
		 ("M-]" . sp-unwrap-sexp)))


;; WRAP-REGION PACKAGE SHOULD HELP ME WRITE THESE BETTER
;;  ("C-c ("  . wrap-with-parens)
;;  ("C-c ["  . wrap-with-brackets)
;;  ("C-c {"  . wrap-with-braces)
;;  ("C-c '"  . wrap-with-single-quotes)
;;  ("C-c \"" . wrap-with-double-quotes)
;;  ("C-c _"  . wrap-with-underscores)
;;  ("C-c `"  . wrap-with-back-quotes))

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

;; I think this would be useful for block comments, but I am not sure
;; if this is what this is supposed to be for.
;;
;; EDIT: For things like HTML where you have many paired delimiters.
(use-package wrap-region ;; Emacs minor mode to wrap region with tag or punctuations
  :straight (:host github :repo "rejeep/wrap-region.el" :fork t :files ("*.el" "out"))
  :delight
  :custom
  (wrap-region-global-mode t))


(use-package sml-mode
  :straight t
  :ensure-system-package sml)

(straight-use-package 'sml-modeline)
(straight-use-package 'smog)
(straight-use-package 'sourcemap) ;;  Sourmap parser in Emacs Lisp
(straight-use-package 'sx) ;; Stackoverflow mode ;-)
(straight-use-package 'svg-tag-mode)

;; Sublimity is annoying, the minimap is more annoying than useful.
;; (use-package sublimity
;;   :straight t
;;   :config
;;   (require 'sublimity-scroll)
;; ;;   (require 'sublimity-map)
;; ;;  (require 'sublimity-attractive)

;;   :hook ((prog-mode text-mode) . sublimity-mode))

(straight-use-package 'tabbar)
(straight-use-package 'tramp)
(straight-use-package 'treepy) ;; tree-walk functionality like a clojure library implementation
(straight-use-package 'ts) ;; A bunch of nice utilities for time and date parsing, better than the built-ins

(use-package vterm
  :straight t
  :bind ;; (("C-c t" . vterm)) Disabling b/c it’ll interfere w/some of my org-mode bindings in places
  :custom (vterm-always-compile-module t))

(use-package eshell-vterm
  :straight t
;;  Not clear that I should need these configuration options, b/c dependencies.
;;  :demand t
;;  :after eshell
  :custom
  (eshell-vterm-mode t)
  :config
  (defalias 'eshell/v 'eshell-exec-visual))

(use-package volatile-highlights ;; Minor mode for visual feedback on some operations.
  :straight t
  :delight
  :custom (volatile-highlights-mode t))

;; I don't think I like undo-tree. Weird undo structure.
;; (use-package undo-tree ;; Treat undo history as a tree
;;   :straight t
;;   :config
;;   (global-undo-tree-mode)
;;   BTW Config section assumes volatile highlights
;;   (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
;;   (vhl/install-extension 'undo-tree))

;; Replaces selectrum. Better b/c Prot told me so.
(use-package vertico
  :straight t
  :config (vertico-mode)
          (file-name-shadow-mode)
  ;; This works with `file-name-shadow-mode' enabled.  When you are in
  ;; a sub-directory and use, say, `find-file' to go to your home '~/'
  ;; or root '/' directory, Vertico will clear the old path to keep
  ;; only your current input.
  ;;
  ;; This needs to be an add-hook b/c I want to add the hook to hide path when I type shadow stuff
  ;; But I need to have vertico already loaded so I can access the minibuffer inside of which this hook will run
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy) )


(use-package marginalia
  :straight t
  :config (marginalia-mode))

(use-package savehist
  :straight t
  :config (savehist-mode))




(use-package visual-regexp  ;; A regexp/replace command for Emacs with interactive visual feedback
  :straight t
  :bind (("C-c C-v r" . vr/replace)
         ("C-c C-v q". vr/query-replace)
		 ;; if you use multiple-cursors, this is for you:
		 ("C-c C-v m" . 'vr/mc-mark)))

(straight-use-package 'visual-regexp-steroids) ;; Extends visual-regexp to support other regexp engines

;; This apparently conflicts w/org-mode, need to adjust
;; https://orgmode.org/manual/Conflicts.html
(use-package yasnippet
  :straight t
  :delight
  :custom (yas-global-mode t)
  :config
  (yas-reload-all)
  ;; :bind (;; Each and every one of these bindings interferes with org-mode bindings.
  ;;        ;; ("C-c y" . yas-expand)
  ;;        ;; ("C-c C-y" . yas-insert-snippet)
  ;; 		 ;; ("C-c C-n" . yas-new-snippet)
  ;; 		 ;; ("C-c C-v" . yas-visit-snippet-file)
  ;; 		 ;; ("C-c C-l" . yas-describe-tables)
  ;; 		 ;; ("C-c C-s" . yas-describe-tables)
  ;; 		 ;; ("C-c C-d" . yas-reload-all)
  ;; 		 ;; ("C-c C-r" . yas-reload-all)
  ;; 		 )
  )



(use-package wc-mode
  :straight t
  :bind ("\C-cw" . wc-mode)) ;; Shadowed, need another binding

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

;; ⮐

(use-package writegood-mode
  :after artbollocks-mode
  :straight t
  :delight " 💯"
  :hook text-mode
  ;; Because I want these added to the artbollocks-mode-map, not the global map.
  ;; Note we demand artbollocks-mode, so this should be safe.
  :bind (:map artbollocks-mode-keymap
		 ("C-c M-a 2 g" . writegood-grade-level)
		 ("C-c M-a 2 e" . writegood-reading-ease)))

(use-package ws-butler ;; Unobtrusively trim extraneous white-space *ONLY* in lines edited.
  :straight t
  :custom (ws-butler-global-mode t))

(straight-use-package 'xr) ;; The reverse regex library (regex->Human)
(straight-use-package 'yafolding) ;; Yet another folding extension for Emacs
(straight-use-package 'yaml-mode) ;; The emacs major mode for editing files in the YAML data serialization format.
(straight-use-package 'yaml-pro)  ;; Perhaps an improved version?
(straight-use-package 'zones)

(use-package zygospore
  :straight t
  :bind ("\C-x1" . zygospore-toggle-delete-other-windows))

(use-package all-the-icons
  :straight t
  :config
  :if (display-graphic-p))

  ;; I had had this there to ignore an error but cannot remember why
  ;; (ignore-error end-of-file ; I suspect this is not the error I wanted to raise.
  ;; 	(all-the-icons-install-fonts t))

;; Also, does this need to be graphics-only, like all-the-icons?
;; https://github.com/domtronn/all-the-icons.el#installation
(use-package all-the-icons-dired
  :straight (:host github :repo "wyuenho/all-the-icons-dired")
  :custom (all-the-icons-dired-monochrome nil)
  :if (display-graphic-p)
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

;; Not needed unless I’m back to doing ACL2 work.
;; (if (file-exists-p "~/Documents/acl2/scripts-master/.lisp.el")
;;     (load-file "~/Documents/acl2/scripts-master/.lisp.el"))

(let ((gnu-ls-path (executable-find "gls")))
  (when gnu-ls-path
    (setq insert-directory-program gnu-ls-path)))

(defun insert-look-of-disapproval ()
  "A function to insert the look of disapproval."
  (interactive (insert "ಠ_ಠ")))

(defun insert-shrug ()
  "A function to insert the shrug."
  (interactive (insert "¯\\_(ツ)_/¯")))

(defun insert-caffeine ()
  "A function to insert a caffeine guy."
  (interactive (insert "ᕕ( ᐛ )ᕗ")))

(defun insert-facepalm ()
  "A function to insert a facepalm."
  (interactive (insert "(－‸ლ)")))

(defun insert-fury ()
  "A function to insert a furious face."
  (interactive (insert "!(•̀ᴗ•́)و ̑̑")))

;; C-x 8 S (interactive (insert "§")) ;; section
(global-set-key (kbd "C-c (") 'insert-look-of-disapproval)
(global-set-key (kbd "C-c )") 'insert-shrug)
(global-set-key (kbd "C-c C-x (") 'insert-caffeine)
(global-set-key (kbd "C-c C-x )") 'insert-facepalm)
(global-set-key (kbd "C-c C-x x") 'insert-fury)

(defun prime-it ()
  "A function to add a prime character."
  (interactive (insert "′")))

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
  :ensure-system-package (languagetool . "brew install languagetool --cask")
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
  ;; Not sure that I need to do the w-e-a-l 'langtool, but I might
  ;; need to delay the defun until after we load it, and I’m not sure
  ;; the scope of this defun defined here.
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

(use-package mb-depth
  :ensure nil
  :hook (after-init . minibuffer-depth-indicate-mode)
  :config
  (setq read-minibuffer-restore-windows nil) ; Emacs 28
  (setq enable-recursive-minibuffers t))


;; ※ Suggest to use the reference mark, preceeding, for Reftex.
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
;; (add-hook 'text-mode-hook 'electric-quote-mode)
;; I do not like electric-quote-mode. I will turn it on just when I want it.

;; Modes someone had enabled that I want to investigate.



;; (defconst required-packages
;;   '(
;;     adaptive-wrap
;;     ample-theme
;;     anaphora
;;     auto-highlight-symbol
;;     avy
;;     color-theme-modern
;;     company-quickhelp
;;     csharp-mode
;;     counsel-projectile
;;     dashboard
;;     dim
;;     dired-single
;;     dired-subtree
;;     edit-server
;;     elpy
;;     ethan-wspace
;;     fish-mode
;;     geiser geiser-chez geiser-racket
;;     git-gutter
;;     gmail-message-mode
;;     golden-ratio
;;     graphviz-dot-mode
;;     gruvbox-theme
;;     haskell-mode
;;     hide-lines
;;     highlight-escape-sequences
;;     hl-todo
;;     htmlize
;;     iedit
;;     indent-guide
;;     java-snippets
;;     json-mode
;;     logview
;;     lua-mode
;;     markdown-mode
;;     markdown-preview-mode
;;     minimap
;;     modern-cpp-font-lock
;;     omni-scratch
;;     org-preview-html
;;     origami
;;     pandoc-mode
;;     persistent-scratch
;;     plantuml-mode
;;     protobuf-mode
;;     py-autopep8
;;     pyenv-mode
;;     rainbow-delimiters
;;     rainbow-mode
;;     slime
;;     smart-mode-line
;;     smart-mode-line-powerline-theme
;;     separedit
;;     sx
;;     sr-speedbar
;;     swiper
;;     switch-window
;;     tabbar
;;     trashed
;;     undo-tree
;;     use-package
;;     vlf
;;     vimrc-mode
;;     visual-fill-column
;;     visual-regexp-steroids
;;     websocket
;;     wttrin
;;     xcscope
;;     yasnippet
;;     yasnippet-snippets
;;     ztree
;;     cmake-font-lock
;;     cmake-mode
;;     cmake-project


;;     ))

;;
;; (enabled-minor-modes
;;  (auto-fill-mode)
;;  (auto-save-mode)
;;  (company-tng-mode)
;;  (counsel-mode)
;;  (emojify-mode)
;;  (fira-code-mode)
;;  (global-emojify-mode)
;;  (semantic-minor-modes-format))

;; (disabled-minor-modes

;;  (prettify-symbols-mode) ;; font ligatures, no
;;  (global-ligature-mode)
;;  (global-prettify-symbols-mode)
;;  (ligature-mode)
;;  (visual-line-mode)
;;  (global-visual-line-mode)
;;
;;  (electric-layout-mode) ;; automatically set up some fancy newline stuff. Nah.
;;  (size-indication-mode) ;; how big is the buffer
;;  (ispell-minor-mode) ;; I already have flyspell-mode, which does more
;;  (horizontal-scroll-bar-mode)

;;  (archive-subfile-mode)

;;  (auto-fill-function)
;;  (auto-package-update-minor-mode)
;;  (auto-save-visited-mode)
;;  (avy-linum-mode)
;;  (cl-old-struct-compat-mode)
;;  (compilation-minor-mode)
;;  (compilation-shell-minor-mode)
;;  (completion-in-region-mode)
;;  (defining-kbd-macro)
;;  (delete-selection-mode)
;;  (diff-auto-refine-mode)
;;  (diff-minor-mode)
;;  (dired-hide-details-mode)

;;  (emojify-debug-mode)
;;  (emojify-mode-line-mode)
;;  (general-override-local-mode)
;;  (general-override-mode)

;;  (global-dash-fontify-mode)
;;  (global-emojify-mode-line-mode)
;;  (global-fira-code-mode)
;;  (global-reveal-mode)
;;  (global-semantic-highlight-edits-mode)
;;  (global-semantic-highlight-func-mode)
;;  (global-semantic-show-parser-state-mode)
;;  (global-semantic-show-unmatched-syntax-mode)
;;  (global-semantic-stickyfunc-mode)


;;  (hs-minor-mode)
;;  (ido-everywhere)
;;  (image-minor-mode)
;;  (isearch-mode)

;;  (jit-lock-debug-mode)
;;  (menu-bar-mode)
;;  (next-error-follow-minor-mode)
;;  (org-list-checkbox-radio-mode)
;;  (org-src-mode)
;;  (org-table-follow-field-mode)
;;  (org-table-header-line-mode)
;;  (orgtbl-mode)

;;  (overwrite-mode)
;;  (paragraph-indent-minor-mode)
;;  (rectangle-mark-mode)
;;  (reveal-mode)
;;  (semantic-highlight-edits-mode)
;;  (semantic-highlight-func-mode)
;;  (semantic-mode)
;;  (semantic-show-parser-state-mode)
;;  (semantic-show-unmatched-syntax-mode)
;;  (semantic-stickyfunc-mode)
;;  (sh-electric-here-document-mode)

;;  (outline-minor-mode)

;;  (tab-bar-history-mode)
;;  (tab-bar-mode)
;;  (tar-subfile-mode)
;;  (temp-buffer-resize-mode)
;;  (text-scale-mode) ;; show the text scale adjustment in the modeline
;;  (tool-bar-mode)
;;  (tooltip-mode)

;;  (url-handler-mode)
;;  (use-hard-newlines)
;;  (vc-parent-buffer)
;;  (view-mode)
;;  (visible-mode)

;;  (window-divider-mode)
;;  (xref-etags-mode))

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

;; (add-hook 'ciao-mode-hook #'enable-paredit-mode) ;; not til fix paren space issue.
;; tex-mode has paredit-mode issue too.

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
 '(TeX-font-list
   '((2 "{\\bf " "}")
	 (3 "{\\sc " "}")
	 (5 "{\\em " "\\/}")
	 (9 "{\\it " "\\/}")
	 (18 "{\\rm " "}")
	 (19 "{\\sl " "\\/}")
	 (20 "{\\tt " "}")
	 (4 "" "" t)
	 (17 "\\enquote{" "}")))
 '(ad-redefinition-action 'accept)
 '(apropos-sort-by-scores t)
 '(auto-save-interval 75)
 '(auto-save-timeout 75)
 '(bib-cite-use-reftex-view-crossref t t)
 '(bibtex-maintain-sorted-entries 'plain)
 '(blink-cursor-mode nil)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(bookmark-save-flag 0)
 '(calendar-mark-diary-entries-flag t)
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(compilation-scroll-output 'first-error)
 '(custom-safe-themes t)
 '(debug-on-error t)
 '(default-input-method "TeX")
 '(describe-bindings-outline t)
 '(dired-dwim-target 'dired-dwim-target-recent nil nil "Not obvious which option is *best*, but this at least works when I put two direds side-by-side.")
 '(dired-listing-switches "-alGh1v --group-directories-first --time-style=long-iso" nil nil "long format, w/hidden files, w/o group information, w/good numeric sorting human-readable sizes, and w/directories first")
 '(dired-recursive-copies 'always nil nil "I shouldn't be prompted to recursively copy dirs")
 '(enable-local-eval t)
 '(enable-local-variables t)
 '(find-file-visit-truename t)
 '(flyspell-lazy-mode t nil nil "Customized with use-package flyspell-lazy")
 '(frame-inhibit-implied-resize t)
 '(frame-resize-pixelwise t)
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
 '(org-M-RET-may-split-line '((default)))
 '(org-attach-method 'lns)
 '(org-priority-default 65)
 '(org-trello-current-prefix-keybinding "C-c o" nil nil "Customized with use-package org-trello")
 '(org-use-fast-todo-selection 'expert)
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
 '(reftex-default-bibliography '("~/old-microKanren.bib"))
 '(reftex-extra-bindings t)
 '(reftex-plug-into-AUCTeX t)
 '(require-final-newline t nil nil "Add an EOL to files when I save them.")
 '(revert-without-query '("'(\".*\")"))
 '(ring-bell-function 'ignore)
 '(safe-local-eval-forms
   '((add-hook 'write-file-hooks 'time-stamp)
	 (add-hook 'write-file-functions 'time-stamp)
	 (add-hook 'before-save-hook 'time-stamp nil t)
	 (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))
 '(safe-local-variable-values
   '((flycheck-mode . 0)
	 (writegood-mode)
	 (artbollocks-mode)
	 (flyspell-mode)
	 (writegood-mode . 0)
	 (artbollocks-mode . 0)
	 (flyspell-mode . 0)
	 (idris2-load-packages "base" "contrib")
	 (idris2-load-packages "prelude" "base" "contrib")
	 (TeX-command-extra-options . "-shell-escape")
	 (calc-float-format quote
						(fix 2))
	 (org-table-copy-increment)
	 (visual-line-mode)
	 (TeX-command-extra-options . "--synctex=1 --shell-escape")
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
 '(scroll-bar-mode nil)
 '(select-enable-clipboard t)
 '(sentence-end-double-space nil)
 '(show-trailing-whitespace t)
 '(sort-fold-case t nil nil "Make buffer-sort functions case-insensitive")
 '(straight-use-package-by-default t)
 '(tab-always-indent 'complete)
 '(tab-width 4 nil nil "Switching to a 4-space tab")
 '(tool-bar-mode nil)
 '(trash-directory "~/.Trash")
 '(truncate-lines t)
 '(use-short-answers t)
 '(vc-follow-symlinks t)
 '(vc-make-backup-files t)
 '(version-control t)
 '(view-read-only t)
 '(visible-bell t)
 '(warning-suppress-types '((comp)))
 '(window-combination-resize t))

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

(defun dynamic-name-from-buffer (options backend)
  "Set title dynamically for export any OPTIONS via any BACKEND."
  (plist-put options :title (buffer-name)))

;; (defun my/org-percent-to-letter-grade (percent)
;;   "Convert PERCENT to letter grade."
;;   (cond ((>= percent 90) "A")
;;         ((>= percent 80) "B")
;;         ((>= percent 70) "C")
;;         ((>= percent 60) "D")
;;         (t "F")))

;; (defun my/org-update-statistics-cookies (a b)
;;   "Update statistics cookies according to values and weights of checkboxes."
;;   ;; Get current headline level and point position.
;;   (let ((level (-max-item '(l . nil) (-map #'car (-filter #'cdr org-outline-regexp-bol-cache))))
;; 		(pos (-max-item '(l . nil) (-map #'car (-filter #'cdr org-outline-regexp-bol-cache)))))
;; 	;; Go through each direct child headline.
;; 	(--each (-filter #'cdr (--mapcat (-zip-pair it (--split-with (> level it) (--drop-while (< posit) (--map #'car org-outline-regexp-bol-cache)))) org-outline-regexp-bol-cache))
;; 	  ;; Get value and weight properties of each checkbox item under the headline.
;; 	  (--each (-filter #'cdr (--mapcat (-zip-pair it (--split-with (> level (+ level it))
;; 																   (--drop-while (< pos (+posit)) (--map #'car org-outline-regexp-bol-cache))))
;; 									   org-outline-regexp-bol-cache))
;; 		(let ((value-property-name "value")
;; 			  value-property-value
;; 			  weight-property-name
;; 			  weight-property-value
;; 			  total-value
;; 			  total-weight
;; 			  average-value
;; 			  statistics-cookie-start
;; 			  statistics-cookie-end
;; 			  statistics-cookie-format
;; 			  statistics-cookie-value
;; 			  new-statistics-cookie-value
;; 			  new-statistics-cookie-string
;; 			  old-point-position)
;; 		  ;; Get value property name and value.
;; 		  ;;
;; 		  ;; If there is no such property for current checkbox item,
;; 		  ;; use default name ("value") and default value (1).
;; 		  (setq value-property-name
;; 				(or (org-entry-get nil "VALUE_PROPERTY_NAME") "value"))
;; 		  (setq value-property-value
;; 				(string-to-number
;; 				 (or (org-entry-get nil (upcase value-property-name)) "1")))
;; 		  ;; Get weight property name and value.
;; 		  ;;
;; 		  ;; If there is no such property for current checkbox item,
;; 		  ;; use default name ("weight") and default value (1).
;; 		  (setq weight-property-name
;; 				(or (org-entry-get nil "WEIGHT_PROPERTY_NAME") "weight"))
;; 		  (setq weight-property-value
;; 				(string-to-number
;; 				 (or (org-entry-get nil (upcase weight-property-name)) "1")))
;; 		  ;; Update total value by adding current checkbox's weighted value,
;; 		  ;; which equals its own value times its own weight.
;; 		  (cl-incf total-value (* value-property-value weight-property-value))
;; 		  ;; Update total weight by adding current checkbox's own weight.
;; 		  (cl-incf total-weight weight-property-value)))
;; 	  ;; Calculate average value by dividing total weighted sum by total weight sum.
;; 	  (setq average-value (/ total-value total-weight))
;; 	  ;; Find where statistic cookie starts/ends for current headline,
;; 	  ;; get its format and value.
;; 	  (save-excursion
;; 		(goto-char posit)
;; 		(re-search-forward "\\[[0-9]+%\\(\\|/[0-9]+\\)\\]" nil t)
;; 		(setq statistics-cookie-start (match-beginning 0))
;; 		(setq statistics-cookie-end (match-end 0))
;; 		(setq statistics-cookie-format
;; 			  (buffer-substring-no-properties statistics-cookie-start statistics-cookie-end))
;; 		(string-match "\\([0-9]+\\)%\\(\\|/\\([0-9]+\\)\\)" statistics-cookie-format)
;; 		(setq statistics-cookie-value
;; 			  (string-to-number (match-string 1 statistics-cookie-format)))
;; 		(if (match-string 3 statistics-cookie-format)
;; 			(setq total-checkboxes
;; 				  (string-to-number (match-string 3 statistics-cookie-format)))))
;; 	  ;; Calculate new statistic cookie value based on average value,
;; 	  ;; round it to integer.
;; 	  (setq new-statistics-cookie-value	(round average-value))
;; 	  ;; If new statistic cookie value is different than old one,
;; 	  ;; update it in buffer.
;; 	  (unless (= new-statistics-cookie-value statistics-cookie-value)
;; 		;; Format new statistic cookie string based on old format.
;; 		(if total-checkboxes
;; 			(setq new-statistics-cookie-string
;; 				  (format "[%d/%d]" new-statistics-cookie-value total-checkboxes))
;; 		  (setq new-statistics-cookie-string
;; 				(format "[%d%%]" new-statistics-cookie-value)))
;; 		;; Replace old statistic cookie string with new one in buffer.
;; 		(save-excursion
;; 		  (goto-char posit)
;; 		  (delete-region statistics-cookie-start statistics-cookie-end)
;; 		  (insert new-statistics-cookie-string))))))

;; (add-hook 'org-after-todo-statistics-hook 'my/org-update-statistics-cookies)


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
 '(font-lock-function-name-face ((t (:foreground "#385e6b" :weight bold))))
 '(mode-line ((t (:background "#f0f0f1" :box (:line-width (1 . 1) :color "#383a42") :height 0.6)))))



(require 'org-protocol)
;; (require 'org-roam-protocol) ;; b/c busted when I reloaded

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

;; Presumes geiser-mode, at least by default.
;; Documentation shows how to add racket-mode
;; However issue says there is a bug, expecting an old racket-mode function
;; Documentation shows how to add prolog support
;; (use-package org-babel-eval-in-repl
;;   :straight t
;;   :bind (:map org-mode-map
;; 		 ("C-<return>" . 'ober-eval-in-repl)
;; 		 ("M-<return>" . 'ober-eval-block-in-repl)))

(use-package ob-racket
  :after org
  :straight (ob-racket :type git :host github :repo "hasu/emacs-ob-racket" :files ("*.el" "*.rkt"))
  :demand t
  :hook (ob-racket-pre-runtime-library-load . ob-racket-raco-make-runtime-library)
  :config (add-to-list 'org-babel-load-languages '(racket . t))
  (setq org-babel-command:racket (executable-find "racket"))
;;   :straight (:type git :host github :repo "togakangaroo/ob-racket" :files ("*.el" "*.rkt"))

)

(setq-default major-mode 'text-mode)

(defvar my-theme-loaded nil "Flag to indicate if a theme has been loaded.")
;; Pick a random theme.

;; Good themes leuven-dark modus-vivendi
(unless my-theme-loaded
  (let* ((excluded-themes '(light-blue tsdh-dark modus-vivendi wombat leuven))
         (available-themes (cl-remove-if (lambda (theme)
                                           (member theme excluded-themes))
                                         (custom-available-themes)))
         (random-theme (nth (random (length available-themes)) available-themes)))
    (enable-theme random-theme t)
    (setq my-theme-loaded t)))

(defun describe-current-themes ()
  "Display the current enabled themes in the modeline."
  (interactive "")
  (message "%S" custom-enabled-themes))



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

;; Try to run org-roam-ui-mode, but A-okay if socket is taken
;; (ignore-error file-error
;;   (org-roam-ui-mode))

(define-key global-map "\C-c\C-c" nil)
(provide 'init.el)
;;; init.el ends here
