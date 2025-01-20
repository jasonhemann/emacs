;; Note packages I have removed because they are out of date or strictly less useful than an alternative, or that I have tried and decided I did not like. Packages that I have sunset.

;; Not covered here are packages that are removed because I have misconfigured or haven't correctly configured, or buggy, or conflict with other existing packages.


;; Commenting b/c the agda2-mode seems to come with agda installation
;; itself w/its own agda
;;
;; (use-package agda2-mode
;;   :straight (:includes (eri annotation))
;;   :ensure-system-package agda
;;   :mode (("\\.agda\\'" . agda2-mode)
;; 		 ("\\.lagda.md\\'" . agda2-mode)
;; 		 ("\\.lagda\\'" . agda2-mode)))



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

;; fontawesome is abandonware


;; All eclim abandoned and better use java-lsp.
;; Skip company-emacs-eclim ac-emacs-eclim too.

;; No need to use these, as flycheck is better
;; (straight-use-package 'flylisp) ;; Add highlighting to mismatched parentheses, so you can see the mistake
;; (straight-use-package 'flymake-easy)
;; (straight-use-package 'flymake-gradle)
;; (straight-use-package 'flymake-racket)
;; (straight-use-package 'flymd) No longer works w/FF >= 68

;; Themes from specific packages. I have enough; don't think I need these.
;; (straight-use-package 'color-theme-modern)
;; (straight-use-package 'cyberpunk-theme)
;; (straight-use-package 'gotham-theme)
;; (straight-use-package 'green-phosphor-theme)
;; (straight-use-package 'hc-zenburn-theme)
;; (straight-use-package 'solarized-emacs)

;; (straight-use-package 'idris-mode) ;; We strictly prefer idris2-mode
;; (use-package idris-mode
;;   :straight (:host github :repo "idris-hackers/idris-mode" :files ("*.el" "*.png" "Makefile"))
;;   :ensure-system-package idris2
;;   :config (keymap-unset idris-mode-map "C-c C-c") ;; default idris2-case-dwim clobbers too much
;;   :hook (idris2-mode . (lambda ()
;; 						 (setq smartparens-global-mode nil)
;; 						 (setq smartparens-mode nil)
;; 						 (setq smartparens-strict-mode nil)
;; 						 (setq wc-mode nil)
;; 						 (setq idris-load-packages '("prelude" "base" "contrib"))))
;;   :bind (:map idris-mode-map
;; 			  ("C-c c"       . idris-case-dwim)
;; 			  ("C-c C-j"     . idris-jump-to-def)
;; 			  ("C-c C-c C-a" . copilot-accept-completion)
;; 			  ("C-c C-c C-c" . copilot-current-completion)
;; 			  ("C-c C-c C-n" . copilot-next-completion)
;; 			  ("C-c C-c C-p" . copilot-previous-completion)))


;; (straight-use-package 'nlinum) %% with emacs 26 built-in line numbering, not wanted

;; Actually somewhat annoying in practice, at least in the default configuration.
;; Also IMO, copilot does a better completion job.
;; (use-package org-ac
;;   :straight t
;;   :config (org-ac/config-default))

;; Disabled because it once upon a time led to eating 100% CPU to fontify
;; (use-package org-bullets
;;   :straight t
;;   :hook ((org-mode org-roam-mode) . org-bullets-mode))


;; org-dropbox-mode starts up a daemon to sync org-notes via dropbox.
;; Not using, possibly deprecated in favor of other solutions
;; (straight-use-package '(org-dropbox :hook org-mode))

;; Don't want org-doing. An old, one-off

;; Don't want org-dotemacs
;; I instead want to develop a literate org-mode file that I can disentangle to a .el.


;; Ultimately, I wasn't using trello, so skipped
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

;; Smartparens is now the successor to paredit:
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

;; SMEX (smart M-x) was causing a major slowdown w/my config.
;;
;; (use-package smex
;;   :straight t
;;   :config (smex-initialize) ; Can be omitted. This might cause a (minimal) delay when Smex is auto-initialized on its first run.
;;   :bind (("M-x" . smex)
;; 		 ("M-X" . smex-major-mode-commands)
;; 		 ("C-c C-c M-x" . execute-extended-command)))
;; This is your old M-x.


;; I don't think I like undo-tree. Weird undo structure.
;; (use-package undo-tree ;; Treat undo history as a tree
;;   :straight t
;;   :config
;;   (global-undo-tree-mode)
;;   BTW Config section assumes volatile highlights
;;   (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
;;   (vhl/install-extension 'undo-tree))


;; Sublimity is annoying, the minimap is more annoying than useful.
;; (use-package sublimity
;;   :straight t
;;   :config
;;   (require 'sublimity-scroll)
;; ;;   (require 'sublimity-map)
;; ;;  (require 'sublimity-attractive)

;;   :hook ((prog-mode text-mode) . sublimity-mode))


;; vertico now replaces selectrum. Better b/c Prot told me so.

;; relies on phantomjs, which is discontinued upstream
;; (straight-use-package 'ob-browser)
