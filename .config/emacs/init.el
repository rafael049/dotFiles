(setq inhibit-startup-message t)  ; Disable startup page
(setq gc-cons-threshold 100000000) ; Set when GC is called (100MB)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
			   (package-refresh-contents))

(unless (package-installed-p 'use-package)
			   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(scroll-bar-mode -1)   ; Disable scrollbar
(tool-bar-mode -1)     ; Disable toolbar
(tooltip-mode -1)      ; Disable tooltips
(set-fringe-mode 10)   ; Give some breathing room
(menu-bar-mode -1)     ; Disable the menu bar

(add-to-list 'default-frame-alist '(font . "Roboto Mono-11"))

(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package doom-themes
:init (load-theme 'doom-palenight t)
:config
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t)) ; if nil, italics is universally disabled

(use-package smart-mode-line)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(setq user-full-name "Rafael Moraes"
      user-mail-address "rafael1.618@outlook.com")

; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-j"))

(setq org-confirm-babel-evaluate nil)

(defun my/org-font-setup ()
;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
			'(("^ *\\([-]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Set faces for heading levels
(dolist (face '((org-level-1 . 1.5)
		(org-level-2 . 1.3)
		(org-level-3 . 1.15)
		(org-level-4 . 1.1)
		(org-level-5 . 1.1)
		(org-level-6 . 1.1)
		(org-level-7 . 1.1)
		(org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Liberation Serif" :weight 'regular :height (cdr face)))
;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun my/org-mode-setup ()
(org-indent-mode)
(variable-pitch-mode 1)
(visual-line-mode 1)
(setq org-file-apps '(("\\.pdf\\'" . emacs)) ) )

(use-package org
  :hook (org-mode . my/org-mode-setup)
  :config
  (setq org-latex-pdf-process (list
			       "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))
  (add-to-list 'org-latex-packages-alist
	       '("AUTO" "babel" t ("pdflatex")))
  (setq org-ellipsis " ⯆"
	org-hide-emphasis-markers t)
  (my/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package org-bullets
:after org
:hook (org-mode . org-bullets-mode)
:custom
(org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(defun my/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/.config/emacs/emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'my/org-babel-tangle-config)))

;; Enable code evaluation on Org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil)
   (python . t)))

;; Syntax highlight in org-mode latex exported
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Disable line breaks
(dolist (mode '(prog-mode-hook
                ))
  (add-hook mode (lambda () (auto-fill-mode 0))))
;; Disable line wraping
(set-default 'truncate-lines t)

;; Enable auto-fill-mode in text files
(setq text-mode-hook 'turn-on-auto-fill)

;; Spaces as tabs
(setq indent-tabs-mode nil)
(setq indent-tabs-mode nil)

;; Configure Spelling
(cond
 ;; try hunspell at first
  ;; if hunspell does NOT exist, use aspell
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "pt_BR")
  (setq ispell-local-dictionary-alist
        ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
        ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))

 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))

(use-package counsel
:bind (("M-x" . counsel-M-x)
       ("C-x b" . counsel-ibuffer)
       ("C-x C-f" . counsel-find-file)
       :map minibuffer-local-map
       ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy)
(ivy-mode 1)  ; Activate ivy
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(use-package lsp-mode
    :hook (c++-mode-hook . lsp-deferred)
    :commands (lsp lsp-deferred)
    :init
    (setq lsp-keymap-prefix "C-c l")
    :config
    (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-show-with-mouse t))

;; For Ivy
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package ccls
  :ensure t
  :config
  (setq ccls-executable "ccls")
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

(setq-default c-basic-offset 4)
(setq compilation-read-command nil); do not prompt
(setq compile-command "make run -C ..")
(define-key c++-mode-map [f9] #'compile)

(use-package modern-cpp-font-lock
:ensure t)

(use-package glsl-mode
:ensure t)

(use-package projectile
      :config
      (projectile-mode)
      :bind-keymap
      ("C-c p" . projectile-command-map))
(setq projectile-project-run-cmd "make run")

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump t)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-s") 'evil-write)
  (define-key evil-normal-state-map (kbd "C-f") 'counsel-find-file)
  (define-key evil-normal-state-map (kbd "C-q") 'kill-current-buffer)
  (define-key evil-normal-state-map (kbd "g l") 'next-buffer)
  (define-key evil-normal-state-map (kbd "g h") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "g b") 'counsel-ibuffer)
  (define-key evil-insert-state-map (kbd "C-j") 'newline))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(fset 'yes-or-no-p 'y-or-n-p)

(use-package which-key
:init (which-key-mode)
:diminish which-key-mode
:config
(setq which-key-idle-delay 0.3))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-find-file))

(use-package dired-single)

(define-skeleton skeleton/org-latex-abnt2
"Org latex abnt2 skeleton" 
""
"#+OPTIONS: toc:nil\n"
"#+LANGUAGE: pt-br\n"
"#+LATEX_CLASS: article\n"
"#+LATEX_HEADER: \\usepackage{hyperref}\n"
"#+LATEX_HEADER: \\hypersetup{hidelinks}\n"
"#+LATEX_HEADER: \\usepackage[alf]{abntex2cite}\n"
"#+LATEX_HEADER: \\usepackage{times}\n"
"#+LATEX_HEADER: \\usepackage{indentfirst}\n"
"#+LATEX_HEADER: \\usepackage[lmargin=3cm, rmargin=2cm, tmargin=3cm, bmargin=2cm]{geometry}\n"
"\n"
"#+BEGIN_EXPORT latex\n"
"\\title{"_"}\n"
"\\author{Rafael Batista de Moraes}\n"
"\\maketitle\n"
"#+END_EXPORT\n"
_
"\n\n\n\\bibliography{ref.bib}")

(define-skeleton skeleton/bib-tex
  "Bib skeleton"
  ""
  "\@BOOK{"_",\n"
  "AUTHOR=\"""\",\n"
  "TITLE=\"\",\n"
  "PUBLISHER=\"\",\n"
  "YEAR=\"\",\n"
  "}\n"
)
(put 'upcase-region 'disabled nil)
