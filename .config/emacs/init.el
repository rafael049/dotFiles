(setq inhibit-startup-message t)  ; Disable startup page

(scroll-bar-mode -1)   ; Disable scrollbar
(tool-bar-mode -1)     ; Disable toolbar
(tooltip-mode -1)      ; Disable tooltips
(set-fringe-mode 10)   ; Give some breathing room
(menu-bar-mode -1)     ; Disable the menu bar

; Font
(add-to-list 'default-frame-alist '(font . "Roboto Mono-11"))
;(set-face-attribute 'default nil :font "Roboto Mono-11" )

; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

; Disable keys
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-j"))


; Disable line numbers in some modes
(dolist (mode '(org-mode-hook
		dired-mode-hook
		term-mode-hook
		eww-mode-hook
		pdf-view-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

; Line numbers
(global-display-line-numbers-mode t)
(setq global-display-line-numbers 'relative)
(set-default 'display-line-numbers 'relative)

;; Disable line breaks
(dolist (mode '(prog-mode-hook
		))
  (add-hook mode (lambda () (auto-fill-mode 0))))
;; Disable line wraping
(set-default 'truncate-lines t)
;; Spaces as tabs
(setq indent-tabs-mode nil)

; Configure PATH
(setenv "PATH" (concat (getenv "PATH") ":/home/rafael049/.local/bin"))
(setq exec-path (append exec-path '("/home/rafael049/.local/bin")))

; Eshell prompt
(setq eshell-prompt-regexp "^[^#$\n]*[#$] "
      eshell-prompt-function
      (lambda nil
        (concat
	 ""
	 (if (string= (eshell/pwd) (getenv "HOME"))
	     "~" (eshell/basename (eshell/pwd)))
	 " "
	 (if (= (user-uid) 0) "# " "$ "))))


; Enable auto-fill-mode in text files
(setq text-mode-hook 'turn-on-auto-fill)

; Enable code evaluation on Org-mode
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
; Octave-mode for ".m" files
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(setq inferior-octave-startup-args '("--line-editing" "-q"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize package sources;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" default))
 '(haskell-mode-hook '(interactive-haskell-mode))
 '(org-agenda-files
   '("/media/Rafael/Unifei/PET/2020/curso_python/tutorial_curso.org"))
 '(org-export-with-sub-superscripts '{})
 '(package-selected-packages
   '(rustic rust-mode edit-server dired-single evil-collection lsp-jedi lsp-pyls pdf-tools simpleclip visual-fill-column org-bullets company lsp-ui eglot lsp-eglot lsp-ccls lsp-clangd lsp-clang lsp-haskell lsp-mode evil doom-palenight doom-palenight-theme doom-themes helpful ivy-rich which-key rainbow-delimiters doom-modeline counsel use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;
;;Packages;;
;;;;;;;;;;;;

(ivy-mode 1)  ; Activate ivy
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :init (load-theme 'doom-palenight t)
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t)) ; if nil, italics is universally disabled

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package simpleclip)

;; Org Mode Setup -------------
(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (setq org-file-apps '(("\\.pdf\\'" . emacs)) ) )

(defun efs/org-font-setup ()
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

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-latex-pdf-process (list
   "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))
  (add-to-list 'org-latex-packages-alist
             '("AUTO" "babel" t ("pdflatex")))
  (setq org-ellipsis " ⯆"
	org-hide-emphasis-markers t)
  (efs/org-font-setup))

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



(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump t)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-s") 'evil-write)
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

;; Language Server
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (c-mode-hook . lsp)
  (c++-mode-hook . lsp)
  (rust-mode-hook . lsp)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-completion-provider :capf)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui)

; Haskell
(use-package lsp-haskell
  :hook
  (haskell-mode-hook . lsp-deferred)
  (haskell-literate-mode-hook . lsp-deferred)
  (haskell-mode-hook . interactive-haskell-mode)
  :config
  ;(require 'haskell-interactive-mode)
  ;(require 'haskell-process)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (setq haskell-interactive-popup-errors nil))

(use-package eglot
  :hook
  (c-mode-hook . eglot)
  (c++-mode-hook . eglot)
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd")))

(use-package company
  :hook
  (prog-mode . company-mode)
  :bind
  (:map company-active-map
	("<tab>" . company-complete-selection)
	("C-n"   . company-select-next)
        ("C-p"   . company-select-previous)
        ("<space>"   . company-complete-selection))
  (:map lsp-mode-map
        ("<space>"   . company-complete-selection)
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-iddle-delay 0))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-find-file))

(use-package dired-single)

(use-package edit-server
  :config
  (edit-server-start))

(use-package pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'auto-revert-mode) )

(use-package rust-mode
  :bind
  (("C-c C-r" . rust-run))
  :hook
  (rust-mode . lsp))






;;;;;;;;;;;;;;;
;; Skeletons ;;
;;;;;;;;;;;;;;;

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
"#+LATEX_HEADER: \usepackage{indentfirst}\n"
"#+LATEX_HEADER: \usepackage[lmargin=3cm, rmargin=2cm, tmargin=3cm, bmargin=2cm]{geometry}\n"
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
