;; Emacs Init File (Alex Romauld)

;; Code completion requires that clang is installed and on the path
;; For spell-checking to work, hunspell needs to be installed and on the path

;; Install links
;; Clang: https://releases.llvm.org/download.html
;; Font:  https://github.com/microsoft/cascadia-code#installation

;; References:
;; https://github.com/ecxr/handmadehero/blob/master/misc/.emacs
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html - customizable syntax
;; https://pastebin.com/5tTEjWjL - jon blow color scheme
;; Old init file: https://github.com/alex-romauld/emacs/blob/b9e35715e4f309f4c08a28ff99798a52903d1eb5/init.el

;; TODO:
;; - open keys: C-t   C-;   C-'
;; - C-c C-b toggle visibility of compilation buffer
;; - more unified back and forth binds for xref and dired

;; ===================================================================
;; @                       Startup / Packages
;; ===================================================================

(setq frame-title-format "GNU Emacs")
(set-background-color "#3f3f3f")
(menu-bar-mode -1)                  ; Disable the menubar
(tool-bar-mode -1)                  ; Disable the toolbar
(when (member "Cascadia Mono" (font-family-list)) (set-frame-font "Cascadia Mono 10" nil t))

(add-to-list 'exec-path              "~/.emacs.d/hunspell/bin")
(add-to-list 'load-path              "~/.emacs.d/other")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'zenburn t)

(require 'package)
(require 'redo+)
(require 'drag-stuff)
(require 'diminish)
(require 'smart-tabs-mode)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package flycheck :ensure t)

(use-package company
  :ensure t
  :custom
  (company-minimum-prefix-length 1)
  (company-backends
   '(company-bbdb company-semantic company-cmake company-capf company-clang company-files
				  (company-dabbrev-code company-gtags company-etags company-keywords)
				  company-oddmuse))
  (company-idle-delay 0.005)
  (company-require-match nil)
  (company-selection-wrap-around t)
  (company-tooltip-idle-delay 0.005)
  (company-tooltip-scrollbar-width 0.9)
  (company-tooltip-width-grow-only t)
  :bind (:map company-active-map ("<tab>" . company-complete-selection)))

(use-package lsp-mode
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  :ensure t
  :diminish
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  :config
  (setq lsp-clients-clangd-args '("--header-insertion=never"
								  "--header-insertion-decorators=0"))
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-lens-enable nil)
  ;; (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-modeline-code-actions-enable nil)
  ;; (setq lsp-diagnostics-provider :none)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-completion-show-kind nil)
  (setq lsp-enable-links nil)
  (setq lsp-enable-on-type-formatting nil)
  ;; (setq lsp-completion-enable-additional-text-edit nil)
  (setq lsp-idle-delay 0))

(setq inhibit-splash-screen t) ; Turn off splash screen and go straight to scratch buffer
(setq gc-cons-threshold 64000000) (add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000))) ; By default Emacs triggers garbage collection at ~0.8MB which makes startup really slow. Since most systems have at least 64MB of memory, we increase it during initialization.

;; 'universal-keymap' can be used for universal bindings across all modes
(defvar universal-keymap (make-keymap)) (define-minor-mode universal-key-mode "Minor mode for my personal keybindings." :init-value t :global t :keymap universal-keymap) (add-to-list 'emulation-mode-map-alists `((universal-key-mode . ,universal-keymap)))

;; ===================================================================
;; @                            Functions
;; ===================================================================

(defun my/pulse-current-region (&rest _)
  "Pulse the current implicit or active region."
  (if mark-active
	  (pulse-momentary-highlight-region (region-beginning) (region-end))
	(pulse-momentary-highlight-region (mark) (point))))

(defun my/scroll-down ()
  "Scroll down and recenter."
  (interactive) (setq scroll-preserve-screen-position t) (recenter) (scroll-down) (setq scroll-preserve-screen-position nil))

(defun my/scroll-up ()
  "Scroll up and recenter."
  (interactive) (setq scroll-preserve-screen-position t) (scroll-up) (recenter) (setq scroll-preserve-screen-position nil))

(defun my/delete-word (arg)
  "Delete characters forward until encountering the end of a word. With argument, do this that many times. This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word. With argument, do this that many times. This command does not push text to `kill-ring'."
  (interactive "p")
  (my/delete-word (- arg)))

(defun my/delete-line ()
  "Delete text from current position to end of line char. This command does not push text to `kill-ring'."
  (interactive)
  (delete-region
   (point)
   (progn (end-of-line 1) (point)))
  (delete-char 1))

(defun my/delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (kill-region p1 p2)))

(defun find-project-directory-recursive (file)
  "Recursively search for a file."
  (interactive)
  (if (or (file-exists-p file) (string-equal default-directory "c:/")) t
      (cd "../")
      (find-project-directory-recursive file)))

(defun root-compile (arg)
  (interactive)
  (save-buffer)
  (defvar _cwd)
  (setq _cwd default-directory)
  (find-project-directory-recursive "build.bat")
  (compile arg)
  (cd _cwd))

(defun root-run (arg)
  (interactive)
  (defvar _cwd)
  (setq _cwd default-directory)
  (find-project-directory-recursive "build.bat")
  (defadvice async-shell-command (around hide-async-windows activate)
       (save-window-excursion
          ad-do-it))
  (async-shell-command arg)
  (cd _cwd))

(defun my/project-search ()
  (interactive)
  (setq _compile-command compile-command) (setq _cwd default-directory) ; Save current state to be restored
  (setq search-input (read-string "Project search: "))
  (setq cmd (concat "findstr /s /i /n /c:\"" search-input "\" *.h *.hpp *.hxx *.c *.cpp *.cxx *.td *.inc *.gl *.glsl *.glh *.yaml"))
  (find-project-directory-recursive ".git")
  (compile cmd)
  (cd _cwd) (setq compile-command _compile-command))

;; ===================================================================
;; @                           Programming
;; ===================================================================

(defun my/prog-mode-hook ()
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)" 1 font-lock-warning-face t))) ; Highlight some keywords
  (local-set-key  (kbd "<return>") 'newline-and-indent)
  (local-set-key  (kbd "C-c C-u")  'uncomment-region)
  (local-set-key  (kbd "C-c C-r")  'lsp-rename)
  (local-set-key  (kbd "C-c C-s")  'xref-find-references)
  (global-set-key (kbd "C-<f5>")   'kill-compilation)
  (hs-minor-mode)
  (diminish 'hs-minor-mode)
  (local-set-key  (kbd "C-<return>")   'hs-toggle-hiding)
  (local-set-key  (kbd "C-S-<return>") 'hs-show-all))

(defun my/c-mode-common-hook ()
 (c-toggle-comment-style -1)             ; Comment blocks using '/' instead of '/**/'
 (c-set-offset 'substatement-open    0)
 (c-set-offset 'case-label          '+)  ; Indent cases in switch statement
 (c-set-offset 'statement-case-open  0)  ; Don't indent '{' in case statements
 (c-set-offset 'brace-list-intro    '+)  ; Indent items in list
 (c-set-offset 'brace-list-open     '0)  ; Don't indent open brace on new line
 (setq c-basic-offset 4)                 ; Default is 2
 (setq c-indent-level 4)                 ; Default is 2
 (setq c-tab-always-indent   t)
 (setq c++-tab-always-indent t))

(defun my/compilation-mode-hook ()
  (local-set-key (kbd "C-<return>") 'compilation-display-error)
  (setq truncate-lines nil)
  (setq truncate-partial-width-windows nil)
  (setq compilation-scroll-output t)
  (setq compilation-always-kill t))

(defun my/dired-mode-hook ()
  (dired-hide-details-mode)
  (dired-omit-mode)
  (local-set-key (kbd "C-<return>")   'dired-find-file)
  (local-set-key (kbd "C-S-<return>") 'dired-up-directory)
  (local-set-key (kbd "S-<return>")   'dired-up-directory))

(add-hook 'prog-mode-hook         'my/prog-mode-hook)
(add-hook 'c-mode-common-hook     'my/c-mode-common-hook)
(add-hook 'compilation-mode-hook  'my/compilation-mode-hook)
(add-hook 'dired-mode-hook        'my/dired-mode-hook)

(setq compile-command "")

;; Toggle Header/Source hints
(setq cc-other-file-alist '(("\\.c" (".h")) ("\\.cpp" (".h")) ("\\.h" (".c"".cpp"))))
(setq ff-search-directories '("." ".." "../.." "../src" "../include" "src" "include"))

;; Default modes for some file extensions
(add-to-list 'auto-mode-alist '("\\.h\\'"    . c++-mode))
(add-to-list 'auto-mode-alist '("\\.gl\\'"   . c++-mode))
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.glh\\'"  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'"  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.td\\'"   . c-mode))

;; Smart tabs
(autoload 'smart-tabs-mode "smart-tabs-mode" "Intelligently indent with tabs, align with spaces!")
(autoload 'smart-tabs-mode-enable "smart-tabs-mode")
(autoload 'smart-tabs-advice "smart-tabs-mode")
(autoload 'smart-tabs-insinuate "smart-tabs-mode")
(smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'python 'ruby 'nxml)

;; ===================================================================
;; @                       Interface / Editing
;; ===================================================================

(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)

;; Interface
;;(scroll-bar-mode -1)                ; Disable the scrollbar
(blink-cursor-mode 0)               ; Make cursor not blink

(set-fringe-mode '(4 . 1))                       ; Side margins: half width left fringe, no right fringe
(set-face-attribute 'fringe nil :background nil) ; Transparent fringe color
(global-display-line-numbers-mode)               ; Enable line numbers
(setq display-line-numbers-width-start t)        ; Line number margin big enough for longest number
(setq display-line-numbers-grow-only t)          ; Never shring the line number margin
(setq column-number-mode t)                      ; Show column number in footer
(set-default 'truncate-lines t)                  ; Disable line wrap

(setq ring-bell-function 'ignore)   ; Don't ring the bell
(setq vc-follow-symlinks t)         ; Don't ask to follow symlink in git

;; Automatic window splits are side-by-side
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Editing
(setq echo-keystrokes .01)          ; Print keystroke combos immediately
(delete-selection-mode t)           ; Overwrite region selected
(setq cua-mode t)                   ; Use CUA to delete selections
(global-auto-revert-mode 1)         ; Refresh file automatically
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode) ; Buffer list keeps reverting when trying to use it
(setq scroll-conservatively 100)                    ; Does something with keyboard scrolling that is nice
(setq mouse-wheel-scroll-amount '(5 ((shift) . 5))) ; Five lines at a time
(setq mouse-wheel-progressive-speed nil)            ; Don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ; Scroll window under mouse
(pixel-scroll-precision-mode)                       ; Nice touchpad scrolling (also fixes a bug present in emacs 29.1)
;; (setq mouse-wheel-tilt-scroll t)                    ; Enable horizontal scrolling by tilting mouse wheel or via touchpad. (this seems broken in emacs 29.1)
(electric-indent-mode 0)
(setq-default tab-width 4)                          ; Tabs are 4 spaces wide
(setq-default indent-tabs-mode t)                   ; Use tabs for indentation
(global-superword-mode t)                           ; Symbol characters are part of a word
(setq completion-ignore-case t)                     ; Completion is case insensitive
(global-company-mode)

;; Stop Emacs from losing undo information by setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; Saving
(setq auto-save-default nil)        ; Disable the horrid auto-save
(setq make-backup-files nil)        ; Prevent emacs from creating a backup file
(setq create-lockfiles  nil)        ; Disbale creating .# lock files
(setq backup-inhibited  t)
(setq require-final-newline t)      ; Add a newline automatically at the end of the file on save

;; Whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; Remove trailing white space upon saving

;; y-n confirmations
(defalias 'yes-or-no-p 'y-or-n-p) ; Map "yes" and "no" to "y" and "n"
(setq confirm-kill-emacs nil)
(setq confirm-kill-processes nil)
(setq confirm-nonexistent-file-or-buffer nil)
(set-buffer-modified-p nil)
(add-hook 'kill-buffer-query-functions (lambda () (not-modified) t))

;; Searching
(setq-default case-fold-search t          ; Case insensitive searches by default
              isearch-wrap-pause 'no-ding ; Wrap-search immediately
              search-highlight t)         ; Highlight matches when searching

;; Recenter when searching
(defadvice isearch-repeat-forward  (after isearch-repeat-forward-recenter activate)  (recenter))
(defadvice isearch-repeat-backward (after isearch-repeat-backward-recenter activate) (recenter))
(ad-activate 'isearch-repeat-forward)
(ad-activate 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<backspace>")   'isearch-del-char) ; Otherwise backspace interacts with the search in a confusing way
(define-key isearch-mode-map (kbd "C-<backspace>") 'isearch-edit-string)

;; Exclude '*' and dired buffers from buffer cycling
(set-frame-parameter (selected-frame) 'buffer-predicate
					 (lambda (buf)
					   (let ((name (buffer-name buf)))
						 (not (or (string-prefix-p "*" name)
								  (eq 'dired-mode (buffer-local-value 'major-mode buf)))))))

;; Highlighting
(show-paren-mode t)                          ; Highlight matching brackets
(when window-system (global-hl-line-mode t)) ; Highlight the line we are currently on
(setq x-stretch-cursor t)                    ; Stretch Cursor To Character Width (including tabs)
(advice-add #'kill-ring-save :before #'my/pulse-current-region) ; Highlight copied region

;; Dired mode
(put 'dired-find-alternate-file 'disabled nil) ; Reuse same buffer when opening directories
(setf dired-kill-when-opening-new-dired-buffer t)
(setq dired-listing-switches "-aBhl  --group-directories-first")
(setq dired-clean-confirm-killing-deleted-buffers nil)
(setq dired-confirm-shell-command nil)
(setq dired-recursive-deletes (quote always))
(setq dired-deletion-confirmer '(lambda (x) t))
(setq dired-recursive-deletes 'always)
(setq global-auto-revert-non-file-buffers t) ; Auto refresh when files are added/deleted

;; Spellcheck
(setq ispell-program-name "hunspell")

;; Position cursor to "@@" in abbreviations
(setq-default abbrev-mode t)
(defadvice expand-abbrev (after my/expand-abbrev activate) (if ad-return-value (run-with-idle-timer 0 nil (lambda () (let ((cursor "@@")) (if (search-backward cursor last-abbrev-location t) (delete-char (length cursor))))))))

;; ===================================================================
;; @                         General Bindings
;; ===================================================================

;; Unbindings
(global-unset-key (kbd "C-x u"))
(global-unset-key (kbd "C-z"))

;; Editing
(global-set-key (kbd "C-S-k")         'my/delete-line-backward) ; Ctrl+Shift+k
;; (global-set-key (kbd "C-k")           'my/delete-line)
(global-set-key (kbd "M-d")           'my/delete-word)
(global-set-key (kbd "C-<backspace>") 'my/backward-delete-word)
(global-set-key (kbd "M-<backspace>") 'my/backward-delete-word)
(global-set-key (kbd "C-x C-r")       'revert-buffer)
(global-set-key (kbd "M-r")           'query-replace)

(global-set-key (kbd "C-/") 'undo)
(global-set-key (kbd "C-?") 'redo)

(drag-stuff-global-mode t)
(drag-stuff-define-keys)

;; Navigation
(define-key universal-keymap (kbd "C-<tab>")   'switch-to-next-buffer)
(define-key universal-keymap (kbd "C-S-<tab>") 'switch-to-prev-buffer)
(define-key universal-keymap (kbd "C-o")       'other-window)
(define-key universal-keymap (kbd "M-p")       'backward-paragraph)
(define-key universal-keymap (kbd "M-n")       'forward-paragraph)
(global-set-key              (kbd "M-g")       'goto-line)

;; Window resizing
(global-set-key (kbd "C-M-<up>")    'enlarge-window)
(global-set-key (kbd "C-M-<down>")  'shrink-window)
(global-set-key (kbd "C-M-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)

;; Scrolling
(define-key universal-keymap (kbd "C-v")      'my/scroll-up)
(define-key universal-keymap (kbd "M-v")      'my/scroll-down)
(define-key universal-keymap (kbd "C-<up>")   'scroll-down-line)
(define-key universal-keymap (kbd "C-<down>") 'scroll-up-line)
(define-key universal-keymap (kbd "C-M-p")    'scroll-down-line)
(define-key universal-keymap (kbd "C-M-n")    'scroll-up-line)
(define-key universal-keymap (kbd "C-M-S-p")  (lambda () (interactive) (scroll-down 5)))
(define-key universal-keymap (kbd "C-M-S-n")  (lambda () (interactive) (scroll-up   5)))

;; Compilation
(global-set-key (kbd "<f5>")   (lambda () (interactive) (root-compile "build.bat -r")))
(global-set-key (kbd "<f6>")   (lambda () (interactive) (root-compile "build.bat")))
(global-set-key (kbd "<f7>")   (lambda () (interactive) (root-run     "cd run_tree && \"adenoid_debug.exe\"")))
(global-set-key (kbd "S-<f5>") (lambda () (interactive) (root-compile "build.bat -release -r")))
(global-set-key (kbd "S-<f6>") (lambda () (interactive) (root-compile "build.bat -release")))
(global-set-key (kbd "S-<f7>") (lambda () (interactive) (root-run     "cd run_tree && \"The Adenoid.exe\"")))
;;
(global-set-key (kbd "C-=")  'next-error)
(global-set-key (kbd "C--")  'previous-error)
(define-key universal-keymap (kbd "C-c C-k") (lambda () (interactive) (kill-buffer "*compilation*")))
;;
(global-set-key (kbd "M-t")   'ff-find-other-file)
(global-set-key (kbd "C-S-o") 'project-find-file)
(global-set-key (kbd "C-S-s") 'my/project-search)

;; Misc
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; (global-set-key (kbd "<f1>")  (lambda () (interactive) (defadvice async-shell-command (around hide-async-windows activate) (save-window-excursion ad-do-it)) (async-shell-command "explorer .")))
(global-set-key (kbd "<f8>")  'ispell-region)
(global-set-key (kbd "<f12>") 'visual-line-mode)

;; Diminish (clean up mode line)
(diminish 'abbrev-mode)
(diminish 'superword-mode)
(diminish 'drag-stuff-mode)
(diminish 'eldoc-mode)
(diminish 'company-mode)

;; ===================================================================
;; @                       PCLP Modifications
;; ===================================================================

;; TODO should be c:/dev/pclp.el?
;; (let ((pclp-file (expand-file-name "other/pclp.el" user-emacs-directory))) (when (file-exists-p pclp-file) (load pclp-file)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comment-empty-lines nil)
 '(display-buffer-base-action '(display-buffer-use-least-recent-window))
 '(ibuffer-expert t)
 '(orderless-matching-styles '(orderless-regexp orderless-literal orderless-flex))
 '(orderless-smart-case nil)
 '(package-selected-packages
   '(clang-format company-mode company lsp-mode cape orderless))
 '(pop-up-frames nil)
 '(pop-up-windows nil)
 '(read-buffer-completion-ignore-case t)
 '(vc-suppress-confirm t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
