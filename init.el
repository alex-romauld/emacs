;; Emacs Init File (Alex Romauld)

;; Code completion requires that clang is installed and on the path
;; For spell-checking to work, hunspell needs to be installed and on the path
;; Many things can be customized by using M-x customize-group package-name

;; References:
;; https://github.com/ecxr/handmadehero/blob/master/misc/.emacs
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html - customizable syntax
;; https://pastebin.com/5tTEjWjL - jon blow color scheme
;; Old init file: https://github.com/alex-romauld/emacs/blob/b9e35715e4f309f4c08a28ff99798a52903d1eb5/init.el
;; TODO:
;; - probably turn off interactive save for work (remove-hook)
;; - consider c tab and c shift tab for navigating back and forth
;; - C-c s for find references?
;; - figure out a search all files in project thing (call-interactively 'compile)))
;; ? line number blank line
;; open keys: C-t   C-;   C-'  C-tab  C-backtab
;; Set default window to be percentage of display
;; C-c C-b toggle visibility of compilation buffer
;;    figure out tab weirdness



;; Install links
;; Clang: https://releases.llvm.org/download.html
;; Font:  https://github.com/microsoft/cascadia-code#installation

(defvar pclp-mode t)
;; (defvar pclp-mode nil)

;; ===================================================================
;; @                       Startup / Packages
;; ===================================================================

(add-to-list 'exec-path              "~/.emacs.d/hunspell/bin")
(add-to-list 'load-path              "~/.emacs.d/other")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(set-background-color "#3f3f3f")
(when (member "Cascadia Mono" (font-family-list)) (set-frame-font "Cascadia Mono 10" nil t))
(load-theme 'zenburn t)

(setq inhibit-splash-screen t) ;; Turn off splash screen and go straight to scratch buffer
(setq gc-cons-threshold 64000000) (add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000))) ;; By default Emacs triggers garbage collection at ~0.8MB which makes startup really slow. Since most systems have at least 64MB of memory, we increase it during initialization.

;; Window size and position
(when (window-system)
  (set-frame-height (selected-frame) 40)
  (set-frame-width (selected-frame) 150)
  (modify-frame-parameters (selected-frame) '((user-position . t) (top . 0.5) (left . 0.5))))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; ===================================================================
;; @                            FUNCTIONS
;; ===================================================================

;; Scroll + Recenter

(defun my-scroll-down () (interactive) (setq scroll-preserve-screen-position t) (scroll-down) (setq scroll-preserve-screen-position nil))
(defun my-scroll-up () (interactive) (setq scroll-preserve-screen-position t) (scroll-up) (recenter) (setq scroll-preserve-screen-position nil))

;; Custom delete functions that avoid putting content into the clipboard

(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word. With argument, do this that many times. This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word. With argument, do this that many times. This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(defun my-delete-line ()
  "Delete text from current position to end of line char. This command does not push text to `kill-ring'."
  (interactive)
  (delete-region
   (point)
   (progn (end-of-line 1) (point)))
  (delete-char 1))

(defun my-delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position. This command does not push text to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))

;; Moves Line/Region Up/Down

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

;; Compilation/Running

(defun find-project-directory-recursive (file)
  "Recursively search for a file."
  (interactive)
  (if (or (file-exists-p file) (string-equal default-directory "c:/")) t
      (cd "../")
      (find-project-directory-recursive file)))

(defun c-save-compile ()
  (interactive)
  (save-buffer)
  (defvar _cwd)
  (setq _cwd default-directory)
  (find-project-directory-recursive "build.bat")
  (compile "build.bat")
  (cd _cwd))

(defun c-save-compile-run ()
  (interactive)
  (save-buffer)
  (defvar _cwd)
  (setq _cwd default-directory)
  (find-project-directory-recursive "build.bat")
  (compile "build.bat -r")
  (cd _cwd))

(defun run-debug-build ()
  (interactive)
  (defvar _cwd)
  (setq _cwd default-directory)
  (find-project-directory-recursive "build.bat")
  (defadvice async-shell-command (around hide-async-windows activate)
       (save-window-excursion
          ad-do-it))
  (async-shell-command "bin\\debug\\*.exe")
  (cd _cwd)
)

(defun my-project-search ()
  (interactive)
  (setq _compile-command compile-command) (setq _cwd default-directory) ;; save current state to be restored
  (setq search-input (read-string "Project search: "))
  (setq cmd (concat "findstr /s /i /n /c:\"" search-input "\" *.h *.hpp *.hxx *.c *.cpp *.cxx *.td *.inc"))
  (find-project-directory-recursive ".git")
  (compile cmd)
  (cd _cwd) (setq compile-command _compile-command))

;; ===================================================================
;; @                           C/C++ Setup
;; ===================================================================

(defun my-c-mode-common-hook ()
 (c-toggle-comment-style -1)             ;; comment blocks using '/' instead of '/**/'
 (c-set-offset 'substatement-open    0)
 (c-set-offset 'case-label          '+)  ;; indent cases in switch statement
 (c-set-offset 'statement-case-open  0)  ;; don't indent '{' in case statements
 (c-set-offset 'brace-list-intro    '+)  ;; indent items in list
 (c-set-offset 'brace-list-open     '0)  ;; don't indent open brace on new line

 (setq c-basic-offset   4)               ;; Default is 2
 (setq c-indent-level   4)               ;; Default is 2
 (setq tab-width        4)
 (when (not pclp-mode)
   (setq indent-tabs-mode t)               ;; use spaces only if nil
   (setq c++-tab-always-indent t)
   )
 )

(defun my-xref-keybindings ()
  (define-key xref--xref-buffer-mode-map (kbd "C-<return>") 'xref-show-location-at-point)
  (define-key xref--xref-buffer-mode-map (kbd "<return>")   'xref-goto-xref)
  )

(defun my-compilation-mode-keybindings ()
  (local-set-key (kbd "C-<return>") 'compilation-display-error)
  (setq truncate-lines nil)
  (setq truncate-partial-width-windows nil)
  (setq compilation-scroll-output t)
  (setq compilation-always-kill t)
  )

(add-hook 'c-mode-common-hook     'my-c-mode-common-hook)
(add-hook 'compilation-mode-hook  'my-compilation-mode-keybindings)
(add-hook 'xref-after-update-hook 'my-xref-keybindings)

(setq compile-command "")

;; Toggle Header/Source hints
(setq cc-other-file-alist '(("\\.c" (".h")) ("\\.cpp" (".h")) ("\\.h" (".c"".cpp"))))
;; (setq ff-search-directories '("." ".." "../.." "../src" "../include" "src" "include"))

(add-to-list 'auto-mode-alist '("\\.h\\'" .   c++-mode)) ;; .h          files open in cpp mode
(add-to-list 'auto-mode-alist '("\\.gl\\'" .  c++-mode)) ;; .gl  (glsl) files open in cpp mode
(add-to-list 'auto-mode-alist '("\\.glh\\'" . c++-mode)) ;; .glh (glsl) files open in cpp mode
(add-to-list 'auto-mode-alist '("\\.inc\\'" . c++-mode)) ;; .inc        files open in cpp mode
(add-to-list 'auto-mode-alist '("\\.td\\'"  . c-mode))   ;; .td         files open in c mode

(when (not pclp-mode)
  (require 'smart-tabs-mode)
  (autoload 'smart-tabs-mode "smart-tabs-mode" "Intelligently indent with tabs, align with spaces!")
  (autoload 'smart-tabs-mode-enable "smart-tabs-mode")
  (autoload 'smart-tabs-advice "smart-tabs-mode")
  (autoload 'smart-tabs-insinuate "smart-tabs-mode")
  (smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'python 'ruby 'nxml)
  )

(with-eval-after-load 'eglot
  ;;(add-to-list 'eglot-server-programs
  ;;             '((c-mode c++-mode)
  ;;               . ("clangd"
  ;;                  "--log=error"
  ;;                  "--background-index"
  ;;                  "--header-insertion=never")))
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode)
                 . ("clangd"
                    "-j=8"
;;                    "--log=verbose"
                    "--background-index"
                    "--completion-style=detailed"
                    "--pch-storage=memory"
                    "--header-insertion=never"
                    "--header-insertion-decorators=0")))
  ;; (add-to-list 'eglot-stay-out-of 'flymake)
  (add-to-list 'eglot-stay-out-of 'eldoc)
  )
(add-hook 'c-mode-hook   'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'objc-mode     'eglot-ensure)

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.0)
  (corfu-history-mode)
  (corfu-indexed-mode t)
  ;; (corfu-separator ?\s)
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary t)
  (corfu-preview-current nil)
  (corfu-scroll-margin 4)
  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
		;;  (shell-mode . corfu-mode)
        ;;  (eshell-mode . corfu-mode))
  :init
  (global-corfu-mode))

;; ===================================================================
;; @                       INTERFACE / EDITING
;; ===================================================================

;; Interface
(menu-bar-mode -1)                  ;; Disable the menubar
(tool-bar-mode -1)                  ;; Disable the toolbar
;;(scroll-bar-mode -1)                ;; Disable the scrollbar
(global-display-line-numbers-mode)  ;; Enable line numbers
(defun display-line-numbers-equalize () (setq display-line-numbers-width (length (number-to-string (line-number-at-pos (point-max)))))) ;; equalize the line-width margin
(add-hook 'find-file-hook 'display-line-numbers-equalize)
(blink-cursor-mode 0)               ;; Make cursor not blink
(setq column-number-mode t)         ;; Show column number in footer
(set-default 'truncate-lines t)     ;; Disable line wrap
(defalias 'yes-or-no-p 'y-or-n-p)   ;; Map "yes" and "no" to "y" and "n"
(setq ring-bell-function 'ignore)   ;; Don't ring the bell
(setq vc-follow-symlinks t)         ;; Don't ask to follow symlink in git
(set-fringe-mode '(4 . 1))          ;; Side margins: half width left fringe, no right fringe
(set-face-attribute 'fringe nil :background nil) ;; transparent fringe color

;; Editing
(setq echo-keystrokes .01)          ;; Print keystroke combos immediately
(delete-selection-mode t)           ;; Overwrite region selected
(setq cua-mode t)                   ;; Use CUA to delete selections
(global-auto-revert-mode 1)         ;; refresh file automatically
(setq scroll-conservatively 100)    ;; does something with keyboard scrolling that is nice
(setq mouse-wheel-scroll-amount '(5 ((shift) . 5))) ;; five lines at a time
(setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ;; scroll window under mouse
(electric-indent-mode 0)
(setq-default tab-width 4)                          ;; tabs are 4 spaces wide
(when (not pclp-mode)
  (setq indent-tabs-mode t)                           ;; use tabs instead of spaces
  )
(global-superword-mode t)                           ;; symbol characters are part of a word
(setq completion-ignore-case t)                     ;; completion is case insensitive

;; Stop Emacs from losing undo information by setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; Saving
(setq auto-save-default nil)        ;; Disable the horrid auto-save
(setq make-backup-files nil)        ;; Prevent emacs from creating a backup file
(setq create-lockfiles  nil)        ;; Disbale creating .# lock files
(setq backup-inhibited  t)

;; Remove trailing white space upon saving
;; Note: because of a bug in EIN we only delete trailing whitespace when not in EIN mode.
(add-hook 'before-save-hook (lambda () (when (not (derived-mode-p 'ein:notebook-multilang-mode)) (delete-trailing-whitespace))))

;; Searching
(setq-default case-fold-search t    ;; case insensitive searches by default
              search-highlight t)   ;; highlight matches when searching

;; Highlighting
(show-paren-mode t)                             ;; Highlight matching brackets
(when window-system (global-hl-line-mode t))    ;; Highlight the line we are currently on
(setq x-stretch-cursor t)                       ;; Stretch Cursor To Character Width (including tabs)

;; Highlight some keywords in prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            ;; Highlighting in cmake-mode this way interferes with cmake-font-lock
            (when (not (derived-mode-p 'cmake-mode))
              (font-lock-add-keywords
               nil
               '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)"
                  1 font-lock-warning-face t))))
            )
          )

;; Dired mode
(setq dired-listing-switches "-aBhl  --group-directories-first")
(defun my-dired-mode-hook () (dired-hide-details-mode)
	   (local-set-key (kbd "C-<return>") 'dired-find-file)
	   (local-set-key (kbd "C-S-<return>") 'dired-up-directory)
	   (local-set-key (kbd "S-<return>") 'dired-up-directory))
(add-hook 'dired-mode-hook 'my-dired-mode-hook)
(setf dired-kill-when-opening-new-dired-buffer t)
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))

;; Spellcheck
(setq ispell-program-name "hunspell")

;; ===================================================================
;; @                         General Bindings
;; ===================================================================

;; 'gkeymap' can be used for global bindings across all modes
(defvar gkeymap (make-keymap)) (define-minor-mode gkey-mode "Minor mode for my personal keybindings." :init-value t :global t :keymap gkeymap) (add-to-list 'emulation-mode-map-alists `((gkey-mode . ,gkeymap)))

;; Unbindings
(global-unset-key (kbd "C-x u"))
(global-unset-key (kbd "C-z"))

;; Editing
(global-set-key (kbd "C-S-k")         'my-delete-line-backward) ; Ctrl+Shift+k
;; (global-set-key (kbd "C-k")           'my-delete-line)
(global-set-key (kbd "M-d")           'my-delete-word)
(global-set-key (kbd "<C-backspace>") 'my-backward-delete-word)
(global-set-key (kbd "<M-backspace>") 'my-backward-delete-word)

(global-set-key [M-up]   'move-text-up)
(global-set-key [M-down] 'move-text-down)

(require 'redo+)
(global-set-key (kbd "C-/") 'undo)
(global-set-key (kbd "C-?") 'redo)

;; (global-set-key (kbd "") 'revert-buffer)

;; Navigation
(define-key gkeymap (kbd "C-<tab>") 'switch-to-buffer)
(define-key gkeymap (kbd "C-o") 'other-window)
(define-key gkeymap (kbd "M-p") 'backward-paragraph)
(define-key gkeymap (kbd "M-n") 'forward-paragraph)
;;
(define-key gkeymap (kbd "C-v") 'my-scroll-up)
(define-key gkeymap (kbd "M-v") 'my-scroll-down)
;;
(global-set-key (kbd "M-p") 'backward-paragraph);
(global-set-key (kbd "M-n") 'forward-paragraph);
;;
(global-set-key "\M-t" 'ff-find-other-file)
(global-set-key (kbd "C-S-o") 'project-find-file)
(global-set-key (kbd "C-S-s") 'my-project-search)

;; Compilation
(global-set-key (kbd "<f5>") 'c-save-compile-run)
(global-set-key (kbd "<f6>") 'c-save-compile)
(global-set-key (kbd "<f7>") 'run-debug-build)
(global-set-key (kbd "C-=")  'next-error)
(global-set-key (kbd "C--")  'previous-error)
(define-key gkeymap (kbd "C-c C-k") (lambda () (interactive) (kill-buffer "*compilation*")))

;; Misc
(global-set-key (kbd "<f8>")  'ispell-region)
(global-set-key (kbd "<f12>") 'visual-line-mode)

;; Coding
(defun prog-mode-bindings-hook ()
  (local-set-key (kbd "<return>")     'newline-and-indent)
  ;;(global-set-key (kbd "C-c C-f")      'clang-format-region)
  (local-set-key (kbd "C-c C-u")      'uncomment-region)
  (local-set-key (kbd "C-c C-r")      'eglot-rename)
  (local-set-key (kbd "C-c C-s")      'xref-find-references)
  ;; (local-set-key (kbd "C-<return>")   'xref-find-definitions)
  (local-set-key (kbd "C-c C-d")   'xref-find-definitions)
  (local-set-key (kbd "C-S-<return>") 'xref-go-back)
  (local-set-key (kbd "C-'")          'xref-go-forward)
  (local-set-key (kbd "C-;")          'xref-go-back)

  (local-set-key (kbd "C-c C-e")      'eglot-reconnect)
  )
(add-hook 'prog-mode-hook 'prog-mode-bindings-hook)

(define-key corfu-map (kbd "<space>")  #'corfu-quit)
(define-key corfu-map (kbd "<return>") #'corfu-complete)

;; ===================================================================
;; @                       PCLP Modifications
;; ===================================================================

(defvar pclp-debug-args "")
(defun compile-pclp-debug-args ()
  (interactive)
  (setq pclp-debug-args (read-string "Debug Arguments: " pclp-debug-args))
  (compile-pclp-debug))
(defun compile-pclp-debug ()
  (interactive)
  (setq _compile-command compile-command) (setq _cwd default-directory) ;; save current state to be restored
  (cd (vc-root-dir)) (cd "build/Debug")
  (setq cmd "cmake --build .. --target pclp --config Debug")
  (if (not (string= pclp-debug-args "")) (setq cmd (concat cmd " && " pclp-debug-args)))
  (save-buffer) (compile cmd)
  (cd _cwd) (setq compile-command _compile-command))

(defvar pclp-release-args "")
(defun compile-pclp-release-args ()
  (interactive)
  (setq pclp-release-args (read-string "Release Arguments: " pclp-release-args))
  (compile-pclp-release))
(defun compile-pclp-release ()
  (interactive)
  (setq _compile-command compile-command) (setq _cwd default-directory) ;; save current state to be restored
  (cd (vc-root-dir)) (cd "build/Release")
  (setq cmd "cmake --build .. --target pclp --config Release")
  (if (not (string= pclp-release-args "")) (setq cmd (concat cmd " && " pclp-release-args)))
  (save-buffer) (compile cmd)
  (cd _cwd) (setq compile-command _compile-command))

(defun pclp-c-mode-common-hook ()
  (setq indent-tabs-mode      nil)
  (setq c-tab-always-indent   nil)
  (setq c++-tab-always-indent nil))

(when pclp-mode
  (message "pclp mode")

  (find-file "c:/dev/notes.txt")
  (split-window-horizontally)
  (find-file-other-window "c:/dev/pclp")
  (other-window 1)

  (setq indent-tabs-mode nil)
  (add-hook 'c-mode-common-hook 'pclp-c-mode-common-hook)

  (use-package clang-format :ensure t)
  ;(require 'clang-format)
  (global-set-key (kbd "C-c C-f") 'clang-format-region)

  (global-set-key (kbd "<f5>")    'compile-pclp-debug)
  (global-set-key (kbd "S-<f5>")  'compile-pclp-debug-args)
  ;;
  (global-set-key (kbd "<f6>")    'compile-pclp-release)
  (global-set-key (kbd "S-<f6>")  'compile-pclp-release-args)
  ;;
  (global-set-key (kbd "S-<f7>") 'compile)
  (global-set-key (kbd "<f7>")   'recompile)
  ;;(global-set-key (kbd "<f7>")    'compile)

  ;; Bind f7 to run pclp on some file, prompy user for file_name, compile pclp && pclp file_name
  )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-buffer-base-action '(display-buffer-use-least-recent-window))
 '(eglot-ignored-server-capabilities
   '(:codeActionProvider :codeLensProvider :documentFormattingProvider :documentRangeFormattingProvider :documentOnTypeFormattingProvider :foldingRangeProvider :executeCommandProvider :inlayHintProvider))
 '(eldoc-echo-area-use-multiline-p nil)
 '(eldoc-idle-delay 0)
 '(package-selected-packages '(corfu zenburn-theme))
 '(pop-up-frames nil)
 '(pop-up-windows nil)
 '(read-buffer-completion-ignore-case t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
