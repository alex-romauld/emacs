;; emacs initfile
;;; Alex Romauld
;;; Emacs 27.1 and newer tested
;;; some functions taken from: https://github.com/ecxr/handmadehero/blob/master/misc/.emacs
;;;
;;;
;;; the specified folder is where emacs will look for external .el files
;    (add-to-list 'load-path "~/.emacs.d")
;(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/other")
    ;(add-to-list 'load-path "C:/dev/emacs_config/rust-mode")
;;;
;;;
;;; A custom theme folder can be specified here:
;    (add-to-list 'custom-theme-load-path "c:/dev/emacs_config")
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")


;; ======================
;; DEPENDENCIES and things that use dependencies
;; COMMENT THESE OUT to remove a dependency
;; ======================

(set-background-color "#000000")

;(set-frame-font "Hack 8" nil t) ; https://github.com/source-foundry/Hack | default font size is 10
(set-frame-font "Cascadia Mono 10" nil t)


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


(require 'redo+)
(global-set-key (kbd "C-/") 'undo) ;(global-set-key [(control z)] 'undo)
(global-set-key (kbd "C-?") 'redo) ;(global-set-key [(control shift z)] 'redo)

;; More requires at the end of the file (to make emacs use the color scheme quicker)

;(require 'misc) ; includes forward to word
;(global-set-key (kbd "M-f") 'forward-to-word) ; M-f should jump to the start of the next word, not just before it
;(global-set-key (kbd "M-F") 'forward-word)


; (require 'cc-mode)
;(require 'cedet)
;(require 'semantic)
;
;(semantic-mode 1)
;(global-ede-mode 1)
;
;(global-set-key (kbd "<backtab>") 'complete-symbol)
;(global-set-key (kbd "C-<return>") 'semantic-ia-fast-jump)
;(global-set-key (kbd "M-<return>") 'semantic-complete-jump)
;
;(setq semanticdb-project-roots '("C:/adenoid/src"))
;;(semantic-add-system-include "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.34.31933/include" 'c++-mode)
;
;(ede-cpp-root-project "Adenoid"
;					  :file "C:/adenoid/src/adenoid.cpp"
;					  ; :system-include-path '("C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.34.31933/include")
;					  ; include-path '( "/include" "../include" "/c/include" )
;					  )
;(setq ede-project '"Adenoid")
;
;(global-semanticdb-minor-mode 1)
;(global-semantic-idle-scheduler-mode 1)
;(semanticdb-enable-gnu-global-databases 'c-mode)
;(semanticdb-enable-gnu-global-databases 'c++-mode)

; (setq semanticdb-project-roots '("/path/to/your/project-root1" "/path/to/your/project-root2")) ;; Multiple project roots
; (semantic-add-system-include "C:\adenoid\src\graphics")

(global-ede-mode t)
(ede-cpp-root-project "Adenoid"
					  :file "C:/adenoid/build.bat"
					  :include-path '("deps/include", "src")
					; :system-include-path '("C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.34.31933/include")
					  )

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'objc-mode 'eglot-ensure)

(with-eval-after-load 'eglot
  ; (add-to-list 'eglot-stay-out-of 'flymake)
  )


;; Company Mode is BAD!
(require 'company)
(with-eval-after-load 'company
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (setq company-require-match nil)
  (setq company-tooltip-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-transformers '(company-sort-by-occurrence)) ; Enable caching

  ; (setq company-backends '(company-semantic))
;  (setq company-backends
;      '(company-semantic ; completions for project
;        company-elisp))  ; completions for editing elisp

  (setq company-backends (delete 'company-clang company-backends))

  (global-company-mode) ; Enable Company Mode globally

  ;; Map the Tab key to trigger completion
  ; (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  ; (define-key company-active-map (kbd "<tab>") #'company-complete-selection)

  (define-key company-active-map (kbd "M-p") nil) ;; change from nil to 0 to disable normal functionality until the tooltip is closed
  (define-key company-active-map (kbd "M-n") nil)

  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-n") #'company-select-next)

   ; (define-key company-active-map (kbd "<backtab>") #'company-show-location)
   ; (define-key company-active-map (kbd "M-.") #'company-show-location)
)
;;
;; (defun suppress-messages (old-fun &rest args)
;;   (cl-flet ((silence (&rest args1) (ignore)))
;;     (advice-add 'message :around #'silence)
;;     (unwind-protect
;;          (apply old-fun args)
;;       (advice-remove 'message #'silence))))
;; (advice-add 'company-clang--handle-error :around #'suppress-messages) ; Suppress annoying clang errors generated by company


;; Custom delete/backspace bindings to disable saving contents to the clipboard

(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(defun my-delete-line ()
  "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
  (interactive)
  (delete-region
   (point)
   (progn (end-of-line 1) (point)))
  (delete-char 1))

(defun my-delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))

; bind them to emacs's default shortcut keys:
(global-set-key (kbd "C-S-k") 'my-delete-line-backward) ; Ctrl+Shift+k
; (global-set-key (kbd "C-k") 'my-delete-line)
(global-set-key (kbd "M-d") 'my-delete-word)
(global-set-key (kbd "<C-backspace>") 'my-backward-delete-word)
(global-set-key (kbd "<M-backspace>") 'my-backward-delete-word)

;; ======================
;;       SETTINGS
;; ======================


;; startup
(setq inhibit-splash-screen t) ; turn off splash screen and go straight to scratch buffer
; By default Emacs triggers garbage collection at ~0.8MB which makes
; startup really slow. Since most systems have at least 64MB of memory,
; we increase it during initialization.
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                            (setq gc-cons-threshold 800000)))

(defun my-inhibit-startup-screen-always ()
  "Startup screen inhibitor for `command-line-functions`.
Inhibits startup screen on the first unrecognised option."
  (ignore (setq inhibit-startup-screen t)))
(add-hook 'command-line-functions #'my-inhibit-startup-screen-always)



;; Interface
(menu-bar-mode -1)                  ;; Disable the menubar
(tool-bar-mode -1)                  ;; Disable the toolbar
(scroll-bar-mode -1)                ;; Disable the scrollbar
(blink-cursor-mode 0)               ;; Make cursor not blink
(setq column-number-mode t)         ;; Show column numbers by default
(set-default 'truncate-lines t)     ;; Truncate Lines (line wrap)
(defalias 'yes-or-no-p 'y-or-n-p)   ;; We don't want to type yes and no all the time so, do y and n
(setq ring-bell-function 'ignore)   ;; Don't ring the bell
(setq vc-follow-symlinks t)         ;; Don't ask to follow symlink in git
(set-fringe-mode '(4 . 1))          ;; side margins: half width left fringe, no right fringe
(set-face-attribute 'fringe nil :background nil) ;; transparent fringe color

;; Editing
(setq echo-keystrokes .01)          ;; Print keystroke combos immediately
(delete-selection-mode t)           ;; Overwrite region selected
(setq cua-mode t)                   ;; Use CUA to delete selections
(global-auto-revert-mode 1)         ;; refresh file automatically
(modify-syntax-entry ?_ "w")        ;; underscores count as part of word
(setq scroll-conservatively 100)    ;; does something with keyboard scrolling that is nice
(setq mouse-wheel-scroll-amount '(5 ((shift) . 5))) ;; five lines at a time
(setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ;; scroll window under mouse
(electric-indent-mode 0)
(setq-default tab-width 4)                          ; tabs are 4 spaces wide
(setq indent-tabs-mode t)                           ; use tabs instead of spaces

; Stop Emacs from losing undo information by setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; Saving
(setq auto-save-default nil)        ;; Disable the horrid auto-save
(setq make-backup-files nil)        ;; Prevent emacs from creating a backup file
(setq create-lockfiles nil)         ;; Disbale creating .# lock files
(setq backup-inhibited t)

;; Searching
(setq-default case-fold-search t    ;; case insensitive searches by default
            search-highlight t)     ;; highlight matches when searching

;; Highlighting
(show-paren-mode t)                             ;; Highlight matching brackets
(when window-system (global-hl-line-mode t))    ;; Highlight the line we are currently on
(setq x-stretch-cursor t)                       ;; Stretch Cursor To Character Width (including tabs)



;; ======================
;;       FUNCTIONS
;; ======================

;; Compilation/Running
(defun find-project-directory-recursive (file)
  "Recursively search for a file."
  (interactive)
  (if (file-exists-p file) t
      (cd "../")
      (find-project-directory-recursive file)))
(setq compile-command "build.bat")
(defun c-save-compile ()
  (interactive)
  (save-buffer)
  ; use a vertical split for compilation
  (when (= (length (window-list)) 1)
	(split-window-horizontally))
  (other-window 1)
  (switch-to-buffer "*compilation*")
  (other-window 1)
  (defvar _cwd)
  (setq _cwd default-directory)
  (find-project-directory-recursive "build.bat")
  (compile "build.bat")
  (cd _cwd)
)
(defun c-save-compile-run ()
  (interactive)
  (save-buffer)
  (when (= (length (window-list)) 1)
	(split-window-horizontally))
  (other-window 1)
  (switch-to-buffer "*compilation*")
  (other-window 1)
  (defvar _cwd)
  (setq _cwd default-directory)
  (find-project-directory-recursive "build.bat")
  (compile "build.bat -r")
  (cd _cwd)
)

(defun run-debug-build ()
  (interactive)
  (defvar _cwd)
  (setq _cwd default-directory)
  (find-project-directory-recursive "build.bat")
  ;(setq async-shell-command-buffer nil)
  ;(call-process-shell-command "okular&" nil 0)
  (defadvice async-shell-command (around hide-async-windows activate)
       (save-window-excursion
          ad-do-it))
  (async-shell-command "bin\\debug\\*.exe")
  (cd _cwd)
)


 ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
(defun my-c-mode-common-hook ()
 (c-set-offset 'substatement-open 0)
 (modify-syntax-entry ?_ "w")          ;; underscores count as part of word
 (c-set-offset 'case-label '+)         ;; indent cases in switch statement
 (c-set-offset 'statement-case-open 0) ;; don't indent '{' in case statements
 (c-set-offset 'brace-list-intro '+)   ;; indent items in list
 (c-set-offset 'brace-list-open '0)    ;; don't indent open brace on new line

 (setq c++-tab-always-indent t)
 (setq c-basic-offset 4)                  ;; Default is 2
 (setq c-indent-level 4)                  ;; Default is 2
 (setq tab-width 4)
 (setq indent-tabs-mode t)  ; use spaces only if nil


 (global-set-key (kbd "RET") 'newline-and-indent)
 (local-set-key (kbd "C-c C-u") 'uncomment-region)
 )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))   ;; .h files open in cpp mode
(add-to-list 'auto-mode-alist '("\\.gl\\'" . c++-mode))  ;; .gl  (glsl) files open in cpp mode
(add-to-list 'auto-mode-alist '("\\.glh\\'" . c++-mode)) ;; .glh (glsl) files open in cpp mode

(defun my-compilation-mode-keybindings ()
  (local-set-key (kbd "C-o") 'other-window)
  (local-set-key (kbd "C-<return>") 'compilation-display-error)
  (setq truncate-lines nil)
  )

(add-hook 'compilation-mode-hook 'my-compilation-mode-keybindings)


;; Toggle Header/Source hints
(setq cc-other-file-alist
    '(("\\.c"   (".h"))
    ("\\.cpp"   (".h"))
    ("\\.h"   (".c"".cpp"))))
(setq ff-search-directories
    '("." ".." "../.." "../src" "../include" "src" "include"))



;; Remove trailing white space upon saving
;; Note: because of a bug in EIN we only delete trailing whitespace when not in EIN mode.
(add-hook 'before-save-hook
        (lambda ()
            (when (not (derived-mode-p 'ein:notebook-multilang-mode))
            (delete-trailing-whitespace))))

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
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)


;; Indent/Unindent
(defun simple-indent ()
  "Indent region or line"
  (interactive)
  (if (region-active-p)
      (indent-rigidly-right-to-tab-stop (region-beginning) (region-end))
    ;(insert "\t")
    (insert-tab)
  ))
(defun simple-unindent ()
  "Unindent region or line"
  (interactive)
  (if (region-active-p)
      (indent-rigidly-left-to-tab-stop (region-beginning) (region-end))
    (indent-rigidly-left-to-tab-stop (line-beginning-position) (line-end-position))
    )
  )
;; Leave selection up after indenting the selection
(defadvice indent-rigidly-right-to-tab-stop (after keep-transient-mark-active ())
  "Override the deactivation of the mark."
  (setq deactivate-mark nil))
(ad-activate 'indent-rigidly-right-to-tab-stop)
(defadvice indent-rigidly-left-to-tab-stop (after keep-transient-mark-active ())
  "Override the deactivation of the mark."
  (setq deactivate-mark nil))
(ad-activate 'indent-rigidly-left-to-tab-stop)



;; ======================
;; CUSTOM KEY-BINDINGS/HOOKS
;; ======================

;; default unbindings
(global-unset-key (kbd "C-x u"))
(global-unset-key (kbd "C-z"))

;; Navigation
(global-set-key (kbd "M-p") 'backward-paragraph);
(global-set-key (kbd "M-n") 'forward-paragraph);
(global-set-key (kbd "C-o") 'other-window)

;; Indentation
;(global-set-key (kbd "TAB") 'simple-indent);
;(global-set-key (kbd "<backtab>") 'simple-unindent);

;; Compilation
(global-set-key (kbd "<f5>") 'c-save-compile-run)
(global-set-key (kbd "<f6>") 'c-save-compile)
(global-set-key (kbd "<f7>") 'run-debug-build)
(global-set-key (kbd "C-=") 'next-error)
(global-set-key (kbd "C--") 'previous-error)

;; Misc
;(global-set-key (kbd "M-<up>") (lambda () (interactive) (move-line -1)))
;(global-set-key (kbd "M-<down>") (lambda () (interactive) (move-line 1)))
(global-set-key "\M-t" 'ff-find-other-file)
;(define-key global-map (kbd "C-c ;") 'iedit-mode) ;; requires the package loader ;;; iedit
(global-set-key (kbd "<f12>") 'visual-line-mode)


;; ======================
;; COLOR SCHEME AND FONT
;; ======================

; customizable syntax:
; https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html

; M-x -> comment-region, uncomment-region: set/unset default color scheme
; M-x -> eval-region: try out a color scheme


;; Beef
;; (set-face-attribute 'font-lock-preprocessor-face nil :foreground "#687FB9" :bold t) ; macros
;; (set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")              ; names of build-in functions
;; (set-face-attribute 'font-lock-comment-face nil :foreground "#61605F" :italic t)    ; comments
;; (set-face-attribute 'font-lock-constant-face nil :foreground "#998BCC")             ; constants and namespaces (std::, NULL, false, ...)
;; (set-face-attribute 'font-lock-function-name-face nil :foreground "#ABC529")        ; function being defined or declared
;; (set-face-attribute 'font-lock-keyword-face nil :foreground "#DFDFBF" :bold t)      ; keywords (if, return, enum, ...)
;; (set-face-attribute 'font-lock-string-face nil :foreground "#7F9F7F")               ; strings
;; (set-face-attribute 'font-lock-type-face nil :foreground "#6292A4")                 ; variable types
;; (set-face-attribute 'font-lock-variable-name-face nil :foreground "#8BCCBF")        ; variable names
;; (set-face-background 'hl-line "#242424")                                            ; current-line highlight
;; (set-foreground-color "#E0DEE4")                                                    ; primary text
;; (set-background-color "#2E2D33")                                                    ; background
;; (set-cursor-color "#33C633")                                                        ; cursor
;; (set-face-attribute 'region nil :background "#0000cd")                              ; highlight region


;; HH-Like
;; (set-face-attribute 'font-lock-preprocessor-face nil :foreground "#687FB9" :bold t) ; macros
;; (set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")              ; names of built-in functions
;; (set-face-attribute 'font-lock-comment-face nil :foreground "gray45" :italic t)     ; comments
;; (set-face-attribute 'font-lock-constant-face nil :foreground "#7CB8BB")             ; constants and namespaces (std::, NULL, false, ...)
;; (set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3")     ; function being defined or declared
;; (set-face-attribute 'font-lock-keyword-face nil :foreground "#DFDFBF" :bold t)      ; keywords (if, return, enum, ...)
;; (set-face-attribute 'font-lock-string-face nil :foreground "#7F9F7F")               ; strings
;; (set-face-attribute 'font-lock-type-face nil :foreground "DarkGoldenrod3")          ; variable types
;; (set-face-attribute 'font-lock-variable-name-face nil :foreground "#DCA3A3")        ; variable names
;; (set-face-background 'hl-line "#242424")                                            ; current-line highlight
;; (set-foreground-color "burlywood3")                                                 ; primary text
;; (set-background-color "#161616") ;(set-background-color "#090909")                  ; background
;; (set-cursor-color "#33C633")                                                        ; cursor
;; (set-face-attribute 'region nil :background "#0000cd")                              ; highlight region


;; DonHo
;(set-face-attribute 'font-lock-preprocessor-face nil :foreground "#768e85" :bold t) ; macros
;(set-face-attribute 'font-lock-builtin-face nil :foreground "#aa7585")              ; names of built-in functions
;(set-face-attribute 'font-lock-comment-face nil :foreground "#5b5961" :italic t)    ; comments
;;(set-face-attribute 'font-lock-doc-face nil :foreground "#5b5961" :italic t)        ; "document' comments
;(set-face-attribute 'font-lock-constant-face nil :foreground "#cdbfa8")             ; constants and namespaces (std::, NULL, false, ...)
;(set-face-attribute 'font-lock-function-name-face nil :foreground "#aa7585")        ; function being defined or declared
;(set-face-attribute 'font-lock-keyword-face nil :foreground "#aa7585" :bold t)      ; keywords (if, return, enum, ...)
;(set-face-attribute 'font-lock-string-face nil :foreground "#b0a591")               ; strings
;(set-face-attribute 'font-lock-type-face nil :foreground "#92bed0")                 ; variable types
;(set-face-attribute 'font-lock-variable-name-face nil :foreground "#b4baca")        ; variable names
;(set-face-background 'hl-line "#110e24")                                            ; current-line highlight
;(set-foreground-color "#b4baca") ;#999999                                           ; primary text
;(set-background-color "#010001")                                                    ; background
;(set-cursor-color "#a5edd9")                                                        ; cursor
;;(set-face-attribute 'region nil :background "#00005d")                              ; highlight region
;(set-face-attribute 'region nil :background "#051e30")                              ; highlight region


;; jon blow
;; https://pastebin.com/5tTEjWjL


;; end color scheme

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(zenburn))
 '(custom-safe-themes
   '("934f61fb91fa00da959c31bb118a90d5496e3cefe79fbe29a9078f92bfddce6e" "080fd60366fb1d6e7aea9f8fd0de03e2a40ac995e51b1ed21de37431d43b4d88" default))
 '(eglot-ignored-server-capabilities
   '(:documentFormattingProvider :documentRangeFormattingProvider :documentOnTypeFormattingProvider))
 '(package-selected-packages '(eglot company)))
 ;'(package-selected-packages '(company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-theme 'zenburn)

;; these are down here cause it makes emacs less blinding when it opens
; (require 'cmake-mode)
;(require 'rust-mode)
(require 'smarttabs) ; functions to indent with tabs, but align using spaces
