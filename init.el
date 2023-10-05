;; Emacs Init File (Alex Romauld)

;; Code completion requires that clang is installed
;; Many things can be customized by using M-x customize-group package-name

;; References:
;; https://github.com/ecxr/handmadehero/blob/master/misc/.emacs
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html - customizable syntax
;; https://pastebin.com/5tTEjWjL - jon blow color scheme
;; (add-to-list 'load-path              "~/.emacs.d/other")
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; Old init file: https://github.com/alex-romauld/emacs/blob/b9e35715e4f309f4c08a28ff99798a52903d1eb5/init.el
;; (add-to-list 'exec-path "C:/dev/emacs_config/hunspell/bin")

;; ===================================================================
;; @                       Startup / Packages
;; ===================================================================

(set-background-color "#3f3f3f")

(setq inhibit-splash-screen t) ; turn off splash screen and go straight to scratch buffer

; By default Emacs triggers garbage collection at ~0.8MB which makes
; startup really slow. Since most systems have at least 64MB of memory,
; we increase it during initialization.
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               (setq gc-cons-threshold 800000)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'zenburn-theme)
  (package-install 'zenburn-theme))

(unless (package-installed-p 'company)
  (package-install 'company))

(set-frame-font "Cascadia Mono 10" nil t)
(load-theme 'zenburn t)

;; ===================================================================
;; @                            FUNCTIONS
;; ===================================================================

;; Custom delete functions that avoid putting content into the clipboard

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

;; Compilation/Running

(defun find-project-directory-recursive (file)
  "Recursively search for a file."
  (interactive)
  (if (or (file-exists-p file) (string-equal default-directory "c:/")) t
      (cd "../")
      (find-project-directory-recursive file)))

(defun ede-cd-to-project-root ()
  "Change the current working directory to the root of the current EDE project."
  (interactive)
  (let ((project (ede-current-project)))
    (if project
        (setq default-directory (ede-project-root-directory project))
      (message "Not in an EDE project."))))

(defun c-save-compile ()
  (interactive)
  (save-buffer)
  (defvar _cwd)
  (setq _cwd default-directory)
  (find-project-directory-recursive "build.bat")
  (compile "build.bat")
  (cd _cwd)
)

(defun c-save-compile-run ()
  (interactive)
  (save-buffer)
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
  (defadvice async-shell-command (around hide-async-windows activate)
       (save-window-excursion
          ad-do-it))
  (async-shell-command "bin\\debug\\*.exe")
  (cd _cwd)
)

;; redo+

(defvar redo-version "1.19"
  "Version number for the Redo+ package.")

(defvar last-buffer-undo-list nil
  "The head of buffer-undo-list at the last time an undo or redo was done.")
(make-variable-buffer-local 'last-buffer-undo-list)

(make-variable-buffer-local 'pending-undo-list)

;; Emacs 20 variable
;;(defvar undo-in-progress) ; Emacs 20 is no longer supported.

;; Emacs 21 variable
(defvar undo-no-redo nil)

(defun redo-error (format &rest args)
  "Call `user-error' if available.  Otherwise, use `error' instead."
  (if (fboundp 'user-error)
      (apply 'user-error format args)
    (apply 'error format args)))

(defun redo (&optional count)
  "Redo the the most recent undo.
Prefix arg COUNT means redo the COUNT most recent undos.
If you have modified the buffer since the last redo or undo,
then you cannot redo any undos before then."
  (interactive "*p")
  (if (eq buffer-undo-list t)
      (redo-error "No undo information in this buffer"))
  (if (eq last-buffer-undo-list nil)
      (redo-error "No undos to redo"))
  (or (eq last-buffer-undo-list buffer-undo-list)
      ;; skip one undo boundary and all point setting commands up
      ;; until the next undo boundary and try again.
      (let ((p buffer-undo-list))
		(and (null (car-safe p)) (setq p (cdr-safe p)))
		(while (and p (integerp (car-safe p)))
		  (setq p (cdr-safe p)))
		(eq last-buffer-undo-list p))
      (redo-error "Buffer modified since last undo/redo, cannot redo"))
  (and (eq (cdr buffer-undo-list) pending-undo-list)
       (redo-error "No further undos to redo in this buffer"))
  ;; This message seems to be unnecessary because the echo area
  ;; is rewritten before the screen is updated.
  ;;(or (eq (selected-window) (minibuffer-window))
  ;;    (message "Redo..."))
  (let ((modified (buffer-modified-p))
		(undo-in-progress t)
		(recent-save (recent-auto-save-p))
		(old-undo-list buffer-undo-list)
		(p buffer-undo-list)
		(q (or pending-undo-list t))
		(records-between 0)
		(prev nil) next)
    ;; count the number of undo records between the head of the
    ;; undo chain and the pointer to the next change.  Note that
    ;; by `record' we mean clumps of change records, not the
    ;; boundary records.  The number of records will always be a
    ;; multiple of 2, because an undo moves the pending pointer
    ;; forward one record and prepend a record to the head of the
    ;; chain.  Thus the separation always increases by two.  When
    ;; we decrease it we will decrease it by a multiple of 2
    ;; also.
    (while p
      (setq next (cdr p))
      (cond ((eq next q)
			 ;; insert the unmodified status entry into undo records
			 ;; if buffer is not modified.  The undo command inserts
			 ;; this information only in redo entries.
			 (when (and (not modified) (buffer-file-name))
			   (let* ((time (nth 5 (file-attributes (buffer-file-name))))
					  (elt (if (cddr time) ;; non-nil means length > 2
							   time                           ;; Emacs 24
							 (cons (car time) (cadr time))))) ;; Emacs 21-23
				 (if (eq (car-safe (car prev)) t)
					 (setcdr (car prev) elt)
				   (setcdr prev (cons (cons t elt) p)))))
			 (setq next nil))
			((null (car next))
			 (setq records-between (1+ records-between))))
      (setq prev p
			p next))
    ;; don't allow the user to redo more undos than exist.
    ;; only half the records between the list head and the pending
    ;; pointer are undos that are a part of this command chain.
    (setq count (min (/ records-between 2) count)
		  p (primitive-undo (1+ count) buffer-undo-list))
    (if (eq p old-undo-list)
		nil ;; nothing happened
      ;; set buffer-undo-list to the new undo list.  if has been
      ;; shortened by `count' records.
      (setq buffer-undo-list p)
      ;; primitive-undo returns a list without a leading undo
      ;; boundary.  add one.
      (undo-boundary)
      ;; now move the pending pointer backward in the undo list
      ;; to reflect the redo.  sure would be nice if this list
      ;; were doubly linked, but no... so we have to run down the
      ;; list from the head and stop at the right place.
      (let ((n (- records-between count)))
		(setq p (cdr old-undo-list))
		(while (and p (> n 0))
		  (setq p (cdr (memq nil p))
				n (1- n)))
		(setq pending-undo-list p)))
    (and modified (not (buffer-modified-p))
		 (delete-auto-save-file-if-necessary recent-save))
    (or (eq (selected-window) (minibuffer-window))
		(message "Redo!"))
    (setq last-buffer-undo-list buffer-undo-list)))

(defun undo (&optional arg)
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric argument serves as a repeat count."
  (interactive "*p")
  (let ((modified (buffer-modified-p))
		(recent-save (recent-auto-save-p)))
    ;; This message seems to be unnecessary because the echo area
    ;; is rewritten before the screen is updated.
    ;;(or (eq (selected-window) (minibuffer-window))
    ;;    (message "Undo..."))
    (let ((p buffer-undo-list)
		  (old-pending-undo-list pending-undo-list))
      (or (eq last-buffer-undo-list buffer-undo-list)
		  ;; skip one undo boundary and all point setting commands up
		  ;; until the next undo boundary and try again.
		  (progn (and (null (car-safe p)) (setq p (cdr-safe p)))
				 (while (and p (integerp (car-safe p)))
				   (setq p (cdr-safe p)))
				 (eq last-buffer-undo-list p))
		  (progn (undo-start)
				 ;; get rid of initial undo boundary
				 (undo-more 1)
				 (not undo-no-redo))
		  ;; discard old redo information if undo-no-redo is non-nil
		  (progn (if (car-safe last-buffer-undo-list)
					 (while (and p (not (eq last-buffer-undo-list
											(cdr-safe p))))
					   (setq p (cdr-safe p)))
				   (setq p last-buffer-undo-list))
				 (if p (setcdr p old-pending-undo-list)))
		  ))
    (undo-more (or arg 1))
    ;; Don't specify a position in the undo record for the undo command.
    ;; Instead, undoing this should move point to where the change is.
    ;;
    ;;;; The old code for this was mad!  It deleted all set-point
    ;;;; references to the position from the whole undo list,
    ;;;; instead of just the cells from the beginning to the next
    ;;;; undo boundary.  This does what I think the other code
    ;;;; meant to do.
    (let* ((p buffer-undo-list)
		   (list (cons nil p))
		   (prev list))
      (while (car p)
		(if (integerp (car p))
			(setcdr prev (cdr p))
		  (setq prev p))
		(setq p (cdr p)))
      (setq buffer-undo-list (cdr list)))
    (and modified (not (buffer-modified-p))
		 (delete-auto-save-file-if-necessary recent-save)))
  (or (eq (selected-window) (minibuffer-window))
      (message "Undo!"))
  (setq last-buffer-undo-list buffer-undo-list))

;; Modify menu-bar and tool-bar item of GNU Emacs
(unless (featurep 'xemacs)
  ;; condition to undo
  (mapc (lambda (map)
		  (let* ((p (assq 'undo (cdr map)))
				 (l (memq :enable (setcdr p (copy-sequence (cdr p))))))
			(when l
			  (setcar (cdr l)
					  '(and (not buffer-read-only)
							(consp buffer-undo-list)
							(or (not (or (eq last-buffer-undo-list
											 buffer-undo-list)
										 (eq last-buffer-undo-list
											 (cdr buffer-undo-list))))
								(listp pending-undo-list)))))))
		(append (list menu-bar-edit-menu)
				(if window-system (list tool-bar-map))))
  ;; redo's menu-bar entry
  (define-key-after menu-bar-edit-menu [redo]
    '(menu-item "Redo" redo
				:enable
				(and
				 (not buffer-read-only)
				 (not (eq buffer-undo-list t))
				 (not (eq last-buffer-undo-list nil))
				 (or (eq last-buffer-undo-list buffer-undo-list)
					 (let ((p buffer-undo-list))
					   (and (null (car-safe p)) (setq p (cdr-safe p)))
					   (while (and p (integerp (car-safe p)))
						 (setq p (cdr-safe p)))
					   (eq last-buffer-undo-list p)))
				 (not (eq (cdr buffer-undo-list) pending-undo-list)))
				:help "Redo the most recent undo")
    'undo)
  ;; redo's tool-bar icon
  (when window-system
    (tool-bar-add-item-from-menu
     'redo "redo" nil
     :visible '(not (eq 'special (get major-mode 'mode-class))))
    (define-key-after tool-bar-map [redo]
      (cdr (assq 'redo tool-bar-map)) 'undo)
    ;; use gtk+ icon if Emacs23
    (if (boundp 'x-gtk-stock-map)
		(setq x-gtk-stock-map
			  (cons '("etc/images/redo" . "gtk-redo") x-gtk-stock-map)))
    ;; update tool-bar icon immediately
    (defun redo-toolbar-update (&optional bgn end lng)
      (interactive)
      (set-buffer-modified-p (buffer-modified-p)))
    (add-hook 'after-change-functions 'redo-toolbar-update))
  )

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
 (setq indent-tabs-mode t)               ;; use spaces only if nil
 (setq c++-tab-always-indent t)

 (local-set-key (kbd "C-c C-u")      'uncomment-region)
 (local-set-key (kbd "C-c C-r")      'eglot-rename)
 (local-set-key (kbd "C-c C-f")      'xref-find-references)
 ;; (local-set-key (kbd "")        'xref-find-references-and-replace)

 (local-set-key (kbd "C-<return>")   'xref-find-definitions)
 (local-set-key (kbd "C-S-<return>") 'xref-go-back)

 (local-set-key (kbd "C-.")          'xref-go-forward)
 (local-set-key (kbd "C-,")          'xref-go-back)
 )

(defun my-xref-keybindings ()
  (define-key xref--xref-buffer-mode-map (kbd "C-o")        'other-window)
  (define-key xref--xref-buffer-mode-map (kbd "C-<return>") 'xref-show-location-at-point)
  (define-key xref--xref-buffer-mode-map (kbd "<return>")   'xref-goto-xref)
  )

(defun my-compilation-mode-keybindings ()
  (local-set-key (kbd "C-o") 'other-window)
  (local-set-key (kbd "C-<return>") 'compilation-display-error)
  (setq truncate-lines nil)
  (setq truncate-partial-width-windows nil)
  (setq compilation-scroll-output 'first-error)
  )

(add-hook 'c-mode-common-hook     'my-c-mode-common-hook)
(add-hook 'compilation-mode-hook  'my-compilation-mode-keybindings)
(add-hook 'xref-after-update-hook 'my-xref-keybindings)

(setq compile-command "")

;; Toggle Header/Source hints

(setq cc-other-file-alist
    '(("\\.c"   (".h"))
    ("\\.cpp"   (".h"))
    ("\\.h"   (".c"".cpp"))))

(setq ff-search-directories
    '("." ".." "../.." "../src" "../include" "src" "include"))

(add-to-list 'auto-mode-alist '("\\.h\\'" .   c++-mode)) ;; .h          files open in cpp mode
(add-to-list 'auto-mode-alist '("\\.gl\\'" .  c++-mode)) ;; .gl  (glsl) files open in cpp mode
(add-to-list 'auto-mode-alist '("\\.glh\\'" . c++-mode)) ;; .glh (glsl) files open in cpp mode

;; smart-tabs (indent with tabs, align with spaces)

(defadvice align (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice align-regexp (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice indent-relative (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice indent-according-to-mode (around smart-tabs activate)
  (let ((indent-tabs-mode indent-tabs-mode))
    (if (memq indent-line-function
              '(indent-relative
                indent-relative-maybe))
        (setq indent-tabs-mode nil))
    ad-do-it))

(defmacro smart-tabs-advice (function offset)
  `(progn
     (defvaralias ',offset 'tab-width)
     (defadvice ,function (around smart-tabs activate)
       (cond
        (indent-tabs-mode
         (save-excursion
           (beginning-of-line)
           (while (looking-at "\t*\\( +\\)\t+")
             (replace-match "" nil nil nil 1)))
         (setq tab-width tab-width)
         (let ((tab-width fill-column)
               (,offset fill-column)
               (wstart (window-start)))
           (unwind-protect
               (progn ad-do-it)
             (set-window-start (selected-window) wstart))))
        (t
         ad-do-it)))))

(smart-tabs-advice c-indent-line c-basic-offset)
(smart-tabs-advice c-indent-region c-basic-offset)

;; My Projects

;;(global-ede-mode t)
;;(ede-cpp-root-project "Adenoid"
;;					  :file "C:/adenoid/build.bat"
;;					  :include-path '("deps/include", "src")
;;					  ;; :system-include-path '("C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.34.31933/include")
;;					  )
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'objc-mode 'eglot-ensure)
;; (with-eval-after-load 'eglot (add-to-list 'eglot-stay-out-of 'flymake))
(with-eval-after-load 'eglot (add-to-list 'eglot-stay-out-of 'eldoc))

;; Company

(require 'company)
(with-eval-after-load 'company
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (setq company-require-match nil)
  (setq company-tooltip-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-transformers '(company-sort-by-occurrence))

  (setq company-backends
		'(company-capf     ;; completions for project
		  company-elisp))  ;; completions for editing elisp
  ;; (setq company-backends (delete 'company-clang company-backends))
  ;; (global-company-mode) ; Enable Company Mode globally

  ;; Map the Tab key to trigger completion
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  )

(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c-mode-hook 'company-mode)
(add-hook 'objc-mode 'company-mode)
(add-hook 'emacs-lisp-mode-hook (lambda () (company-mode 1)))

;; ===================================================================
;; @                       INTERFACE / EDITING
;; ===================================================================

;; Interface
(menu-bar-mode -1)                  ;; Disable the menubar
(tool-bar-mode -1)                  ;; Disable the toolbar
;;(scroll-bar-mode -1)                ;; Disable the scrollbar
(global-display-line-numbers-mode)  ;; Enable line numbers
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
(setq indent-tabs-mode t)                           ;; use tabs instead of spaces
(global-superword-mode t)                           ;; symbol characters are part of a word

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
(add-hook 'before-save-hook
          (lambda ()
            (when (not (derived-mode-p 'ein:notebook-multilang-mode))
              (delete-trailing-whitespace))))

;; Searching
(setq-default case-fold-search t    ;; case insensitive searches by default
              search-highlight t)     ;; highlight matches when searching

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

;; Spellcheck
(setq ispell-program-name "hunspell")

;; ===================================================================
;; @                         General Bindings
;; ===================================================================

;; Unbindings
(global-unset-key (kbd "C-x u"))
(global-unset-key (kbd "C-z"))

;; Editing
(global-set-key (kbd "C-S-k") 'my-delete-line-backward) ; Ctrl+Shift+k
;;(global-set-key (kbd "C-k")   'my-delete-line)
(global-set-key (kbd "M-d")   'my-delete-word)
(global-set-key (kbd "<C-backspace>") 'my-backward-delete-word)
(global-set-key (kbd "<M-backspace>") 'my-backward-delete-word)

(global-set-key [M-up]   'move-text-up)
(global-set-key [M-down] 'move-text-down)

(global-set-key (kbd "C-/") 'undo)
(global-set-key (kbd "C-?") 'redo)

;; Navigation
(global-set-key (kbd "M-p") 'backward-paragraph);
(global-set-key (kbd "M-n") 'forward-paragraph);
(global-set-key (kbd "C-o") 'other-window)
(global-set-key "\M-t" 'ff-find-other-file)

;; Compilation
(global-set-key (kbd "<f5>") 'c-save-compile-run)
(global-set-key (kbd "<f6>") 'c-save-compile)
(global-set-key (kbd "<f7>") 'run-debug-build)
(global-set-key (kbd "C-=")  'next-error)
(global-set-key (kbd "C--")  'previous-error)

;; Misc
(global-set-key (kbd "<f8>")    'ispell-region)
(global-set-key (kbd "<f12>")   'visual-line-mode)


;; ===================================================================
;; @                       PCLP Modifications
;; ===================================================================

;;(find-file "c:/Users/ARomauld/Documents/notes.txt")
;;
;;(unless (package-installed-p 'clang-format)
;;  (package-install 'clang-format))
;;(require 'clang-format)
;;
;;(defun work-save-recompile ()
;;  (interactive)
;;  (save-buffer)
;;  (defvar _cwd)
;;  (setq _cwd default-directory)
;;  (recompile)
;;  (cd _cwd)
;;)
;;
;;(defun work-save-compile ()
;;  (interactive)
;;  (save-buffer)
;;  (defvar _cwd)
;;  (setq _cwd default-directory)
;;  (compile)
;;  (cd _cwd)
;;)
;;
;;(defun my-work-c-mode-common-hook ()
;;  (setq indent-tabs-mode      nil)
;;  (setq c++-tab-always-indent ni)
;;  )
;;
;;(add-hook 'c-mode-common-hook 'my-work-c-mode-common-hook)
;;(setq indent-tabs-mode nil)
;;
;;(global-set-key (kbd "C-<tab>") 'clang-format-region)
;;(global-set-key (kbd "<f5>")    'work-c-save-compile-run)
;;(global-set-key (kbd "<f6>")    'work-c-save-compile)







(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-ignored-server-capabilities
   '(:hoverProvider :documentFormattingProvider :documentRangeFormattingProvider :documentOnTypeFormattingProvider :foldingRangeProvider))
 '(package-selected-packages '(zenburn-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
