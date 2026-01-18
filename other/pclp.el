;; ===================================================================
;; @                       PCLP Modifications
;; ===================================================================

(use-package clang-format :ensure t)

(defun pclp/show-trailing-ws ()
	"Show trailing whitespace in the current buffer, unless it is read-only."
	(setq-local show-trailing-whitespace (not buffer-read-only)))

(defun pclp/project-compile ()
  "Compile PC-lint Plus."
  (interactive)
  (setq compile-command "MSBuild.exe build/LLVM.sln -target:pclp -property:Configuration=Debug -noLogo -m -v:m")
  ;; (setq compile-command "cmake --build build --target pclp --config Debug")
  (project-compile))

(defun pclp/compilation-mode-hook ()
  ;; TYPE is 2 or nil for a real error or 1 for warning or 0 for info.
  ;; next-error does not jump to info messages, so pclp info and note messages are treated as warnings
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pclp_error . ("\\([A-Za-z0-9_-]+\.[A-Za-z]+\\)\s\s\\([0-9]+\\)\s\serror\s[0-9]+:"       1 2 nil 2)))
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pclp_warning . ("\\([A-Za-z0-9_-]+\.[A-Za-z]+\\)\s\s\\([0-9]+\\)\s\swarning\s[0-9]+:"   1 2 nil 1)))
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pclp_info . ("\\([A-Za-z0-9_-]+\.[A-Za-z]+\\)\s\s\\([0-9]+\\)\s\sinfo\s[0-9]+:"         1 2 nil 1)))
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pclp_note . ("\\([A-Za-z0-9_-]+\.[A-Za-z]+\\)\s\s\\([0-9]+\\)\s\snote\s[0-9]+:"         1 2 nil 1)))
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pclp_supl . ("\\([A-Za-z0-9_-]+\.[A-Za-z]+\\)\s\s\\([0-9]+\\)\s\ssupplemental\s[0-9]+:" 1 2 nil 0)))
  (add-to-list 'compilation-error-regexp-alist 'pclp_error)
  (add-to-list 'compilation-error-regexp-alist 'pclp_warning)
  (add-to-list 'compilation-error-regexp-alist 'pclp_info)
  (add-to-list 'compilation-error-regexp-alist 'pclp_note)
  (add-to-list 'compilation-error-regexp-alist 'pclp_supl))

(remove-hook 'before-save-hook       'delete-trailing-whitespace)
(add-hook    'post-command-hook      'pclp/show-trailing-ws)
(add-hook    'compilation-mode-hook  'pclp/compilation-mode-hook)

(setq-default indent-tabs-mode nil)             ; use space for indentation
(smart-tabs-mode nil)

(global-unset-key (kbd "<f5>"))
(global-unset-key (kbd "<f6>"))
(global-unset-key (kbd "<f7>"))
(global-unset-key (kbd "S-<f5>"))
(global-unset-key (kbd "S-<f6>"))
(global-unset-key (kbd "S-<f7>"))

(define-key universal-keymap (kbd "C-c C-w") 'whitespace-cleanup)
(global-set-key              (kbd "C-c C-f") 'clang-format-region)

(global-set-key (kbd "<f1>")  (lambda () (interactive) (find-file "C:/dev/notes.md")))
(global-set-key (kbd "<f2>")  (lambda () (interactive) (find-file "/-:aromauld@vapvddev08.vi.vector.int#22:~/")))

(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "<f6>") 'pclp/project-compile)
(global-set-key (kbd "<f7>") 'compile)

(find-file "c:/dev/notes.md")
(message "pclp mode")
