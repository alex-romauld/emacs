;; ===================================================================
;; @                       PCLP Modifications
;; ===================================================================

(use-package clang-format :ensure t)

(defun pclp/show-trailing-ws ()
	"Show trailing whitespace in the current buffer, unless it is read-only."
	(setq-local show-trailing-whitespace (not buffer-read-only)))

(defun pclp/compilation-mode-hook ()
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pclp_error . ("\\([A-Za-z0-9_-]+\.[A-Za-z]+\\)\s\s\\([0-9]+\\)\s\serror\s[0-9]+:" 1 2 nil)))
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pclp_warning . ("\\([A-Za-z0-9_-]+\.[A-Za-z]+\\)\s\s\\([0-9]+\\)\s\swarning\s[0-9]+:" 1 2 1)))
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pclp_info . ("\\([A-Za-z0-9_-]+\.[A-Za-z]+\\)\s\s\\([0-9]+\\)\s\sinfo\s[0-9]+:" 1 2 0)))
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pclp_note . ("\\([A-Za-z0-9_-]+\.[A-Za-z]+\\)\s\s\\([0-9]+\\)\s\snote\s[0-9]+:" 1 2 0)))
  (add-to-list 'compilation-error-regexp-alist 'pclp_error)
  (add-to-list 'compilation-error-regexp-alist 'pclp_warning)
  (add-to-list 'compilation-error-regexp-alist 'pclp_info)
  (add-to-list 'compilation-error-regexp-alist 'pclp_note))

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

(global-set-key (kbd "<f1>")  (lambda () (interactive) (find-file "C:/dev/notes.txt")))
(global-set-key (kbd "<f2>")  (lambda () (interactive) (find-file "/-:aromauld@vapvddev08.vi.vector.int#22:~/")))

(setq compile-command "MSBuild.exe build/LLVM.sln -target:pclp -property:Configuration=Debug -noLogo -m -v:m")
;; (setq compile-command "cmake --build build --target pclp --config Debug")

(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "<f6>") 'project-compile)
(global-set-key (kbd "<f7>") 'compile)

(find-file "c:/dev/notes.txt")
(message "pclp mode")
