;; https://ianyepan.github.io/posts/setting-up-use-package/
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
	; speedup startup time by only loading use-packages when needed
	use-package-always-defer t
	; set to t for debugging 
	use-package-verbose nil
        use-package-expand-minimally t))

(eval-when-compile
  (require 'use-package))

;;(require 'diminish)               ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; generate statistics with M-x use-package-report
(setq use-package-compute-statistics t)

;;
;; end use-package stuff
;;

;; load and reload config file
(defun safferli/config-visit ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c e") 'safferli/config-visit)

(defun safferli/config-reload ()
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c r") 'safferli/config-reload)




;; (setq initial-frame-alist
;;       (append initial-frame-alist
;;               '((left   . ???)
;;                 (top    . ???)
;;                 (width  . ???)
;;                 (height . ???))))

;; or this? 
;; (dolist (var '(default-frame-alist initial-frame-alist))
;;   (add-to-list var '(width . (text-pixels . 1200)))
;;   (add-to-list var '(height . (text-pixels . 900))))
;; (setq frame-resize-pixelwise t
;;       frame-inhibit-implied-resize t)




;; emoji support
(use-package emojify)

(use-package ido
  :ensure t
  :init
  (ido-mode))

;; (use-package ido-vertical-mode
;;   :requires ido
;;   :config
;;   (setq ido-vertical-show-count t)
;;   (ido-vertical-mode))

;; (setq ido-vertical-show-count t)
;; (setq ido-use-faces t)
;; (set-face-attribute 'ido-vertical-first-match-face nil
;;                     :background "#e5b7c0")
;; (set-face-attribute 'ido-vertical-only-match-face nil
;;                     :background "#e52b50"
;;                     :foreground "white")
;; (set-face-attribute 'ido-vertical-match-face nil
;;                     :foreground "#b00000")
;; (ido-vertical-mode 1)


;; highlight current line
(when window-system (add-hook 'prog-mode-hook 'hl-line-mode))

;; yaml mode
(use-package yaml-mode
  :mode "\\.yml\\'")

;; markdown mode
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . auto-fill-mode)
  :custom-face (markdown-code-face ((t (:inherit org-block)))))

;; adoc-mode
(use-package adoc-mode
  :mode "\\.\\(adoc\\|asciidoc\\)\\'"
  :hook
  (adoc-mode . visual-line-mode)
					;(adoc-mode . variable-pitch-mode)
  )

; https://stackoverflow.com/questions/12756531/using-the-current-buffers-file-name-in-m-x-compile 
(add-hook 'adoc-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
		 (concat "asciidoctor " (shell-quote-argument buffer-file-name))
                 )))




;; uniquify package

;; unfill (opposite of fill)
; rebind M-q to toogle fill/unfill
(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))


;; expand semantically depending on mode/region
(use-package expand-region
  :bind ("C-=" . er/expand-region))


;; (custom-set-variables
;;   ; put all backups to this dir: ~/.emacs.d/backups/
;;  '(backup-directory-alist `(("." \, (concat user-emacs-directory "backups"))))
;; )

;; put all backups (~ files) in a backup dir
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; set default encoding
(set-language-environment "UTF-8")
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; proxy woes
(defun safferli/set-proxy ()
  (interactive)
  (customize-set-variable 'url-proxy-services
                          '(("http"  . "proxy.ubisoft.org:3218")
                            ("https" . "proxy.ubisoft.org:3218"))))
(defun safferli/unset-proxy ()
  (interactive)
  (customize-set-variable 'url-proxy-services nil))


;; line numbers from emacs-26 onwards
(use-package display-line-numbers
    :defer nil
    :ensure nil
    :config
    (global-display-line-numbers-mode))

; start a command and wait, then this will show the completions available:
(use-package which-key
  :ensure t
  ;:defer nil
  :init
  (which-key-mode))
;  (add-hook 'after-init-hook 'which-key-mode))

; avy should be amazing(?)
; https://github.com/abo-abo/avy

; hex colour code will be highlighted in the corresponding colour
(use-package rainbow-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; highlight matching parens (ensure nil, because this is a core package)
(use-package paren
  :ensure nil
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode +1))

;; colour-code delimiter depth
(use-package rainbow-delimiters
  :ensure t
  :defer nil
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; flycheck
;;(use-package flycheck
;;  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spelling                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; tell ispell that apostrophes are part of words
;; ;; and select Bristish dictionary
;; (setq ispell-local-dictionary-alist
;;       `((nil "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_GB") nil utf-8)))

;; On OS X/Darwin, make sure we add the path to the homebrew installs
(when (string-equal system-type "darwin")
  (setq exec-path (append exec-path '"/opt/homebrew/bin")))
;; or directly push aspell bin 
;; (setq ispell-program-name "/opt/local/bin/aspell")
;; for macOS, this loads all PATH variables 
;; (package-install 'exec-path-from-shell)
;; (exec-path-from-shell-initialize)

(when (executable-find "aspell")
  (setq-default ispell-program-name "aspell")
  (setq ispell-really-aspell t))

;; needed if using aspell, because -l means "language" there
;; (setq ispell-list-command "--list")

(use-package flyspell
  :init
  (progn
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (add-hook 'text-mode-hook 'flyspell-mode)
    )
  ;(flyspell-mode 1)
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-list-command "--list") ;; run flyspell with aspell, not ispell
  ;; Sets flyspell correction to use two-finger mouse click
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
  )


(defun safferli/flyspell-german ()
  (interactive)
  (ispell-change-dictionary "german")
  (flyspell-buffer))

(defun safferli/flyspell-english ()
  (interactive)
  (ispell-change-dictionary "default")
  (flyspell-buffer))




;; magit - git
(use-package magit
  :ensure t
  :config
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  :bind
  ("M-g" . magit-status))

;; make file executable if first line has shebang
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; remove toolbar
(if window-system
    (tool-bar-mode -1)
    )

;; zenburn theme
(use-package zenburn-theme
  :ensure t
  :defer nil
  :config
  (load-theme 'zenburn t))

;; LaTeX - AucTeX
(use-package latex
  :defer t 
  :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :init
  (setq reftex-plug-into-AUCTeX t)
  (setenv "PATH" (concat "/Library/TeX/texbin:"
                         (getenv "PATH")))
  (add-to-list 'exec-path "/Library/TeX/texbin")
  ;; :mode "\\.\\(tex\\|ltx\\)\\'"
  :bind
  (:map LaTeX-mode-map
	("C-c C-a" . 'align-current))
  )
;; (add-hook 'LaTeX-mode-hook #'turn-on-auto-fill)

;; try this sometime: https://www.gnu.org/software/auctex/manual/auctex/Control.html


;; not all modes do this
(global-set-key "\C-c;" 'comment-region)
;; quick way to get to top/bottom
(global-set-key [home] 'beginning-of-buffer) 
(global-set-key [end]  'end-of-buffer) 

;; https://pragmaticemacs.wordpress.com/category/editing/ 
(defun safferli/align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun safferli/align-ampersand (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))

;; join line to next line
(global-set-key (kbd "C-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))

; https://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun safferli/show-buffer-file-name ()
  "Show the full path to the current file in the minibuffer and copy it to the kill ring."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))




;; remove auto-updated custom-set-variables
;; https://www.reddit.com/r/emacs/comments/4x655n/packageselectedpackages_always_appear_after/
(setq custom-file "~/.emacs.d/package-selected-packages.el")
(load custom-file)

;; misc topics

;; If you use M-! (shell-command) and then press M-n
;; (next-history-element), the current file name will be inserted into
;; the minibuffer with point before it. Then you just need to type the
;; command you want to run and hit return.



;; https://protesilaos.com/emacs/dotemacs
;; ;;;###autoload
;; (defun prot-common-reverse-percentage (number percent change-p)
;;   "Determine the original value of NUMBER given PERCENT.

;; CHANGE-P should specify the increase or decrease.  For simplicity,
;; nil means decrease while non-nil stands for an increase.

;; NUMBER must satisfy `numberp', while PERCENT must be `natnump'."
;;   (unless (numberp number)
;;     (user-error "NUMBER must satisfy numberp"))
;;   (unless (natnump percent)
;;     (user-error "PERCENT must satisfy natnump"))
;;   (let* ((pc (/ (float percent) 100))
;;          (pc-change (if change-p (+ 1 pc) pc))
;;          (n (if change-p pc-change (float (- 1 pc-change)))))
;;     ;; FIXME 2021-12-21: If float, round to 4 decimal points.
;;     (/ number n)))

;; ;;;###autoload
;; (defun prot-common-percentage-change (n-original n-final)
;;   "Find percentage change between N-ORIGINAL and N-FINAL numbers.

;; When the percentage is not an integer, it is rounded to 4
;; floating points: 16.666666666666664 => 16.667."
;;   (unless (numberp n-original)
;;     (user-error "N-ORIGINAL must satisfy numberp"))
;;   (unless (numberp n-final)
;;     (user-error "N-FINAL must satisfy numberp"))
;;   (let* ((difference (float (abs (- n-original n-final))))
;;          (n (* (/ difference n-original) 100))
;;          (round (floor n)))
;;     ;; FIXME 2021-12-21: Any way to avoid the `string-to-number'?
;;     (if (> n round) (string-to-number (format "%0.4f" n)) round)))
