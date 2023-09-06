;;; init.el --- -*- lexical-binding: t; -*-

;;
;; 00 Table of Contents
;;

; (occur "^;; [0-9]+")

;;
;; 01 package management
;; 

;; https://ianyepan.github.io/posts/setting-up-use-package/
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; TODO Emacs 29 will have use-package as part of core
;; TODO Emacs 30 integrates vc: into use-package: https://tony-zorman.com/posts/use-package-vc.html
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

(require 'diminish)               ;; if you use :diminish
(require 'bind-key)               ;; if you use any :bind variant

;; generate statistics with M-x use-package-report
(setq use-package-compute-statistics t)

;; emacs startup profiler -- errors out
;; (use-package esup)


;;
;; 02 name and email
;;

(setq user-full-name "Christoph Safferling")
(setq user-mail-address "christoph.safferling@ubisoft.com")


;;
;; 10 appearance
;;

;; "open with" in the original frame, not a new one 
;; https://superuser.com/questions/277755/emacs-opens-files-in-a-new-frame-when-opened-with-open-a
(setq ns-pop-up-frames nil)


(use-package dashboard
  :config
  (setq dashboard-startup-banner 'logo)
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5)
                        ;;(bookmarks . 5)
                        (projects . 5)
                        ;;(agenda . 5)
                        ;;(registers . 5)
			))
  :hook ((after-init . dashboard-refresh-buffer)))


;; version 6.0 is now in beta, using the svg branch: https://github.com/domtronn/all-the-icons.el/tree/svg
;; TODO keep an eye out for when it's stable! 
;; 
;; run M-x all-the-icons-install-fonts to install the fonts
;; doom-modeline also needs M-x nerd-icons-install-fonts
(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; (use-package all-the-icons-completion
;;   :defer
;;   :hook
;;   (marginalia-mode . #'all-the-icons-completion-marginalia-setup)
;;   :init
;;   (all-the-icons-completion-mode))


;; emoji support
(use-package emojify
  :hook (after-init . global-emojify-mode)
  )


;; titlebar with full path to buffer (%f)
(setq-default frame-title-format "%b (%f)")


;; highlight current line
(when window-system (add-hook 'prog-mode-hook #'hl-line-mode))

;; remove toolbar
(if window-system
    (tool-bar-mode -1)
  )


;; Give a pulse light when switching windows, or switching focus to
;; the minibuffer.
(require 'pulse)
(set-face-attribute 'pulse-highlight-start-face nil :background "#09055f")
(add-hook 'window-selection-change-functions
          (lambda (frame)
            (when (eq frame (selected-frame))
              (pulse-momentary-highlight-one-line))))

;; (use-package pulse
;;   ;; Highlight cursor postion after movement
;;   :unless my/is-terminal
;;   :defer t
;;   :init (defun pulse-line (&rest _)
;;           (pulse-momentary-highlight-one-line (point)))
;;   (dolist (command '(other-window
;;                      windmove-do-window-select
;;                      mouse-set-point
;;                      mouse-select-window))
;;     (advice-add command :after #'pulse-line)))



;; size and position on initial open
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


;; new windows open at 2/3rds and switching windows changes their size
(use-package golden-ratio
  :diminish
  :init
  (golden-ratio-mode)
  :custom
  (golden-ratio-exclude-modes '(occur-mode))
   )


;; breaks scroll (perhaps because on mac?)
;; (use-package centered-cursor-mode
;;   :diminish centered-cursor-mode)
;; or this?
;; https://github.com/jmercouris/emacs-centered-point


;;
;; 11 themes
;;

;; zenburn theme
(use-package zenburn-theme
  :defer nil
  :config
  (load-theme 'zenburn t))


;;
;; 12 modeline
;; 

;; modeline -- should at some point just pull out what I want
(setq column-number-mode t)
(setq inhibit-compacting-font-caches t)

(use-package doom-modeline
  ;;:after all-the-icons
  :defer nil
  :config
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-unicode-fallback t)
  (setq doom-modeline-height 25)
  :hook
  (after-init . doom-modeline-mode))

;; TODO un-doomify
;; https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
;; https://github.com/Gavinok/emacs.d/blob/main/lisp/modeline.el
;; master branch now (2023-04-01) has this:
;; Modeline elements can now be right-aligned. Anything following the
;; symbol 'mode-line-format-right-align' in 'mode-line-format' will be
;; right-aligned. Exactly where it is right-aligned to is controlled
;; by the new user option 'mode-line-right-align-edge'.


;;
;; 20 my functions
;;

;; load and reload config file
(defun safferli/config-visit ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c i e") 'safferli/config-visit)

(defun safferli/config-reload ()
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c i r") 'safferli/config-reload)

;; proxy woes
(defun safferli/set-proxy ()
  (interactive)
  (customize-set-variable 'url-proxy-services
                          '(("http"  . "proxy.ubisoft.org:3218")
                            ("https" . "proxy.ubisoft.org:3218"))))
(defun safferli/unset-proxy ()
  (interactive)
  (customize-set-variable 'url-proxy-services nil))


(defun safferli/insert-date ()
  (interactive)
  (let (( time (current-time-string) ))
    (insert (format-time-string "%Y-%m-%d"))))
(global-set-key (kbd "C-c i d") 'safferli/insert-date)


;; none of these currently work because of IT restrictions
(defun safferli/get-current-url-edge ()
  (do-applescript "tell application \"Microsoft Edge\" to return URL of active tab of front window"))

(defun safferli/get-current-title-edge ()
  (do-applescript "tell application \"Microsoft Edge\" to return Title of active tab of front window"))

(defun safferli/get-current-url-firefox ()
  (do-applescript "tell application \"Firefox\" to activate
                   tell application \"System Events\"
                     keystroke \"l\" using command down keystroke
                               \"c\" using command down key
                               code 53 -- esc key
                   end tell
                   delay 0.1
                   return the clipboard"))

;; tell application "System Events" to tell process "Firefox"
;; 	set frontmost to true
;; 	set the_title to name of windows's item 1
;; end tell

;; If you're running GUI Emacs on macOS (AKA Emacs NeXTStep port),
;; don't forget about ns-do-applescript, which runs AppleScript
;; directly from Emacs. It can be a lot faster than shelling out to
;; osascript


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


;; https://stackoverflow.com/questions/17958397/emacs-delete-whitespaces-or-a-word
(defun safferli/kill-whitespace-or-word ()
  (interactive)
  (if (looking-at "[ \t\n]")
      (let ((p (point)))
        (re-search-forward "[^ \t\n]" nil :no-error)
        (backward-char)
        (kill-region p (point)))
    (kill-word 1)))
(global-set-key (kbd "C-<delete>") 'safferli/kill-whitespace-or-word)

(defun safferli/turn-off-line-numbers ()
  "Disable line numbering in the current buffer."
  (display-line-numbers-mode -1))




;;
;; 25 global settings 
;;

;; https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/ 
(setq ad-redefinition-action 'accept)


;; M-x world-clock
(setq display-time-world-list
  '(("Etc/UTC" "UTC")
    ("Europe/Paris" "Paris")
    ("America/Montreal" "Montreal") 
    ("Asia/Shanghai" "Chengdu")
    ("America/Los_Angeles" "San Francisco")
    ))
;;(setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")


;; put all backups (~ files) in a backup dir
(defvar --backup-directory (concat user-emacs-directory "backups/")) ; does this need trailing / ?
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t    ; backup of a file the first time it is saved
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 10   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
)


;; remove auto-updated custom-set-variables
;; https://www.reddit.com/r/emacs/comments/4x655n/packageselectedpackages_always_appear_after/
(setq custom-file "~/.emacs.d/package-selected-packages.el")
(load custom-file)


;; set default encoding
(set-language-environment "UTF-8")
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)


;; line numbers from emacs-26 onwards
(use-package display-line-numbers
    :defer nil
    :ensure nil
    :config
    (global-display-line-numbers-mode))



;; opens a macOS X finder in the current folder 
(use-package reveal-in-osx-finder
  :bind
  ("C-c f" . reveal-in-osx-finder)) 


;; Navigate between visible buffers (windows in emacs speak) (thanks skybert)
(defun safferli/other-window-backward (&optional n)
  (interactive "p")
  (if n
      (other-window (- n))
    (other-frame -1)))
(global-set-key "\C-x\C-n" 'other-window)
(global-set-key "\C-x\C-p" 'safferli/other-window-backward)


;; reload dired buffers
(setq dired-auto-revert-buffer t)
;; reload if files have changed on disk
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)


;; not all modes do this
(global-set-key "\C-c;" 'comment-region)
(global-set-key "\C-c:" 'uncomment-region)
;; quick way to get to top/bottom
(global-set-key [home] 'beginning-of-buffer) 
(global-set-key [end]  'end-of-buffer) 
;; join line to next line
(global-set-key (kbd "C-c j")
		(lambda ()
                  (interactive)
                  (join-line -1)))


; start a command and wait, then this will show the completions available:
(use-package which-key
  :diminish
  :init
  (which-key-mode)
  ;; :config
  ;; (setq which-key-idle-delay 0.2)
  )


;; working with buffers
;; https://www.youtube.com/watch?v=Hql1ySbTGbE
;; remove emacs-internal buffers from buffer screen
(set-frame-parameter (selected-frame) 'buffer-predicate
                     (lambda (buf) 
                       (let ((name (buffer-name buf)))
                         (not (or (string-prefix-p "*" name)
                                  (eq 'dired-mode (buffer-local-value 'major-mode buf)))))))

(global-set-key (kbd "C-`") 'mode-line-other-buffer)
(global-set-key (kbd "C-1") 'kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; remove ^M trailing CR from windows/samba files
(global-set-key (kbd "C-c 5") 'delete-trailing-whitespace)


;;
;; 29 fun stuff
;;

;; M-x dad-joke
(use-package dad-joke)


;;
;; 30 search
;;

;; display a counter showing the number of the current and the total
;; matches. Place it before the prompt.
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)
;; see the preceding and following lines around a match
(setq list-matching-lines-default-context-line 0)

;; search defaults to selected region
;; https://stackoverflow.com/questions/202803/searching-for-marked-selected-text-in-emacs
(defun safferli/isearch-with-region ()
  "Use region as the isearch text."
  (when mark-active
    (let ((region (funcall region-extract-function nil)))
      ;; start at the beginning to get this occurrence to show up as first  
      (goto-char (region-beginning))
      (deactivate-mark)
      (isearch-update)
      (isearch-yank-string region))))

(add-hook 'isearch-mode-hook #'safferli/isearch-with-region)


(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	;; read-buffer-completion-ignore-case t
	))

(use-package marginalia
  :init
  (marginalia-mode))



;; (use-package ido
;;   :ensure t
;;   :init
;;   (ido-mode)
;;   )

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


;; ivy, counsel, swiper
;; ivy-rich 


;;
;; 31 spelling
;;

;; ;; tell ispell that apostrophes are part of words
;; ;; and select Bristish dictionary
;; (setq ispell-local-dictionary-alist
;;       `((nil "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_GB") nil utf-8)))

;; On OS X/Darwin, make sure we add the path to the homebrew installs
;; (when (string-equal system-type "darwin")
;;   (setq exec-path (append exec-path "/opt/homebrew/bin")))
;; or directly push aspell bin 
;; (setq ispell-program-name "/opt/local/bin/aspell")
;; for macOS, this loads all PATH variables 
;; (package-install 'exec-path-from-shell)
;; (exec-path-from-shell-initialize)

;; https://emacs.stackexchange.com/questions/76920/cannot-update-melpa-m-x-package-refresh-contents?noredirect=1#comment127700_76920
;; exec-path, not strictly necessary, but doesn't hurt
;; load-path, use cmake's cmake-mode.el not emacs's
(cond ((string-match-p "aarch64-apple" system-configuration)
       (add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/cmake")
       (add-to-list 'exec-path "/opt/homebrew/bin"))
      ((string-match-p "x86_64-apple" system-configuration)
       (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/cmake")
       (add-to-list 'exec-path "/usr/local/bin")))


(when (executable-find "aspell")
  (setq-default ispell-program-name "aspell")
  (setq ispell-really-aspell t))


;; (setq ispell-list-command "--list")

(use-package flyspell
  :init
  (progn
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (add-hook 'text-mode-hook 'flyspell-mode)
    )
  ;(flyspell-mode 1)
  :config
  (setq ispell-program-name "aspell") ; run flyspell with aspell, not ispell
  (setq ispell-list-command "--list") ; needed if using aspell, because -l means "language" there
  ;; Sets flyspell correction to use two-finger mouse click
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
  )


(defun safferli/flyspell-german ()
  (interactive)
  (ispell-change-dictionary "german")
  (flyspell-buffer))

(defun safferli/flyspell-english ()
  (interactive)
  (ispell-change-dictionary "en_GB-ise-w_accents")
  (flyspell-buffer))


;;
;; 35 text editing
;;

;; relics of the olden days...
(setq sentence-end-double-space t)
;; I actually like the default (nil):
; (setq delete-selection-mode t)


(use-package multiple-cursors
  :ensure t
  :bind (("C-c m ." . mc/mark-next-like-this)
         ("C-c m ," . mc/unmark-next-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("C-c m e" . mc/edit-lines)))


;; unfill (opposite of fill)					
(use-package unfill
  ;; rebind M-q to toogle fill/unfill
  :bind ([remap fill-paragraph] . unfill-toggle))


;; expand semantically depending on mode/region
(use-package expand-region
  :bind (("C-c x r" . er/expand-region)
	 ("C-c x q" . er/mark-inside-quotes)
	 ("C-c x w" . er/mark-outside-quotes)
	 ("C-c x p" . er/mark-inside-pairs)
	 ))


;; also check out this for more: https://protesilaos.com/codelog/2020-07-16-emacs-focused-editing/

;; Distraction-free screen
(use-package olivetti
  :ensure t 
  :init
  (setq olivetti-body-width .67)
  :config
  (defun distraction-free ()
    "Distraction-free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
        (progn
          (window-configuration-to-register 1)
          (delete-other-windows)
          (text-scale-increase 2)
	  ;; fake center-cursor-mode:
	  ;; center cursor and then activate scroll-lock-mode 
	  (recenter)
	  (scroll-lock-mode 1)
          (olivetti-mode t))
        (progn
          (jump-to-register 1)
	  (scroll-lock-mode 0)
          (olivetti-mode 0)
          (text-scale-decrease 2))))
  :bind (("C-<f9>" . distraction-free)
	 ("<f9>" . olivetti-mode)))


(use-package mw-thesaurus
  :config
  ;; open in right
  (add-to-list 'display-buffer-alist
   `(,mw-thesaurus-buffer-name
     (display-buffer-reuse-window
      display-buffer-in-direction)
     (direction . right)
     (window . root)
     (window-width . 0.3)))
  ;; :hook
  ;; (mw-thesaurus-mode . variable-pitch-mode)
  :bind
  ("C-c t" . mw-thesaurus-lookup-dwim)
  )


;; ;; do I really want this? 
;; (use-package puni
;;   :defer t
;;   :hook ((prog-mode sgml-mode) . puni-mode))

;; ace-window for window management
;; https://github.com/abo-abo/ace-window


;;
;; 40 productivity
;;

;;
;; 41 Treesitter
;; 

(use-package treesit
  :ensure nil
  :when (treesit-available-p)
  :init
  (setq treesit-language-source-alist
	'((bash "https://github.com/tree-sitter/tree-sitter-bash")
	  (cmake "https://github.com/uyha/tree-sitter-cmake")
	  (css "https://github.com/tree-sitter/tree-sitter-css")
	  (elisp "https://github.com/Wilfred/tree-sitter-elisp")
	  (go "https://github.com/tree-sitter/tree-sitter-go")
	  (html "https://github.com/tree-sitter/tree-sitter-html")
	  (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	  (json "https://github.com/tree-sitter/tree-sitter-json")
	  (lua "https://github.com/Azganoth/tree-sitter-lua")
	  (make "https://github.com/alemuller/tree-sitter-make")
	  ;;(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	  (markdown "https://github.com/MDeiml/tree-sitter-markdown")
	  (python "https://github.com/tree-sitter/tree-sitter-python")
	  (sql "https://github.com/m-novikov/tree-sitter-sql")
	  (toml "https://github.com/tree-sitter/tree-sitter-toml")
	  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	  (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  (add-to-list 'major-mode-remap-alist
	       '(yaml-mode . yaml-ts-mode))
  )


;;
;; 42 eglot LSP
;;

;; (use-package eglot
;;   :bind (:map eglot-mode-map
;; 	      ("C-c d" . eldoc)
;; 	      ("C-c a" . eglot-code-actions)
;; 	      ("C-c r" . eglot-rename))
;;   )


;;
;; 43 Projectile
;; 


(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/github")
    (setq projectile-project-search-path '("~/github")))
  (setq projectile-switch-project-action #'projectile-dired)
  :bind (:map projectile-mode-map
	      ;; ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  )

;; (setq projectile-project-search-path '("~/projects/" "~/work/" ("~/github" . 1)))


;; After adding or updating a snippet run:
;; =M-x yas-recompile-all=
;; =M-x yas-reload-all=
(use-package yasnippet
  :config
  ;; (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (setq-default yas-snippet-dirs `(,(expand-file-name "snippets/" user-emacs-directory)))
  (yes-reload-all)
  (yas-global-mode 1)
  )


;; https://github.com/Wilfred/helpful
;; (helpful-at-point)

;; https://ianyepan.github.io/posts/emacs-git-gutter/
;; or use this? https://github.com/dgutov/diff-hl

;; need to figure out the browser database directory and homebrew first... 
;; ;; interact with your browser via emacs
;; (use-package ibrowse
;;   :bind
;;   ("C-c b c" . ibrowse-tab-copy-url)
;;   ("C-c b l" . ibrowse-tab-insert-markdown-link)
;;   )


;; regexp magic
;; https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder 
(use-package re-builder
  :ensure nil 
  :config
  (setq reb-re-syntax 'string)
  )


;; transpose stuff -- including sexp!
;; http://yummymelon.com/devnull/moving-text-elegantly-in-emacs.html
(defun cc/move-sexp-backward ()
  "Move balanced expression (sexp) to the right of point backward one sexp.
Point must be at the beginning of balanced expression (sexp)."
  (interactive)
  (transpose-sexps 1)
  (forward-sexp -2))

(defun cc/move-sexp-forward ()
  "Move balanced expression (sexp) to the right of point forward one sexp.
Point must be at the beginning of balanced expression (sexp)."
  (interactive)
  (forward-sexp 1)
  (transpose-sexps 1)
  (forward-sexp -1))


;; https://github.com/dmgerman/editWithEmacs.spoon

;; If the hammerspoon configuration is present, and so is this spoon, load it
(if (file-readable-p "~/.hammerspoon/Spoons/editWithemacs.spoon/hammerspoon.el")
    (let ((load-path (cons "~/.hammerspoon/Spoons/editWithemacs.spoon" load-path )))
      (require 'hammerspoon)))



;;
;; 45 translation
;;

;; https://commission.europa.eu/resources-partners/etranslation_en
;; https://github.com/lorniu/go-translate
;; https://github.com/mtenders/emacs-leo
;; improved version: https://codeberg.org/martianh/emacs-leo

;; https://github.com/dmgerman/yomikun


;;
;; 50 text buffers
;;

;;
;; 51 markdown mode
;;

;; markdown mode
(use-package markdown-mode 
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . auto-fill-mode)
  ;; :bind ( :map markdown-mode-map
  ;;         ("M-Q" . split-pararagraph-into-lines))
  :custom-face (markdown-code-face ((t (:inherit org-block)))))


;;
;; 52 ASCIIdoc mode
;;

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

;; https://hyperscope.link/3/7/1/5/8/Convenient-Emacs-Lisp-functions-for-Asciidoctor-37158.html
(defvar rcd-asciidoctor-admonition-history nil)

(defun rcd-asciidoctor-admonition ()
  "Interactively enter Asciidoctor admonition.
Use prefix key to enter admonition block."
  (interactive)
  (let* ((admonitions '("NOTE" "TIP" "IMPORTANT" "CAUTION" "WARNING"))
	 (completion-ignore-case t)
	 (admonition (completing-read "Choose admonition: " admonitions nil t nil 'rcd-asciidoctor-admonition-history)))
    (if current-prefix-arg
	(progn
	  (insert (format "[%s]\n.%s\n====\n\n====\n\n" admonition
			  (read-from-minibuffer "Title: ")))
	  (previous-line 3))
      (insert (format "%s: " admonition)))))




;;
;; 53 LaTeX
;;

(use-package latex
  :defer t 
  :ensure auctex; since package and use-package use different name
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



;;
;; 54 Obsidian 
;;

(use-package obsidian
  :demand t
  :config
  (obsidian-specify-path "~/Onedrive - Ubisoft/obsidian-notes/")
  (global-obsidian-mode t)
  :custom
  ;; This directory will be used for `obsidian-capture' if set.
  (obsidian-inbox-directory "001 Zettelkasten")
  :bind (:map obsidian-mode-map
  ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
  ("C-c C-o" . obsidian-follow-link-at-point)
  ;; Jump to backlinks
  ("C-c C-b" . obsidian-backlink-jump)
  ;; If you prefer you can use `obsidian-insert-link'
  ("C-c C-l" . obsidian-insert-wikilink)))


;;
;; 55 pdf-tools
;;

(use-package pdf-tools
  :ensure t
  :pin manual ;; don't reinstall when package updates
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (pdf-tools-install :no-query)
  (require 'pdf-occur)
  :hook
  (pdf-view-mode . safferli/turn-off-line-numbers)
  )


;; TODO compile magic: https://emacs.stackexchange.com/questions/31493/print-elapsed-time-in-compilation-buffer/56130#56130
;; (defun tkj/goto-compilation()
;;   (interactive)
;;   (switch-to-buffer
;;    (get-buffer-create "*compilation*")))
;; (global-set-key (kbd "C-c c") 'tkj/goto-compilation)


;; TODO jira
;; (defun tkj/jira-key-at-point-to-link-on-kill-ring()
;;   "Creates a Jira link out of the issue key at point. The function
;; then inserts it into your kill ring so you can paste/yank it
;; where you need it."
;;   (interactive)
;;   (let ((issue (thing-at-point 'filename 'no-properties)))
;;     (kill-new (concat "https://jira.stibodx.com/browse/" issue))))

;; ;; Open Jira issue references in the browser
;; (setq bug-reference-bug-regexp "\\b\\(\\([A-Za-z][A-Za-z0-9]\\{1,10\\}-[0-9]+\\)\\)"
;;       bug-reference-url-format "https://jira.stibodx.com/browse/%s")
;; (add-hook 'org-mode-hook 'bug-reference-mode)




;; magit - git
(use-package magit
  :config
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  :bind
  ("M-g" . magit-status))


;;
;; 60 programming modes
;;

;; make file executable if first line has shebang
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


;; hex colour code will be highlighted in the corresponding colour
(use-package rainbow-mode
  ;;:delight
  :diminish
  :hook ((prog-mode text-mode) . rainbow-mode))


;; colour-code delimiter/parens depth
(use-package rainbow-delimiters
  :defer nil
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; colour highlight everything
;; https://github.com/alphapapa/prism.el 


;; highlight matching parens (ensure nil, because this is a core package)
(use-package paren
  :ensure nil
  :init
  (setq show-paren-delay 0)
  :config
  (setq show-paren-style 'mixed)
  (show-paren-mode +1))


;; this has a lot of examples:
;; https://ebzzry.com/en/emacs-pairs/#installation
(use-package smartparens
  :init
  :hook
  (prog-mode . smartparens-mode)
  (emacs-lisp-mode . smartparens-strict-mode)
  :config
  (progn
    ;; load defaults
    (require 'smartparens-config)
    (setq sp-show-pair-from-inside t)
    )
  ;; :diminish smartparens-mode
  :custom-face
  (sp-show-pair-match-face ((t (:foreground "Red")))) ;; Could also have :background "Grey" for example.
  )


;; yaml mode
(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.yml\\'")


;; lua mode 
(use-package lua-mode)


;; Paradox games
(add-to-list 'load-path "~/.emacs.d/my-packages/paradox.el")
(require 'paradox)
;; Spellchecking in comments and strings
(add-hook 'paradox-mode-hook 'flyspell-prog-mode)


(use-package company
  :config
  (setq company-idle-delay 0.1
	company-miimum-prefix-length 1)
  )


;; https://erick.navarro.io/blog/testing-an-api-with-emacs-and-restclient/
(use-package restclient
  :mode (("\\.http\\'" . restclient-mode))
  )



;;(use-package flycheck
;;  :ensure t)


;;
;; 61 Python
;;

;; http://www.skybert.net/emacs/programming-python-in-emacs-2015/
;; is there a more modern setup by now?
;; https://realpython.com/emacs-the-best-python-editor/

;; ;; https://youtu.be/SbTzIt6rISg Brent Westbrook
;; (use-package python
;;   :hook ((python-ts-mode . eglot-ensure)
;; 	 (pyhton-ts-mode . company-mode))
;;   :bind (:map python-ts-mode-map
;; 	      ("<f5>" . recompile)
;; 	      ("<f6>" . eglot-format))
;;   :mode (("\\.py\\'" . python-ts-mode))
;;   )

;; (use-package conda
;;   :config
;;   (setq conda-env-home-directory
;; 	(expand-file-name "~/whatever"))
;;   )

;; (use-package highlight-indent-guides
;;   :hook (python-ts-mode . highlight-indent-guides-mode)
;;   :config
;;   (set-face-foreground 'highlight-indent-guides-character-face "white")
;;   (setq highlight-indent-guides-method 'character)
;;   )


;;
;; 62 Rust
;;

;; https://robert.kra.hn/posts/rust-emacs-setup/
;; https://www.unwoundstack.com/blog/emacs-as-a-rust-ide.html
;; gavinok https://github.com/Gavinok/emacs.d/blob/main/init.el
;; https://youtu.be/omJhc7zprNs

;; (use-package rust-mode    :ensure t :mode "\\.rs\\'"
;;   :init
;;   ;; scratchpad for rust
;;   (setq lsp-rust-clippy-preference "on")
;;   (use-package rust-playground
;;     :commands (rust-playground)
;;     :ensure t))


;;
;; 70 statistics and data science
;;

;;
;; 71 csv mode
;;

(use-package csv-mode
  :mode
  ("\\.csv\\'" . csv-mode)
  :init
  ;; font-lock makes large files very sluggish
  (add-hook 'csv-mode-hook (lambda () (font-lock-mode -1)))
  ;; only apply to large files
  ;; (when (> (point-max) some-large-number) (font-lock-mode -1))
  :config
  (setq csv-align-max-width 7)
  (setq csv-separators '("," "\t" ";"))
  (define-key csv-mode-map (kbd "C-c C-a") 'csv-align-mode))

;;
;; 72 quarto
;;

;; quarto are the next-gen data science markdowns
(use-package quarto-mode
  :mode
  (("\\.Rmd" . poly-quarto-mode))
  )

;;
;; 73 ESS
;;




;;
;; 74 Jupyter notebooks
;;

;; https://github.com/emacs-jupyter/jupyter


;;
;; 75 SQLite mode (Emacs 29+)
;;

;; https://github.com/xenodium/dotsies/blob/main/emacs/ar/sqlite-mode-extras.el


;;
;; 80 LSPs
;;

;; https://emacs-lsp.github.io/lsp-mode/
;; https://ianyepan.github.io/posts/emacs-ide/


;;
;; 90 ORG mode 
;;

;; getting started with org mode: https://www.youtube.com/watch?v=SzA2YODtgK4
;; Rainer König Org Mode Tutorial: https://www.youtube.com/playlist?list=PLVtKhBrRV_ZkPnBtt_TD1Cs9PJlU0IIdE


;; https://xenodium.com/emacs-dwim-do-what-i-mean/
;; (defun ar/org-insert-link-dwim ()
;;   "Like `org-insert-link' but with personal dwim preferences."
;;   (interactive)
;;   (let* ((point-in-link (org-in-regexp org-link-any-re 1))
;;          (clipboard-url (when (string-match-p "^http" (current-kill 0))
;;                           (current-kill 0)))
;;          (region-content (when (region-active-p)
;;                            (buffer-substring-no-properties (region-beginning)
;;                                                            (region-end)))))
;;     (cond ((and region-content clipboard-url (not point-in-link))
;;            (delete-region (region-beginning) (region-end))
;;            (insert (org-make-link-string clipboard-url region-content)))
;;           ((and clipboard-url (not point-in-link))
;;            (insert (org-make-link-string
;;                     clipboard-url
;;                     (read-string "title: "
;;                                  (with-current-buffer (url-retrieve-synchronously clipboard-url)
;;                                    (dom-text (car
;;                                               (dom-by-tag (libxml-parse-html-region
;;                                                            (point-min)
;;                                                            (point-max))
;;                                                           'title))))))))
;;           (t
;;            (call-interactively 'org-insert-link)))))


;; https://github.com/abo-abo/org-download
;; org-cite?
;; Org-roam "A plain-text personal knowledge management system."
;; https://github.com/kot-behemoth/awesome-org-roam
;; https://github.com/org-roam/org-roam-ui
;; https://orgmode.org/worg/org-contrib/babel/uses.html
;; https://github.com/wdavew/org-excalidraw/
;; https://systemcrafters.net/build-a-second-brain-in-emacs/getting-started-with-org-roam/ 
;; (use-package org-roam
;;   :ensure t
;;   :init
;;   (setq org-roam-v2-ack t)
;;   :custom
;;   (org-roam-directory "~/RoamNotes")
;;   (org-roam-completion-everywhere t)
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;;          ("C-c n f" . org-roam-node-find)
;;          ("C-c n i" . org-roam-node-insert)
;;          :map org-mode-map
;;          ("C-M-i"    . completion-at-point))
;;   :config
;;   (org-roam-setup))

;; https://github.com/Ethanlinyf/General-Pure-Emacs/blob/main/lisp/init-r-roam.el

;; https://www.badykov.com/emacs/be-productive-with-org-mode/
;; https://howardism.org/Technical/Emacs/orgmode-wordprocessor.html

;; https://lucidmanager.org/data-science/visualise-org-roam/


;;
;; 99 notes
;;

;; TODO emacs 29, tons of clean stuff here: https://github.com/victorolinasc/dot_emacs
;; C-h a ts-mode$ : find all available treesitter modes
;; also read the masting emacs article 

;; treesitter movement package: https://github.com/haritkapadia/ts-movement

;; https://github.com/gopar/.emacs.d 


;; elfeed: https://github.com/skeeto/.emacs.d/blob/449d4804dcd97109fb8bb720ebfd189280c11a70/etc/feed-setup.el 

;; https://protesilaos.com/emacs/dotemacs
;; ;;;###autoload
;; (defun prot-common-reverse-percentage (number percent change-p)
;;   "Determine the original value of NUMBER given PERCENT.

;; CHANGE-P should specify the increase or decrease.  For simplicity,
;; nil means decrease while non-nil stands for an increase.

;; NUMBER must satisfy `numberp', while PERCENT must be `natnump'."
;;   (unless (numberp number)
;;     (user-error "NUMBER must satisfy  numberp"))
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


;; ;; mac specific stuff
;; (setq mac-option-modifier 'meta)
;; (setq mac-command-modifier 'hyper)

;; bind space to quicklook in dired. 
;; (defun do-ql-dwim()
;;   (interactive)
;;   (let* ((proc (get-buffer-process "*Async Shell Command*")))
;;     (if proc
;; 	(kill-process proc)
;;       (dired-do-async-shell-command
;;        "qlmanage -p 2>/dev/null" ""
;;        (dired-get-marked-files))
;;       ))
;;   )
;; (add-hook 'dired-mode-hook (lambda ()
;; 			     (define-key dired-mode-map " " 'do-ql-dwim)
;; 			     ))


;; WSL specific stuff
;, https://emacsredux.com/blog/2021/12/19/using-emacs-on-windows-11-with-wsl2/
;; https://emacsredux.com/blog/2021/12/19/wsl-specific-emacs-configuration/

;; emacs 29 specific stuff 
;; (pixel-scroll-precision-mode)
;; unset a binding
;;(keymap-unset clojure-mode-map (kbd "C-c C-z"))
;; remove a binding
;;(keymap-unset clojure-mode-map (kbd "C-c C-z") 'remove)



;; (defun mac-switch-meta nil
;;   "switch meta between Option and Command"
;;   (interactive)
;;   (if (eq mac-option-modifier nil)
;;       (progn
;;         (setq mac-option-modifier 'meta
;;               mac-command-modifier 'hyper))
;;     (progn
;;       (setq mac-option-modifier nil
;;             mac-command-modifier 'meta))))


;; TODO Treesitter
;; https://youtu.be/MZPR_SC9LzE

;; (use-package treemacs-icons-dired
;;   :after treemacs dired
;;   :ensure t
;;   :config (treemacs-icons-dired-mode))

;; code-compass: a guide in your software development 
;; https://github.com/ag91/code-compass
;; https://emacsconf.org/2020/talks/24/
;; https://ag91.github.io/blog/2020/12/27/emacs-as-your-code-compass-how-complex-is-this-code/

;; emacs buddy
;; https://github.com/ag91/emacs-buddy

;; AI
;; https://github.com/xenodium/chatgpt-shell
;; https://github.com/tyler-dodge/org-assistant

;; prescient
;; https://github.com/radian-software/prescient.el

;; Brent Westwood (rust and data science emacs)
;; https://github.com/ntBre/yt-configs/blob/master/.emacs.d/init.el


;; ;; After publishing the above, I learnt about this variable, which has
;; ;; the same effect as `prot/bookmark-save-no-prompt':
;; (setq bookmark-save-flag 1)

;; (setq savehist-additional-variables '(register-alist kill-ring))

;; (savehist-mode 1)

;; ;; Make Emacs repeat the C-u C-SPC command (`set-mark-command') by
;; ;; following it up with another C-SPC.  It is faster to type
;; ;; C-u C-SPC, C-SPC, C-SPC, than C-u C-SPC, C-u C-SPC, C-u C-SPC...
;; (setq set-mark-command-repeat-pop t)

;; pdftools
;; https://xenodium.com/trying-out-emacs-pdf-tools/
;; https://github.com/vedang/pdf-tools


;; (defun split-para-at-sentence-ends ()
;;   "Split current paragraph into lines with one sentence each.
;; Then turn off `auto-fill-mode'."
;;   (interactive)
;;   (let ((mode  major-mode))
;;     (unwind-protect
;;          (progn (text-mode)
;;                 (save-excursion
;;                   (let ((emacs-lisp-docstring-fill-column  t)
;;                         (fill-column                       (point-max)))
;;                     (fill-paragraph))
;;                   (let ((bop  (copy-marker (progn (backward-paragraph) (point))))
;;                         (eop  (copy-marker (progn (forward-paragraph)  (point)))))
;;                     (goto-char bop)
;;                     (while (< (point) eop)
;;                       (forward-sentence)
;;                       (forward-whitespace 1)
;;                       (unless (>= (point) eop)
;;                         (delete-horizontal-space)
;;                         (insert "\n"))))))
;;       (funcall mode)))
;;   (auto-fill-mode -1))

;; (define-minor-mode split-para-mode 
;;     "Toggle between a filled paragraph and one split into sentences."
;;   nil nil nil
;;   (if (not split-para-mode)
;;       (split-para-at-sentence-ends)
;;     (auto-fill-mode 1)
;;     (fill-paragraph)))

;; (global-set-key "\C-o" 'split-para-mode) ; Or some other key.
