(require 'package) ;; You might already have this line
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t)
  (add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


;; Evil Mode
;;
;;
(require 'evil)
(evil-mode 1)

;; TODO check if this is any useful
;; (require 'evil-org)
;; (add-hook 'org-mode-hook 'evil-org-mode)
;; (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
;; (require 'evil-org-agenda)
;; (evil-org-agenda-set-keys)

;; evil ends here


;; helm

;; begin helm
(use-package helm
  :defer 2
  :diminish helm-mode
  :bind (("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-mini)
         ("M-y" . helm-show-kill-ring)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action) ; Rebind TAB to expand
         ("C-i" . helm-execute-persistent-action) ; Make TAB work in CLI
         ("C-z" . helm-select-action)) ; List actions using C-z
  :config
  (progn
    (setq helm-buffer-max-length nil) ;; Size according to longest buffer name

    (setq helm-split-window-in-side-p t)
    (helm-mode 1)))

(use-package helm-fuzzier
  :defer 2
  :config
  (progn
    (setq helm-mode-fuzzy-match t
          helm-M-x-fuzzy-match t
          helm-buffers-fuzzy-match t
          helm-recentf-fuzzy-match t)
    (helm-fuzzier-mode 1)))

(use-package helm-ag)
;; end helm

(global-set-key (kbd "C-x g") 'magit-status) ; "Most Magit commands are commonly invoked from the status buffer"

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-c L" 'org-insert-link-global)
(global-set-key "\C-c o" 'org-open-at-point-global)
(setq org-link-frame-setup '((file . find-file))) ; open link in same frame.
(if (boundp 'org-user-agenda-files)
  (setq org-agenda-files org-user-agenda-files)
  (setq org-agenda-files (quote ("~/projects/notes_privat")))
  )

;; begin golang
(progn
  ; godoc has no cli support any more, thats go doc now
  (setq godoc-and-godef-command "go doc")
  (add-to-list 'exec-path "~/go/bin")
  (add-hook 'go-mode-hook
              (lambda ()
                (set (make-local-variable 'company-backends) '(company-go))
                (company-mode t)
                (yas-minor-mode-on))
  (add-hook 'before-save-hook 'gofmt-before-save)))
;; end golang

;; begin rust
(setq racer-rust-src-path nil) ;; read from shell-nix
(setq racer-cmd "racer") ;; read from shell-nix
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook (lambda()
  (local-set-key (kbd "C-c C-d") 'racer-describe)
  (local-set-key (kbd "C-c .") 'racer-find-definition)
  (local-set-key (kbd "C-c ,") 'pop-tag-mark))
)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)
;;end rust

;; begin theming
(load-theme 'spacemacs-dark t)
(load-theme 'spacemacs-light t)
(disable-theme 'spacemacs-light)
(disable-theme 'spacemacs-dark)
(enable-theme 'spacemacs-dark)

(defun mh/spacemacs-dark ()
  (interactive)
  (disable-theme 'spacemacs-light)
  (enable-theme 'spacemacs-dark)
  )

(defun mh/spacemacs-light ()
  (interactive)
  (disable-theme 'spacemacs-dark)
  (enable-theme 'spacemacs-light)
  )

(global-set-key "\C-ctl" 'mh/spacemacs-light)
(global-set-key "\C-ctd" 'mh/spacemacs-dark)
;; end theming

;; begin emacs convenience
(menu-bar-mode -1)
(tool-bar-mode -1)                  ; Disable the button bar atop screen
(scroll-bar-mode -1)                ; Disable scroll bar
(toggle-scroll-bar -1)
(setq inhibit-startup-screen t)     ; Disable startup screen with graphics
(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs
(setq default-tab-width 2)          ; Two spaces is a tab
(setq tab-width 2)                  ; Four spaces is a tab
(setq visible-bell nil)             ; Disable annoying visual bell graphic
(setq ring-bell-function 'ignore)   ; Disable super annoying audio bell
;;end emacs convenience

;; begin on save hook
(defun mh/before-save()
  (delete-trailing-whitespace)
  )

(defun mh/after-save()
  "Reminds the user to to call magit-status

  magit-status to add, commit, push
  "
  (interactive)
  (if (y-or-n-p "Call magit-status (y/n)?)")
      (magit-status)
    )
  )

(add-hook 'before-save-hook 'mh/before-save)
(add-hook 'after-save-hook 'mh/after-save)
;; end save hook

(fset 'kill-actual-buffer
  [?\C-x ?k return])

(defun mh/open-term-and-rename (name)
  "open a new bash and rename it"
  (interactive "sName of new terminal: ")
  (term "/run/current-system/sw/bin/bash")
  (rename-buffer name)
)
(global-set-key (kbd "M-<f8>") 'kill-actual-buffer)

(global-set-key (kbd "<f5>") 'mh/open-term-and-rename)
(global-set-key (kbd "<f6>") 'other-window)
(global-set-key (kbd "<f7>") 'split-window-right)
(global-set-key (kbd "<f8>") 'delete-other-windows)

(require 'lsp-mode)
(add-hook 'rust-mode-hook #'lsp)

;;; emacs-convenience.el -- My emacs convenience settings
;;;
;;; global settings for all buffers

(progn
  (global-hl-line-mode)
  (global-linum-mode)
  (global-company-mode)
  (setq auto-save-default nil)
  )

;;; emacs-convenience.el ends here

;;; exwm.el -- My EXWM init file
;;; Commentary:
;;; Code:


(require 'exwm)
(require 'exwm-config)

;; Display time in modeline
(progn
  (require 'time)
  (setq display-time-24hr-format t)
  (display-time-mode 1))

;; Display battery mode
(progn
  (require 'battery)
  (display-battery-mode))

;; Define a function to easily run commands
(progn
  (defun exwm-run-systemd (command)
    (interactive (list (read-shell-command "$ ")))
    (let ((cmd (concat "/run/current-system/sw/bin/systemd-run --user " command)))
      (start-process-shell-command cmd nil cmd)))
  (exwm-input-set-key (kbd "s-SPC") 'exwm-run-systemd)
  (exwm-input-set-key (kbd "s-e") 'exwm-run-systemd)

  (defun exwm-run (command)
    "Run COMMAND."
    (interactive (list (read-shell-command "> ")))
    (start-process-shell-command command nil command))
  (exwm-input-set-key (kbd "C-s-SPC") 'exwm-run)

  ;; Special function to run the terminal
  (defun exwm-run-terminal ()
    (interactive)
    (exwm-run-systemd "@xterm@/bin/xterm"))
  (exwm-input-set-key (kbd "s-t") 'exwm-run-terminal))

;; Define desktop environment commands
(progn
  (require 'desktop-environment)
  (setq desktop-environment-screenlock-command "@lockCommand@")
  (setq desktop-environment-screenshot-directory "~"
        desktop-environment-screenshot-command "@flameshot@/bin/flameshot gui"
        desktop-environment-screenshot-partial-command "@flameshot@/bin/flameshot gui")
  (setq desktop-environment-brightness-get-command "@xbacklight@/bin/xbacklight"
        desktop-environment-brightness-set-command "@xbacklight@/bin/xbacklight %s"
        desktop-environment-brightness-get-regexp "\\([0-9]+\\)"
        desktop-environment-brightness-normal-increment "-inc 10"
        desktop-environment-brightness-normal-decrement "-dec 10"
        desktop-environment-brightness-small-increment "-inc 5"
        desktop-environment-brightness-small-decrement "-dec 5")
  (desktop-environment-mode))

;; Set up systray
(progn
  (require 'exwm-systemtray)
  (exwm-systemtray-enable))

;; Set up randr support
(progn
  (require 'exwm-randr)
  (setq exwm-randr-workspace-monitor-plist '(1 "eDP1" 2 "DP-2-2-8" 9 "DP-2-3"))
  (exwm-randr-enable))

(progn
  ;; Bind switch to workspace commands
  (dotimes (i 10)
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))

  ;; Make it possible to do exwm-reset
  (exwm-input-set-key (kbd "s-r") 'exwm-input-grab-keyboard)

  ; test
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t)

  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  (add-hook 'exwm-update-title-hook
            (lambda ()
              (let ((tilde-exwm-title
                     (replace-regexp-in-string (getenv "HOME") "~" exwm-title)))
                (exwm-workspace-rename-buffer (format "%s: %s" exwm-class-name tilde-exwm-title)))))

  (exwm-enable)
  (exwm-init))

;;; exwm.el ends here
