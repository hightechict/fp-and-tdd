;; show linenumber in left column
(require 'linum)
(global-linum-mode 1)

;; various settings
(set-language-environment "UTF-8")
(set-face-attribute 'default nil :font "DejaVu Sans Mono-10")
(server-start)
(delete-selection-mode 1)
(add-hook 'text-mode-hook 'turn-on-auto-fill)  ; auto-formatting in text-mode
(blink-cursor-mode nil)
(column-number-mode t)                         ; Show column number in mode-lin
(defalias 'yes-or-no-p 'y-or-n-p)              ; y/n instead of yes/no
(global-font-lock-mode 1)                      ; Color enabled
(icomplete-mode t)                             ; Completion in mini-buffer
(line-number-mode 1)
(setq auto-save-timeout 60)                    ; Autosave every minute
(setq calendar-week-start-day 1)               ; Week starts monday
(setq default-major-mode 'text-mode)           ; Text-mode is default mode
(setq european-calendar-style 't)              ; European style calendar
(setq format-time-string "%d-%m-%Y %H:%M:%S%z")
(setq frame-title-format "%b - emacs")         ; Use buffer name as frame title
(setq inhibit-startup-message t)               ; No message at startup
(setq ispell-dictionary "english")             ; Set ispell dictionary
(setq make-backup-files nil)                   ; No backup files ~
(setq mouse-yank-at-point t)                   ; Paste at cursor position
(setq next-line-add-newlines t)                ; Add newline when at buffer end
(setq ps-paper-type 'a4)                       ; Specify printing format
(setq require-final-newline 't)                ; Always newline at end of file
(setq sentence-end-double-space nil)           ; Sentences end with one space
(setq suggest-key-bindings t)                  ; Hints for M-x
(setq tab-width 2)                             ; Length of tab is 4 SPC
(setq track-eol nil)                           ; Cursor don't track end-of-line
(setq truncate-partial-width-windows nil)      ; Don't truncate long lines
(setq undo-limit 100000)                       ; Increase number of undo
(setq visible-bell t)                          ; No beep when reporting errors
(setq-default indent-tabs-mode nil)            ; Use spaces instead of tabs
(setq indent-tabs-mode nil)
(setq-default indicate-empty-lines t)          ; Show empty lines
(show-paren-mode t)
(transient-mark-mode t)
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
