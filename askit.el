;;; askit.el --- Claude ai wrapper    -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jerome Riga

;; Author:     Jerome Riga <jriga@zemis.co.uk>
;; Keywords:   lisp, ai, claude, anthropic
;; Version:    0.0.1
;; Homepage:   https://github.com/jriga/askit
;; Maintainer: Jerome Riga <jriga@zemis.co.uk>
;; Package-Requires: ((emacs "29.4"))

;;; Commentary:

;; Interacts with claudeAI, support tools
;; no streaming at the moment

;;; Code:

(require 'askit-core)

(defun askit-setup-org-map ()
  "Set up askit org keyboard map."
  (define-prefix-command 'askit-map)
  (local-set-key (kbd "C-c p") 'askit-map)
  (define-key askit-map (kbd "p") 'askit-prompt-at-point)
  (define-key askit-map (kbd "d") 'askit-dump-client)
  (define-key askit-map (kbd "r") 'askit-restore-client))

(add-hook 'org-mode-hook
          (lambda ()
            (progn
              (require 'askit-org)
              (askit-setup-org-map))))

(defun askit-setup-prog-map ()
  "Set up askit prog keyboard map."
  (define-prefix-command 'askit-map)
  (local-set-key (kbd "C-c p") 'askit-map)
  (define-key askit-map (kbd "b") 'askit-code-review)
  (define-key askit-map (kbd "r") 'askit-code-review-region))

(add-hook 'prog-mode-hook
          (lambda ()
            (progn
              (require 'askit-prog)
              (askit-setup-prog-map))))


(provide 'askit)
;;; askit.el ends here
