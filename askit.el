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
(require 'askit-org)
(require 'askit-blog)
(require 'askit-prog)

;; add kbd map
;; add-hook for modes org, prog
(provide 'askit)
;;; askit.el ends here
