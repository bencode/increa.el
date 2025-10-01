;;; increa.el --- Intelligent completion with LLM -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: qijun
;; URL: https://github.com/bencode/increa.el
;; Package-Requires: ((emacs "27.1") (request "0.3.0"))
;; Version: 0.1.0
;; Keywords: convenience completion

;; MIT License

;;; Commentary:
;; Intelligent code completion using LLM APIs (Qwen-Coder, Claude)
;; Provides Copilot-like ghost text completion experience

;;; Code:

(require 'increa-api)
(require 'increa-overlay)
(require 'increa-context)

(defgroup increa nil
  "Intelligent completion with LLM."
  :group 'completion
  :prefix "increa-")

(defcustom increa-idle-delay 0.5
  "Delay in seconds before triggering completion."
  :type 'number
  :group 'increa)

(defcustom increa-enable-auto-complete t
  "Enable automatic completion on idle."
  :type 'boolean
  :group 'increa)

(defvar increa--post-command-timer nil
  "Timer for delayed completion trigger.")

(defvar-local increa--last-trigger-point nil
  "Last position where completion was triggered.")

(defvar-local increa--completion-in-progress nil
  "Non-nil when API request is in progress.")

(defun increa-complete ()
  "Trigger code completion at current point."
  (interactive)
  (when (and (not (increa--overlay-visible-p))
             (not increa--completion-in-progress)
             (not (minibufferp)))
    (setq increa--last-trigger-point (point))
    (setq increa--completion-in-progress t)

    (let* ((context (increa--get-context))
           (prompt (increa--build-prompt context)))
      (message "Increa: requesting completion...")

      (increa--call-api
       prompt
       (lambda (response)
         (setq increa--completion-in-progress nil)
         (when (and (eq (current-buffer) (window-buffer))
                    (= (point) increa--last-trigger-point)
                    increa-mode)
           (let ((completion (increa--trim-completion response)))
             (when (and completion
                        (not (string-empty-p completion))
                        (not (string-blank-p completion)))
               (increa--set-overlay-text completion)))))
       (lambda (err)
         (setq increa--completion-in-progress nil)
         (message "Increa error: %s" err))))))

(defun increa--should-trigger-p ()
  "Check if completion should be triggered."
  (and increa-enable-auto-complete
       (not (minibufferp))
       (not buffer-read-only)
       (not (increa--overlay-visible-p))
       (not increa--completion-in-progress)
       (> (point) (point-min))
       (not (nth 3 (syntax-ppss)))
       (not (nth 4 (syntax-ppss)))))

(defun increa--post-command ()
  "Handle post-command for completion triggering."
  (unless (and (symbolp this-command)
               (string-prefix-p "increa-" (symbol-name this-command)))
    (increa-clear-overlay))

  (when increa--post-command-timer
    (cancel-timer increa--post-command-timer)
    (setq increa--post-command-timer nil))

  (when (and (numberp increa-idle-delay)
             (> increa-idle-delay 0)
             (increa--should-trigger-p))
    (setq increa--post-command-timer
          (run-with-idle-timer
           increa-idle-delay
           nil
           #'increa--trigger-if-idle
           (current-buffer)
           (point)))))

(defun increa--trigger-if-idle (buffer pos)
  "Trigger completion in BUFFER at POS if still idle."
  (when (and (buffer-live-p buffer)
             (eq buffer (current-buffer))
             (= pos (point))
             increa-mode
             (increa--should-trigger-p))
    (increa-complete)))

(defvar increa-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c i") #'increa-complete)
    (define-key map (kbd "C-c C-i") #'increa-complete)
    (define-key map (kbd "C-<return>") #'increa-accept-completion)
    map)
  "Keymap for `increa-mode'.")

;;;###autoload
(define-minor-mode increa-mode
  "Minor mode for intelligent code completion with LLM."
  :init-value nil
  :lighter " Increa"
  :keymap increa-mode-map
  :group 'increa
  (if increa-mode
      (progn
        (add-hook 'post-command-hook #'increa--post-command nil t)
        (message "Increa mode enabled"))
    (progn
      (remove-hook 'post-command-hook #'increa--post-command t)
      (when increa--post-command-timer
        (cancel-timer increa--post-command-timer)
        (setq increa--post-command-timer nil))
      (increa-clear-overlay)
      (message "Increa mode disabled"))))

;;;###autoload
(define-globalized-minor-mode global-increa-mode
  increa-mode
  (lambda ()
    (unless (or buffer-read-only
                (minibufferp))
      (increa-mode 1)))
  :group 'increa)

(provide 'increa)
;;; increa.el ends here
