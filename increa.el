;;; increa.el --- Intelligent completion with LLM -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: bencode
;; URL: https://github.com/bencode/increa.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (request "0.3.0"))
;; Keywords: completion, ai, llm

;;; Commentary:

;; increa.el provides GitHub Copilot-like code completion using LLM APIs
;; (Qwen-Coder or Claude). It displays ghost text completions asynchronously.
;;
;; Basic usage:
;;
;;   (require 'increa)
;;   (setq increa-api-endpoint "https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions")
;;   (setq increa-api-key "your-api-key")
;;   (setq increa-model "qwen3-coder-flash")
;;   (global-increa-mode 1)
;;
;; For Doom Emacs, add to packages.el:
;;
;;   (package! increa :recipe (:host github :repo "bencode/increa.el"))
;;
;; Then in config.el:
;;
;;   (use-package! increa
;;     :config
;;     (setq increa-api-endpoint "https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions"
;;           increa-api-key (getenv "QWEN_API_KEY")
;;           increa-model "qwen3-coder-flash")
;;     (global-increa-mode 1))

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

(defcustom increa-enabled-modes
  '(prog-mode text-mode)
  "List of major modes where completion is enabled.
Can be specific modes (e.g., python-mode, js-mode) or parent modes (e.g., prog-mode).
Set to nil to enable in all modes."
  :type '(repeat symbol)
  :group 'increa)

(defvar increa--post-command-timer nil
  "Timer for delayed completion trigger.")

(defvar-local increa--last-trigger-point nil
  "Last position where completion was triggered.")

(defvar-local increa--completion-in-progress nil
  "Non-nil when API request is in progress.")

(defvar-local increa--current-request nil
  "Current API request object for cancellation.")

(defun increa-complete ()
  "Trigger code completion at current point."
  (interactive)
  (when (and (not (increa--overlay-visible-p))
             (not increa--completion-in-progress)
             (not (minibufferp)))
    (when increa--current-request
      (request-abort increa--current-request)
      (setq increa--current-request nil))

    (setq increa--last-trigger-point (point))
    (setq increa--completion-in-progress t)

    (let* ((context (increa--get-context))
           (prompt (increa--build-prompt context))
           (trigger-point increa--last-trigger-point)
           (trigger-buffer (current-buffer)))
      (message "Increa: requesting completion...")

      (setq increa--current-request
            (increa--call-api
             prompt
             (lambda (response)
               (setq increa--completion-in-progress nil)
               (setq increa--current-request nil)
               (when (and (buffer-live-p trigger-buffer)
                          (eq trigger-buffer (current-buffer))
                          (= (point) trigger-point)
                          increa-mode)
                 (let ((completion (increa--trim-completion response)))
                   (when (and completion
                              (not (string-empty-p completion))
                              (not (string-blank-p completion)))
                     (increa--set-overlay-text completion)))))
             (lambda (err)
               (setq increa--completion-in-progress nil)
               (setq increa--current-request nil)
               (message "Increa error: %s" err)))))))

(defun increa--mode-enabled-p ()
  "Check if current major mode is enabled for completion."
  (or (null increa-enabled-modes)
      (apply #'derived-mode-p increa-enabled-modes)))

(defun increa--after-whitespace-p ()
  "Check if there's at least one whitespace character after point."
  (or (eolp)
      (looking-at-p "[[:space:]]")))

(defun increa--should-trigger-p ()
  "Check if completion should be triggered."
  (and increa-enable-auto-complete
       (increa--mode-enabled-p)
       (increa--after-whitespace-p)
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
