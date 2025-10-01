;;; example-config.el --- Example configuration for increa.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Example Emacs configuration for increa.el

;;; Code:

;; Add to load-path
(add-to-list 'load-path "/path/to/increa.el")

;; Load increa
(require 'increa)

;; ========== Qwen Configuration ==========
;; Get your API key from: https://help.aliyun.com/zh/model-studio/get-api-key

(setq increa-api-provider 'qwen)
(setq increa-api-key "YOUR_DASHSCOPE_API_KEY_HERE")

;; Use Qwen-Coder Flash model (faster, recommended)
(setq increa-model "qwen3-coder-flash")

;; Or use Qwen-Coder Plus (more powerful)
;; (setq increa-model "qwen3-coder-plus")

;; API endpoint (Beijing region by default)
(setq increa-api-endpoint "https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions")

;; For Singapore region, use:
;; (setq increa-api-endpoint "https://dashscope-intl.aliyuncs.com/compatible-mode/v1/chat/completions")

;; ========== Completion Settings ==========

;; Delay before triggering auto-completion (seconds)
(setq increa-idle-delay 0.5)

;; Context size
(setq increa-context-lines-before 30)
(setq increa-context-lines-after 10)

;; Model parameters
(setq increa-max-tokens 500)
(setq increa-temperature 0.2)

;; ========== Enable Increa ==========

;; Enable globally
(global-increa-mode 1)

;; Or enable only for specific modes
;; (add-hook 'python-mode-hook #'increa-mode)
;; (add-hook 'emacs-lisp-mode-hook #'increa-mode)
;; (add-hook 'js-mode-hook #'increa-mode)

;; ========== Keybindings ==========

;; Manual trigger: C-c i (already bound by default)
;; Accept completion: TAB (when completion is visible)
;; Dismiss completion: C-g

;; Custom keybindings (optional)
;; (with-eval-after-load 'increa
;;   (define-key increa-mode-map (kbd "M-<tab>") #'increa-complete))

;; ========== Visual Settings ==========

;; Customize completion face (optional)
;; (custom-set-faces
;;  '(increa-overlay-face ((t (:foreground "#888888" :italic t)))))

;;; example-config.el ends here