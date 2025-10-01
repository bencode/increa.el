;;; increa-api.el --- LLM API integration for increa -*- lexical-binding: t; -*-

;;; Commentary:
;; API integration for Qwen-Coder and Claude

;;; Code:

(require 'json)
(require 'request)

(defcustom increa-api-provider 'qwen
  "LLM API provider."
  :type '(choice (const :tag "Qwen-Coder" qwen)
                 (const :tag "Claude" claude))
  :group 'increa)

(defcustom increa-api-key ""
  "API key for the LLM provider."
  :type 'string
  :group 'increa)

(defcustom increa-api-endpoint "https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions"
  "Qwen API endpoint."
  :type 'string
  :group 'increa)

(defcustom increa-model "qwen3-coder-flash"
  "Model name to use."
  :type 'string
  :group 'increa)

(defcustom increa-api-timeout 10
  "API request timeout in seconds."
  :type 'integer
  :group 'increa)

(defcustom increa-max-tokens 500
  "Maximum tokens for completion."
  :type 'integer
  :group 'increa)

(defcustom increa-temperature 0.2
  "Temperature for generation (0.0-1.0)."
  :type 'float
  :group 'increa)

(defun increa--call-api (prompt callback &optional error-callback)
  "Call LLM API with PROMPT and invoke CALLBACK with result.
ERROR-CALLBACK is called on error with error message."
  (let ((request-data `((model . ,increa-model)
                        (max_tokens . ,increa-max-tokens)
                        (temperature . ,increa-temperature)
                        (messages . [((role . "system")
                                     (content . "You are a code completion assistant. Only output code, no explanations."))
                                    ((role . "user")
                                     (content . ,prompt))]))))
    (request increa-api-endpoint
      :type "POST"
      :headers `(("Content-Type" . "application/json")
                 ("Authorization" . ,(format "Bearer %s" increa-api-key)))
      :data (json-encode request-data)
      :parser 'json-read
      :encoding 'utf-8
      :timeout increa-api-timeout
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let* ((choices (alist-get 'choices data))
                         (message (alist-get 'message (aref choices 0)))
                         (content (alist-get 'content message)))
                    (when content
                      (funcall callback content)))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (when error-callback
                  (funcall error-callback (format "Request error: %s" error-thrown))))))))

(provide 'increa-api)
;;; increa-api.el ends here