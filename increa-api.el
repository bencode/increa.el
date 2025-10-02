;;; increa-api.el --- API communication layer -*- lexical-binding: t; -*-

(require 'json)
(require 'request)

(defcustom increa-api-key ""
  "API key for the LLM provider."
  :type 'string
  :group 'increa)

(defcustom increa-api-endpoint "https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions"
  "API endpoint URL (OpenAI-compatible format).
Tested endpoints:
- Alibaba (Qwen/GLM): https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions
- Moonshot (Kimi): https://api.moonshot.cn/v1/chat/completions
- OpenAI: https://api.openai.com/v1/chat/completions (may require proxy)"
  :type 'string
  :group 'increa)

(defcustom increa-model "qwen3-coder-flash"
  "Model name to use.
Tested models:
- qwen3-coder-flash (Alibaba, recommended)
- qwen3-coder-plus (Alibaba)
- glm-4.5-air (Alibaba)
- kimi-k2-0905-preview (Moonshot)
- gpt-4o-mini (OpenAI)
- gpt-4o (OpenAI)"
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

(defcustom increa-debug nil
  "Enable debug logging for API requests."
  :type 'boolean
  :group 'increa)

(defun increa--call-api (prompt callback &optional error-callback)
  "Call LLM API with PROMPT and invoke CALLBACK with result.
ERROR-CALLBACK is called on error with error message.
Returns the request object for cancellation."
  (let ((request-data `((model . ,increa-model)
                        (max_tokens . ,increa-max-tokens)
                        (temperature . ,increa-temperature)
                        (messages . [((role . "system")
                                     (content . "You are a code completion assistant. Only output code, no explanations."))
                                    ((role . "user")
                                     (content . ,prompt))]))))
    (when increa-debug
      (message "[Increa] Request data: %s" (json-encode request-data)))
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
                    (when increa-debug
                      (message "[Increa] Response data: %s" (json-encode data)))
                    (when content
                      (funcall callback content)))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (when error-callback
                  (funcall error-callback (format "Request error: %s" error-thrown))))))))

(provide 'increa-api)
