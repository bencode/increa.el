;;; increa-context.el --- Context extraction -*- lexical-binding: t; -*-

(defcustom increa-context-max-chars 100000
  "Maximum characters to send to LLM, -1 means no limit."
  :type 'integer
  :group 'increa)

(defun increa--get-language-id ()
  "Get language identifier for current buffer."
  (let ((mode-name (symbol-name major-mode)))
    (replace-regexp-in-string "-mode$" "" mode-name)))

(defun increa--get-context ()
  "Extract code context around cursor.
Returns plist with :prefix, :suffix, :language, :file-path."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((pos (point))
             (pmax (point-max))
             (pmin (point-min))
             (half-window (/ increa-context-max-chars 2)))
        (cond
         ;; 文件小于限制，使用整个文件
         ((or (< increa-context-max-chars 0) (< pmax increa-context-max-chars))
          (let ((prefix (buffer-substring-no-properties pmin pos))
                (suffix (buffer-substring-no-properties pos pmax)))
            (list :prefix prefix :suffix suffix
                  :language (increa--get-language-id)
                  :file-path (or (buffer-file-name) (buffer-name)))))
         ;; 光标靠近文件尾，截断文件头
         ((< (- pmax pos) half-window)
          (let* ((start (max pmin (- pmax increa-context-max-chars)))
                 (prefix (buffer-substring-no-properties start pos))
                 (suffix (buffer-substring-no-properties pos pmax)))
            (list :prefix prefix :suffix suffix
                  :language (increa--get-language-id)
                  :file-path (or (buffer-file-name) (buffer-name)))))
         ;; 光标靠近文件头，截断文件尾
         ((< (- pos pmin) half-window)
          (let* ((end (min pmax (+ pmin increa-context-max-chars)))
                 (prefix (buffer-substring-no-properties pmin pos))
                 (suffix (buffer-substring-no-properties pos end)))
            (list :prefix prefix :suffix suffix
                  :language (increa--get-language-id)
                  :file-path (or (buffer-file-name) (buffer-name)))))
         ;; 光标在中间，前后各取 half-window
         (t
          (let* ((start (max pmin (- pos half-window)))
                 (end (min pmax (+ pos half-window)))
                 (prefix (buffer-substring-no-properties start pos))
                 (suffix (buffer-substring-no-properties pos end)))
            (list :prefix prefix :suffix suffix
                  :language (increa--get-language-id)
                  :file-path (or (buffer-file-name) (buffer-name))))))))))

(defun increa--build-prompt (context)
  "Build completion prompt from CONTEXT.
CONTEXT is a plist from `increa--get-context'."
  (let ((prefix (plist-get context :prefix))
        (suffix (plist-get context :suffix))
        (language (plist-get context :language)))
    (format "Complete the code at <cursor>. Output ONLY the completion code, no explanations, no markdown.

Language: %s

```%s
%s<cursor>%s
```

Rules:
- Only output the completion starting from <cursor>
- Do NOT repeat prefix or suffix
- Keep it concise (1-3 lines preferred)
- Match existing code style
- No explanations, no markdown fences
- Pay attention to comments near <cursor> that start with 'ai:' or 'ai?' - these provide hints about what to generate

Completion:"
            language
            language
            prefix
            suffix)))

(defun increa--trim-completion (text)
  "Clean up completion TEXT from LLM response."
  (let ((cleaned text))
    ;; 移除 markdown 代码块标记
    (setq cleaned (replace-regexp-in-string "^```[a-z]*\n?" "" cleaned))
    (setq cleaned (replace-regexp-in-string "\n?```$" "" cleaned))
    ;; 移除开头和结尾的空行
    (setq cleaned (replace-regexp-in-string "\\`\n+" "" cleaned))
    (setq cleaned (replace-regexp-in-string "\n+\\'" "" cleaned))
    ;; 返回全部内容（多行）
    cleaned))

(provide 'increa-context)
