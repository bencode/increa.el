# increa.el

Intelligent code completion for Emacs using LLM APIs. Provides GitHub Copilot-like ghost text completions.

Supports OpenAI-compatible APIs.

## Installation

### 1. Install request.el

```elisp
M-x package-install RET request RET

;; Or in Doom Emacs (packages.el):
(package! request)
```

### 2. Install increa.el

```bash
git clone https://github.com/bencode/increa.el.git ~/.emacs.d/increa.el
```


```elisp
(add-to-list 'load-path "~/.emacs.d/increa.el")
(require 'increa)
```

## Configuration

### Basic Setup

Any OpenAI-compatible API should works. Example with Qwen-Coder:

Get API key: https://help.aliyun.com/zh/model-studio/get-api-key

```elisp
(setq increa-api-endpoint "https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions")
(setq increa-api-key "YOUR_API_KEY")
(setq increa-model "qwen3-coder-flash")
(global-increa-mode 1)
```

### Advanced Options

```elisp

;; Control which modes to enable completion
(setq increa-enabled-modes '(prog-mode text-mode))  ; default
;; Or specific modes:
;; (setq increa-enabled-modes '(python-mode js-mode emacs-lisp-mode))
;; Or all modes:
;; (setq increa-enabled-modes nil)

;; Behavior
(setq increa-idle-delay 0.5)            ; trigger delay
(setq increa-context-max-chars 100000)  ; context size
(setq increa-max-tokens 500)            ; completion length
(setq increa-temperature 0.2)           ; randomness

;; Enable for specific modes only (alternative to increa-enabled-modes)
(add-hook 'python-mode-hook #'increa-mode)
(add-hook 'js-mode-hook #'increa-mode)
```

## Usage

- Type code, wait 0.5s for gray ghost text
- `C-<return>` (Ctrl+Enter) to accept
- `C-g` to dismiss
- `C-c i` to trigger manually

### Providing Hints to AI

Use `ai:` or `ai?` comments near the cursor to give hints:

```python
def process_data(items):
    # ai: filter out None values and sort by price
```

The AI will see these hints and generate code accordingly.

## Troubleshooting

**No completion:**
- Check API key: `M-: increa-api-key`
- Check mode: `M-x increa-mode` (should show enabled)
- Check `*Messages*` buffer for errors

**Too slow:**
```elisp
(setq increa-context-max-chars 50000)  ; reduce context
(setq increa-model "qwen3-coder-flash") ; use faster model
```

## License

MIT
