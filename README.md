# increa.el

Intelligent code completion for Emacs using LLM APIs. Provides GitHub Copilot-like ghost text completions.

Supports OpenAI-compatible APIs.

## Installation

### Doom Emacs

Add to `packages.el`:

```elisp
(package! increa :recipe (:host github :repo "bencode/increa.el"))
```

Add to `config.el`:

```elisp
(use-package! increa
  :config
  (setq increa-api-endpoint "https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions"
        increa-api-key (getenv "QWEN_API_KEY")  ; or hardcode your key
        increa-model "qwen3-coder-flash")
  (global-increa-mode 1))
```

Then run `doom sync`.

### Straight.el

```elisp
(straight-use-package
 '(increa :type git :host github :repo "bencode/increa.el"))

(require 'increa)
(setq increa-api-key "YOUR_API_KEY"
      increa-model "qwen3-coder-flash")
(global-increa-mode 1)
```

### Manual Installation

1. Install dependency:

```elisp
M-x package-install RET request RET
```

2. Clone and load:

```bash
git clone https://github.com/bencode/increa.el.git ~/.emacs.d/increa.el
```

```elisp
(add-to-list 'load-path "~/.emacs.d/increa.el")
(require 'increa)
```

## Configuration

### Qwen-Coder

Tested and recommended. Get API key: https://help.aliyun.com/zh/model-studio/get-api-key

```elisp
(setq increa-api-endpoint "https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions")
(setq increa-api-key "YOUR_API_KEY")
(setq increa-model "qwen3-coder-flash")
(global-increa-mode 1)
```

**Available models via Alibaba endpoint:**
- `qwen3-coder-flash` - Fast, recommended for code completion
- `qwen3-coder-plus` - More powerful but slower
- `glm-4.5-air` - Zhipu AI's GLM model

### Moonshot (Kimi)

Tested. Get API key: https://platform.moonshot.cn/console/api-keys

```elisp
(setq increa-api-endpoint "https://api.moonshot.cn/v1/chat/completions")
(setq increa-api-key "YOUR_API_KEY")
(setq increa-model "kimi-k2-0905-preview")
(global-increa-mode 1)
```

### OpenAI

Tested. Get API key: https://platform.openai.com/api-keys

**Note:** May require HTTP proxy configuration in China.

```elisp
(setq increa-api-endpoint "https://api.openai.com/v1/chat/completions")
(setq increa-api-key "YOUR_OPENAI_KEY")
(setq increa-model "gpt-4o-mini")
(global-increa-mode 1)
```

### Other OpenAI-compatible APIs

Any OpenAI-compatible endpoint should work. Just set the three parameters: `increa-api-endpoint`, `increa-api-key`, `increa-model`.

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

## License

MIT
