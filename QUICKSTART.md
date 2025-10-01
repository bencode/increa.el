# Quick Start Guide

## Installation

### 1. Install request.el

```elisp
;; Using package.el
M-x package-install RET request RET

;; Or in Doom Emacs, add to packages.el:
(package! request)
;; Then run: doom sync
```

### 2. Install increa.el

Clone the repository:
```bash
git clone https://github.com/bencode/increa.el.git ~/.emacs.d/increa.el
```

Add to your Emacs config:
```elisp
(add-to-list 'load-path "~/.emacs.d/increa.el")
(require 'increa)
```

## Configuration

### Get API Key

Visit: https://help.aliyun.com/zh/model-studio/get-api-key

### Basic Setup

```elisp
;; Set provider and API key
(setq increa-api-provider 'qwen)
(setq increa-api-key "YOUR_DASHSCOPE_API_KEY")

;; Choose model (flash is faster, plus is more powerful)
(setq increa-model "qwen3-coder-flash")

;; Enable globally
(global-increa-mode 1)
```

### Region-specific Endpoints

**Beijing (default):**
```elisp
(setq increa-api-endpoint "https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions")
```

**Singapore:**
```elisp
(setq increa-api-endpoint "https://dashscope-intl.aliyuncs.com/compatible-mode/v1/chat/completions")
```

## Usage

### Automatic Completion
- Type code normally
- Wait 0.5 seconds (configurable)
- Gray completion text appears
- Press `TAB` or `C-<return>` to accept

### Manual Trigger
- `C-c i` - Trigger completion manually
- `TAB` - Accept completion
- `C-<return>` - Accept completion (Ctrl+Enter)
- `C-g` - Dismiss completion

## Testing

Test in a Python buffer:

```python
def fibonacci(n):
    # Type this and wait for completion
```

Expected completion might be:
```python
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)
```

## Troubleshooting

### No completion appears
1. Check API key: `M-: increa-api-key RET`
2. Check mode: `M-x increa-mode` should show "enabled"
3. Check messages buffer: `*Messages*` for errors

### API errors
- Verify API key is valid
- Check network connectivity
- Verify endpoint URL matches your region

### Completion too slow
- Adjust context size:
  ```elisp
  (setq increa-context-max-chars 50000)  ; Default is 100000
  ```
- Use faster model: `qwen3-coder-flash`

## Advanced Configuration

See `example-config.el` for complete configuration options.

## Next Steps

- Report issues on GitHub
- Contribute improvements