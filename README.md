# increa.el

An intelligent code completion plugin for Emacs, providing a GitHub Copilot-like experience using Qwen-Coder or Claude API.

## Features

- 🎯 **Ghost Text Completion**: Copilot-style overlay display
- ⚡ **Async Requests**: Non-blocking experience
- 🔌 **Multiple Backends**: Qwen-Coder / Claude API
- 🎨 **Lightweight**: No LSP server required
- ⌨️  **Smooth Interaction**: Tab to accept completion

## Requirements

- Emacs 27.1 or later
- [request.el](https://github.com/tkf/emacs-request) for HTTP requests

## Quick Start

### Installation

Install `request.el` first:
```elisp
;; Using package.el
(package-install 'request)

;; Or in Doom Emacs, add to packages.el:
;; (package! request)
```

Then install increa.el:
```elisp
(add-to-list 'load-path "/path/to/increa.el")
(require 'increa)
```

### Configuration

```elisp
;; Qwen-Coder (recommended)
(setq increa-api-provider 'qwen)
(setq increa-api-key "YOUR_QWEN_API_KEY")
(setq increa-model "qwen3-coder-flash")  ; or "qwen3-coder-plus"

;; Or Claude
;; (setq increa-api-provider 'claude)
;; (setq increa-api-key "YOUR_CLAUDE_API_KEY")

;; Enable
(global-increa-mode 1)
```

### Usage

- Type code, wait for gray completion hint
- `TAB` to accept
- `C-<return>` to accept (Ctrl+Enter)
- `C-g` to dismiss
- `C-c i` to trigger manually

## Documentation

- [Quick Start Guide](./QUICKSTART.md)
- [Example Configuration](./example-config.el)

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests on [GitHub](https://github.com/bencode/increa.el).

## License

MIT
